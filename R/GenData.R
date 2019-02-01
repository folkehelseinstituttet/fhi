#' Creates norwayLocations
#' @importFrom readxl read_excel
#' @export GenNorwayLocations
GenNorwayLocations <- function() {
  norwayLocations <- readxl::read_excel(system.file("extdata", "norwayLocations.xlsx", package = "fhi"))
  norwayLocations <- norwayLocations[is.na(norwayLocations$yearEnd), c("municip", "municipName", "county", "countyName")]
  if (dir.exists(file.path("inst", "createddata"))) {
    try(saveRDS(norwayLocations, file.path("inst", "createddata", "norwayLocations.RDS")), TRUE)
  }
  return(invisible(norwayLocations))
}

#' Fetches NorwayLocations
#' @export NorwayLocations
NorwayLocations <- function() {
  if (is.null(vars$norwayLocations)) {
    VARS$norwayLocations <- readRDS(system.file("createddata", "norwayLocations.RDS", package = "fhi"))
  }
  return(VARS$norwayLocations)
}

#' Creates the Norway Municip Merging (kommunesammenslaping) data.table
#'
#' Last updated 2017-07-29
#'
#' @import data.table
#' @importFrom zoo na.locf
#' @export GenNorwayMunicipMerging
GenNorwayMunicipMerging <- function() {
  # variables used in data.table functions in this function
  yearStart <- NULL
  municip <- NULL
  yearEnd <- NULL
  municipName <- NULL
  municipEnd <- NULL
  county <- NULL
  countyName <- NULL
  region <- NULL
  regionName <- NULL
  realEnd <- NULL
  # end

  masterData <- data.table(readxl::read_excel(system.file("extdata", "norwayLocations.xlsx", package = "fhi")))
  maxYear <- max(data.table::year(lubridate::today()), max(masterData$yearStart, na.rm = T)) + 2

  masterData[yearStart <= 2006, yearStart := 2006]
  setnames(masterData, "yearStart", "year")
  skeleton <- expand.grid(year = as.numeric(2006:maxYear), municip = unique(masterData$municip), stringsAsFactors = FALSE)
  skeleton <- data.table(merge(skeleton, masterData, by = c("municip", "year"), all.x = T))
  setorder(skeleton, municip, year)
  skeleton[is.na(yearEnd), yearEnd := maxYear]
  skeleton[, yearEnd := min(yearEnd, na.rm = T), by = municip]
  skeleton <- skeleton[year <= yearEnd]
  skeleton[, yearEnd := NULL]
  skeleton[, yearStart := 9999]
  skeleton[!is.na(municipName), yearStart := year]
  skeleton[, yearStart := min(yearStart, na.rm = T), by = municip]
  skeleton <- skeleton[year >= yearStart]
  skeleton[, municipEnd := zoo::na.locf(municipEnd), by = municip]
  skeleton[, municipName := zoo::na.locf(municipName), by = municip]
  skeleton[, county := zoo::na.locf(county), by = municip]
  skeleton[, countyName := zoo::na.locf(countyName), by = municip]
  skeleton[, region := zoo::na.locf(region), by = municip]
  skeleton[, regionName := zoo::na.locf(regionName), by = municip]

  skeletonFinal <- skeleton[year == maxYear]
  skeletonFinal[, year := NULL]
  skeletonFinal[, municipEnd := NULL]
  skeletonOther <- skeleton[, c("municip", "year", "municipEnd")]

  mappings <- unique(skeleton[!is.na(municipEnd), c("municip", "municipEnd")])
  setnames(mappings, c("municipEnd", "realEnd"))

  continueWithMerging <- TRUE
  while (continueWithMerging) {
    skeletonOther <- merge(skeletonOther, mappings, all.x = T, by = "municipEnd")
    skeletonOther[!is.na(realEnd), municipEnd := realEnd]
    # print(skeletonOther[municip %in% c("municip1723","municip1756","municip5053")])
    if (sum(!is.na(skeletonOther$realEnd)) == 0) {
      continueWithMerging <- FALSE
    }
    skeletonOther[, realEnd := NULL]
  }
  skeletonOther[, realEnd := municip]
  skeletonOther[!is.na(municipEnd), realEnd := municipEnd]
  setnames(skeletonFinal, "municip", "realEnd")

  skeletonFinal <- merge(skeletonOther, skeletonFinal, by = c("realEnd"))
  skeletonFinal[is.na(municipEnd), municipEnd := municip]
  setorder(skeletonFinal, realEnd, year)
  skeletonFinal[, realEnd := NULL]
  # skeletonFinal[municip %in% c("municip1723","municip1756","municip5053")]
  # skeletonFinal[municip %in% c("municip0301")]

  if (dir.exists(file.path("inst", "createddata"))) {
    try(saveRDS(skeletonFinal, file.path("inst", "createddata", "norwayMunicipMerging.RDS")), TRUE)
  }

  return(invisible(skeletonFinal))
}

#' NorwayMunicipMerging
#' @export NorwayMunicipMerging
NorwayMunicipMerging <- function() {
  if (is.null(vars$norwayMunicipMerging)) {
    VARS$norwayMunicipMerging <- readRDS(system.file("createddata", "norwayMunicipMerging.RDS", package = "fhi"))
  }
  return(VARS$norwayLocations)
}

#' Creates the population dataset
#' https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/
#' @import data.table
#' @export GenNorwayPopulation
GenNorwayPopulation <- function() {
  # variables used in data.table functions in this function
  . <- NULL
  value <- NULL
  age <- NULL
  Var2 <- NULL
  agecont <- NULL
  pop <- NULL
  municip <- NULL
  region <- NULL
  variable <- NULL
  agenum <- NULL
  # end

  popFiles <- c(
    "Personer2005-2009.csv",
    "Personer2010-2014.csv",
    "Personer2015-2018.csv",
    "Personer2019.csv"
  )
  pop <- vector("list", length = length(popFiles))
  for (i in seq_along(pop)) {
    pop[[i]] <- fread(system.file("extdata", popFiles[i], package = "fhi"), encoding = "UTF-8")
    pop[[i]] <- melt.data.table(pop[[i]], id.vars = c("region", "age"))
  }
  pop <- rbindlist(pop)
  pop[, municip := sprintf("municip%s", stringr::str_extract(region, "^[0-9][0-9][0-9][0-9]"))]
  pop[, year := as.numeric(stringr::str_extract(variable, "[0-9][0-9][0-9][0-9]$"))]
  pop[, agenum := as.numeric(stringr::str_extract(age, "^[0-9]*"))]

  pop[, age := NULL]
  setnames(pop, "agenum", "age")

  pop <- pop[, .(
    pop = sum(value)
  ), keyby = .(
    municip, age, year
  )]

  # Fixing broken parts in the population data
  # part 1
  pop2 <- pop[municip == "municip0710" & year <= 2017]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip := "municip0706"]
  pop2[, pop := round(pop / 3)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip == "municip0710" & year <= 2017]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip := "municip0719"]
  pop2[, pop := round(pop / 3)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip == "municip0710" & year <= 2017]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip := "municip0720"]
  pop2[, pop := round(pop / 3)]
  pop <- rbind(pop, pop2)

  # part 2
  pop2 <- pop[municip == "municip1756" & year <= 2012]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip := "municip1723"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip == "municip1756" & year <= 2012]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip := "municip1729"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  # part 3
  pop2 <- pop[municip == "municip5046" & year <= 2018]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip := "municip1901"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip == "municip1756" & year <= 2018]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip := "municip1915"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  # part 4
  pop2 <- pop[municip == "municip1505" & year <= 2008]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip := "municip1503"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)

  pop2 <- pop[municip == "municip1505" & year <= 2008]
  pop2[, pop := max(pop), by = age]
  pop2 <- pop2[year != max(year)]
  pop2[, municip := "municip1556"]
  pop2[, pop := round(pop / 2)]
  pop <- rbind(pop, pop2)
  pop[, imputed := FALSE]

  missingYears <- max(pop$year):lubridate::year(lubridate::today()) + 2
  if (length(missingYears) > 1) {
    copiedYears <- vector("list", length = length(missingYears) - 1)
    for (i in seq_along(copiedYears)) {
      copiedYears[[i]] <- pop[year == missingYears[1]]
      copiedYears[[i]][, year := year + i]
    }
    copiedYears <- rbindlist(copiedYears)
    copiedYears[, imputed := TRUE]
    pop <- rbind(pop, copiedYears)
  }

  norwayMerging <- GenNorwayMunicipMerging()
  pop <- merge(pop, norwayMerging[, c("year", "municip", "municipEnd")], by = c("municip", "year"))
  pop <- pop[, .(pop = sum(pop)),
    keyby = .(
      year,
      municip = municipEnd,
      age
    )
  ]

  if (dir.exists(file.path("inst", "createddata"))) {
    try(saveRDS(pop, file.path("inst", "createddata", "norwayPopulation.RDS")), TRUE)
  }

  return(invisible(pop))
}


#' NorwayPopulation
#' @export NorwayPopulation
NorwayPopulation <- function() {
  if (is.null(vars$norwayPopulation)) {
    VARS$norwayPopulation <- readRDS(system.file("createddata", "norwayPopulation.RDS", package = "fhi"))
  }
  return(VARS$norwayPopulation)
}
