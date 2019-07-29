#' isoyear_c
#' @param date The date of interest
#' @export
isoyear_c <- function(date = lubridate::today()) {
  yr <- format.Date(date, "%G")
  return(yr)
}

#' isoyear_n
#' @param date The date of interest
#' @export
isoyear_n <- function(date = lubridate::today()) {
  yr <- as.numeric(format.Date(date, "%G"))
  return(yr)
}

#' isoweek_c
#' @param date The date of interest
#' @export
isoweek_c <- function(date = lubridate::today()) {
  wk <- data.table::isoweek(date)
  wk <- formatC(wk, flag = "0", width = 2)
  return(wk)
}

#' isoweek_n
#' @param date The date of interest
#' @export
isoweek_n <- function(date = lubridate::today()) {
  wk <- data.table::isoweek(date)
  return(wk)
}

#' isoyearweek
#' @param date The date of interest
#' @export
isoyearweek <- function(date = lubridate::today()) {
  return(sprintf("%s-%s", isoyear_n(date), isoweek_c(date)))
}

#' start_of_season
#' @param yrwk a
#' @export
start_of_season <- function(yrwk) {
  retval <- as.numeric(stringr::str_split(yrwk, "-")[[1]])
  yr <- retval[1]
  wk <- retval[2]

  if (wk > 30) {
    start <- glue::glue("{yr}-30")
  } else {
    start <- glue::glue("{yr-1}-30")
  }
  return(start)
}

#' start_of_year
#' @param yrwk a
#' @export
start_of_year <- function(yrwk) {
  retval <- as.numeric(stringr::str_split(yrwk, "-")[[1]])
  yr <- retval[1]
  wk <- retval[2]

  start <- glue::glue("{yr}-01")

  return(start)
}


#' season
#' @param yrwk a
#' @export
season <- function(yrwk) {
  retval <- as.numeric(stringr::str_split(yrwk, "-")[[1]])
  yr <- retval[1]
  wk <- retval[2]

  if (wk > 30) {
    start <- glue::glue("{yr}/{yr+1}")
  } else {
    start <- glue::glue("{yr-1}/{yr}")
  }
  return(start)
}
