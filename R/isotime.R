#' isoyear_c
#' @param x The date of interest
#' @export
isoyear_c <- function(x = lubridate::today()) {
  yr <- format.Date(x, "%G")
  return(yr)
}

#' isoyear_n
#' @param x The date of interest
#' @export
isoyear_n <- function(x = lubridate::today()) {
  yr <- as.numeric(isoyear_c(x))
  return(yr)
}

#' isoweek_c
#' @param x The date of interest
#' @export
isoweek_c <- function(x = lubridate::today()) {
  # wk <- data.table::isoweek(date)
  # wk <- formatC(wk, flag = "0", width = 2)
  wk <- format.Date(x, "%V")
  return(wk)
}

#' isoweek_n
#' @param x The date of interest
#' @export
isoweek_n <- function(x = lubridate::today()) {
  wk <- as.numeric(isoweek_c(x))
  return(wk)
}

#' start_of_season
#' @param yrwk a
#' @param start_week the start week of the season
#' @export
start_of_season <- function(yrwk, start_week = 30) {
  retval <- as.numeric(stringr::str_split(yrwk, "-")[[1]])
  yr <- retval[1]
  wk <- retval[2]

  if (wk >= start_week) {
    start <- glue::glue("{yr}-{start_week}")
  } else {
    start <- glue::glue("{yr-1}-{start_week}")
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

season.int <- function(yrwk, start_week = 30) {
  retval <- as.numeric(stringr::str_split(yrwk, "-")[[1]])
  yr <- retval[1]
  wk <- retval[2]

  if (wk >= start_week) {
    start <- glue::glue("{yr}/{yr+1}")
  } else {
    start <- glue::glue("{yr-1}/{yr}")
  }
  return(start)
}


#' season
#' @param yrwk a
#' @param start_week the start week of the season
#' @export
season <- Vectorize(season.int, vectorize.args = c("yrwk"))

#' x from week
#' @param week week
#' @export
x <- function(week) {
  retval <- week
  retval[week >= 30] <- week[week >= 30] - 29
  retval[week < 30] <- week[week < 30] + 23
  retval[week == 53] <- 23.5

  return(retval)
}
