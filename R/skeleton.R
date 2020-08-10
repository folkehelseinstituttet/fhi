#' make_skeleton
#' @param date_min a
#' @param date_max a
#' @param yrwk_min a
#' @param yrwk_max a
#' @param location_code a
#' @param granularity_geo a
#' @param location_reference a
make_skeleton <- function(
                          date_min = NULL,
                          date_max = NULL,
                          yrwk_min = NULL,
                          yrwk_max = NULL,
                          location_code = NULL,
                          granularity_geo = "all",
                          location_reference = fhidata::norway_locations_long_b2020) {
  if (!is.na(date_min) & !is.na(date_max)) {
    retval <- make_skeleton_date(
      date_min = date_min,
      date_max = date_max,
      location_code = location_code,
      granularity_geo = granularity_geo,
      location_reference = location_reference
    )
  } else if (!is.na(yrwk_min) & !is.na(yrwk_max)) {
    retval <- make_skeleton_date(
      yrwk_min = yrwk_min,
      yrwk_max = yrwk_max,
      location_code = location_code,
      granularity_geo = granularity_geo,
      location_reference = location_reference
    )
  } else {
    stop("must provide either date or yrwk pair")
  }
  return(retval)
}

make_skeleton_date <- function(
                               date_min = NULL,
                               date_max = NULL,
                               location_code = NULL,
                               granularity_geo = "all",
                               location_reference = fhidata::norway_locations_long_b2020,
                               ...) {
  dates <- seq.Date(
    from = as.Date(date_min),
    to = as.Date(date_max),
    by = 1
  )

  locs <- NULL
  if (!is.null(location_code)) {
    locs <- location_code
  } else if (granularity_geo == "all") {
    locs <- location_reference$location_code
  } else {
    x_gran <- granularity_geo
    locs <- location_reference[granularity_geo == x_gran]$location_code
  }
  retval <- expand.grid(
    date = dates,
    location_code = locs,
    ...,
    stringsAsFactors = FALSE
  )
  setDT(retval)
  return(retval)
}

make_skeleton_week <- function(
                               yrwk_min = NULL,
                               yrwk_max = NULL,
                               location_code = NULL,
                               granularity_geo = "all",
                               location_reference = fhidata::norway_locations_long_b2020,
                               ...) {
  yrwks <- seq(
    from = as_isoyearweek(yrwk_min),
    to = as_isoyearweek(yrwk_max),
    by = 1
  )

  locs <- NULL
  if (!is.null(location_code)) {
    locs <- location_code
  } else if (granularity_geo == "all") {
    locs <- location_reference$location_code
  } else {
    x_gran <- granularity_geo
    locs <- location_reference[granularity_geo == x_gran]$location_code
  }
  retval <- expand.grid(
    yrwk = yrwks,
    location_code = locs,
    ...,
    stringsAsFactors = FALSE
  )
  setDT(retval)
  return(retval)
}
