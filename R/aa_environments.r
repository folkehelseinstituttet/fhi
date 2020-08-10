#' Environment to store config
#' test
#' @export config
config <- new.env()

#' Norwegian characters in unicode
#' @export nb
nb <- new.env()
nb$AA <- "\u00C5"
nb$aa <- "\u00E5"
nb$OE <- "\u00D8"
nb$oe <- "\u00F8"
nb$AE <- "\u00C6"
nb$ae <- "\u00E6"

yrwks <- format(
  seq.Date(
    as.Date("1900-01-01"),
    as.Date("2200-01-01"),
    by = 7
  ),
  "%G-%V"
)
