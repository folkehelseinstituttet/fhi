#' Flags/values to be used in the 'dashboards' scene
#' @export PROJ
PROJ <- new.env(parent = emptyenv())
PROJ$COMPUTER_NAME <- "x"
PROJ$PRODUCTION_NAME <- "smhb"
PROJ$IS_PRODUCTION <- FALSE
PROJ$IS_DEV <- FALSE
PROJ$IS_INITIALISED <- FALSE
PROJ$DEFAULT_EMAILS_XLSX_LOCATION <- "x" # nolint
PROJ$DEFAULT_EMAILS_OAUTH_LOCATION <- "x" # nolint

#' Storing the last syscalls
#' @export SYSCALLS
SYSCALLS <- new.env(parent = emptyenv())

#' Environment to store logs
#' test
#' @export LOGDATA
LOGDATA <- new.env(parent = emptyenv())
LOGDATA$x <- 1

#' Environment to store config
#' test
#' @export config
config <- new.env()
config$frost_client_id <- Sys.getenv("FROST_CLIENT_ID","c6d9bf2d-104c-4b5f-accf-d367b2220d62")

#' Norwegian characters in unicode
#' @export NORCHAR
NORCHAR <- new.env(parent = emptyenv())
NORCHAR$AA <- "\u00C5"
NORCHAR$aa <- "\u00E5"
NORCHAR$OE <- "\u00D8"
NORCHAR$oe <- "\u00F8"
NORCHAR$AE <- "\u00C6"
NORCHAR$ae <- "\u00E6"

