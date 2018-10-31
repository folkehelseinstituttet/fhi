#' PROJ
#' @export PROJ
PROJ <- new.env(parent = emptyenv())
PROJ$COMPUTER_NAME <- "x"
PROJ$PRODUCTION_NAME <- "smhb"
PROJ$IS_PRODUCTION <- FALSE
PROJ$IS_DEV <- FALSE
PROJ$IS_INITIALISED <- FALSE
PROJ$DEFAULT_EMAILS_XLSX_LOCATION <- file.path("/etc", "gmailr", "emails.xlsx")
PROJ$DEFAULT_EMAILS_OAUTH_LOCATION <- file.path("/etc", "gmailr", ".httr-oauth")

#' SYSCALLS
#' @export SYSCALLS
SYSCALLS <- new.env(parent = emptyenv())


#' countyToMunicip
#' @docType data
#' @name countyToMunicip
#' @usage data(countyToMunicip)
NULL
