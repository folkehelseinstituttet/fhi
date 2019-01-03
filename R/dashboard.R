#' Is the dashboard in production?
#' @export DashboardIsProduction
DashboardIsProduction <- function() {
  return(PROJ$IS_PRODUCTION)
}

#' Is the dashboard in development?
#' @export DashboardIsDev
DashboardIsDev <- function() {
  return(PROJ$IS_DEV)
}

#' Is the dashboard initialised?
#' @export DashboardIsInitialised
DashboardIsInitialised <- function() {
  return(PROJ$IS_INITIALISED)
}

#' Initialise the automated analysis
#'
#' If folders are setup according to the
#' dashboard philosophy, then this function
#' locates the computer's name in `/tmp/computer`
#' and sets PROJ as appropriate
#' @param STUB The directory containing the `data_raw`, `data_clean`, `data_app`, and `results` folders
#' @param SRC The directory inside `STUB` containing `ANALYSIS/RunProcess.R`
#' @param NAME The name of the automated analysis
#' @param changeWorkingDirToTmp Do you want to change the working directory to a temporary directory?
#' @export DashboardInitialise
DashboardInitialise <- function(
                                STUB = "/",
                                SRC = "src",
                                NAME = NULL,
                                changeWorkingDirToTmp = TRUE) {
  # nolint start
  if (file.exists("/tmp/computer")) {
    con <- file("/tmp/computer", "r")
    COMPUTER_NAME <- readLines(con, n = 1)
    close(con)
  } else {
    COMPUTER_NAME <- "NO_NAME_FOUND"
  }
  # nolint end
  Sys.setenv(COMPUTER = COMPUTER_NAME)

  PROJ$COMPUTER_NAME <- COMPUTER_NAME

  PROJ$STUB <- STUB
  PROJ$SRC <- SRC
  PROJ$NAME <- NAME

  if (changeWorkingDirToTmp) {
    setwd(tempdir())
  }
  PROJ$IS_INITIALISED <- TRUE
}

#' Messaging
#' @param txt Text
#' @param type msg, warn, err
#' @param syscallsDepth The number of syscalls included in the message. Set to 0 to disable.
#' @param newLine Should there be a new line at the start of the message?
#' @export DashboardMsg
DashboardMsg <- function(txt, type = "msg", syscallsDepth = 2, newLine = FALSE) {

  # make warnings print immediately
  op <- options("warn")
  on.exit(options(op))
  options(warn = 1)

  SYSCALLS$CALLS <- sys.calls()
  if (syscallsDepth < 0) stop("syscallsDepth cannot be less than zero")
  if (!type %in% c("msg", "warn", "err")) stop(sprintf("%s not msg, warn, err", type))

  startOfLine <- ""
  if (newLine) startOfLine <- "\r\n"

  fn <- switch(type,
    msg = base::message,
    warn = base::warning,
    err = base::stop
  )

  depth <- sys.nframe() - 1
  x <- sys.calls()
  if (depth >= 1 & syscallsDepth > 0) {
    depthSeq <- depth:1
    if (length(depthSeq) > syscallsDepth) depthSeq <- depthSeq[1:syscallsDepth]
    depthSeq <- rev(depthSeq)
    for (i in depthSeq) {
      base::message(startOfLine, "           ", depth - i + 1, "/", depth, ": ", deparse(x[[i]]))
    }
  }

  if (type == "msg") {
    if (PROJ$IS_INITIALISED) {
      fn(sprintf("%s%s/%s/%s %s\r", startOfLine, Sys.time(), PROJ$COMPUTER_NAME, PROJ$NAME, txt))
    } else {
      fn(sprintf("%s%s %s\r", startOfLine, Sys.time(), txt))
    }
  } else {
    if (PROJ$IS_INITIALISED) {
      fn(sprintf("%s%s/%s/%s %s\r", startOfLine, Sys.time(), PROJ$COMPUTER_NAME, PROJ$NAME, txt), call. = F)
    } else {
      fn(sprintf("%s%s %s\r", startOfLine, Sys.time(), txt), call. = F)
    }
  }
}

#' DashboardInitialiseOpinionated
#' @param NAME The name of the automated analysis folders
#' @param PKG The name of the R package (generally the same as `NAME`)
#' @param STUB The directory containing the `data_raw`, `data_clean`, `data_app`, and `results` folders
#' @param PACKAGE_DIR The directory containing the package source code
#' @param FORCE_DEV_PACKAGE_LOAD a
#' @param DEV_IF_RSTUDIO If function is called from RStudio, flag `PROJ$IS_DEV` as `TRUE`
#' @param SILENT Load all packages silently?
#' @importFrom devtools load_all
#' @export DashboardInitialiseOpinionated

# nolint start
DashboardInitialiseOpinionated <- function(NAME, PKG = NAME, STUB = "/", PACKAGE_DIR = ".", FORCE_DEV_PACKAGE_LOAD = FALSE, DEV_IF_RSTUDIO = TRUE, SILENT = FALSE) {
  # nolint end

  DashboardInitialise(
    STUB = STUB,
    SRC = "src",
    NAME = NAME,
    changeWorkingDirToTmp = (Sys.getenv("RSTUDIO") != "1")
  )

  if (Sys.getenv("RSTUDIO") == "1" | FORCE_DEV_PACKAGE_LOAD) {
    if (SILENT) {
      suppressPackageStartupMessages(devtools::load_all(PACKAGE_DIR, export_all = FALSE, quiet = TRUE))
    } else {
      devtools::load_all(PACKAGE_DIR, export_all = FALSE)
    }

    if (DEV_IF_RSTUDIO) {
      PROJ$IS_DEV <- TRUE
    }
  } else {
    if (SILENT) {
      suppressPackageStartupMessages(library(PKG, character.only = TRUE))
    } else {
      library(PKG, character.only = TRUE)
    }

    if (PROJ$COMPUTER_NAME == PROJ$PRODUCTION_NAME) {
      PROJ$IS_PRODUCTION <- TRUE
      PROJ$DEFAULT_EMAILS_XLSX_LOCATION <- file.path("/etc", "gmailr", "emails.xlsx") # nolint
    }
  }
}

#' If folders are setup according to the
#' dashboard philosophy, then this function
#' finds folders according to the dashboard
#' @param inside where it is inside
#' @param f an optional file
#' @export DashboardFolder
DashboardFolder <- function(inside = "data_raw", f = NULL) {
  retVal <- file.path(PROJ$STUB, inside, PROJ$NAME)
  retVal <- paste0(retVal, "/")
  if (!is.null(f)) {
    retVal <- file.path(retVal, f)
  }
  return(retVal)
}

#' Sends out mass emails
#' @param emailsFromExcel a
#' @param emailSubject a
#' @param emailText a
#' @param emailAttachFiles a
#' @param emailFooter a
#' @param BCC a
#' @param emailsDirect a
#' @param XLSXLocation a
#' @param OAUTHLocation a
#' @import gmailr
#' @importFrom magrittr %>%
#' @export DashboardEmail
DashboardEmail <- function(emailsFromExcel = NULL,
                           emailSubject,
                           emailText,
                           emailAttachFiles = NULL,
                           emailFooter = TRUE,
                           BCC = TRUE,
                           emailsDirect = NULL,
                           XLSXLocation = PROJ$DEFAULT_EMAILS_XLSX_LOCATION,
                           OAUTHLocation = PROJ$DEFAULT_EMAILS_OAUTH_LOCATION) {
  if (!is.null(emailsFromExcel)) {
    emails <- readxl::read_excel(XLSXLocation)
    emails <- stats::na.omit(emails[[emailsFromExcel]])
  } else {
    emails <- emailsDirect
    if (length(emails) > 1) emails <- paste0(emails, collapse = ",")
  }

  if (!fhi::DashboardIsProduction()) {
    emailSubject <- paste0("TESTING: ", emailSubject)
  }

  if (emailFooter) {
    emailText <- paste0(
      emailText,
      "<br><br><br>
    ------------------------
    <br>
    DO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!
    <br>
    To add or remove people to/from this notification list, send their details to richard.white@fhi.no
    "
    )
  }

  text_msg <- gmailr::mime()

  sendEmailsBCC <- sendEmailsTo <- NULL
  if (BCC) {
    sendEmailsTo <- "dashboards@fhi.no"
    sendEmailsBCC <- paste0(emails, collapse = ",")
  } else {
    sendEmailsTo <- paste0(emails, collapse = ",")
    sendEmailsBCC <- "dashboards@fhi.no"
  }

  gmailr::mime() %>%
    gmailr::to(sendEmailsTo) %>%
    gmailr::from("Dashboards FHI <dashboardsfhi@gmail.com>") %>%
    gmailr::bcc(sendEmailsBCC) %>%
    gmailr::subject(emailSubject) %>%
    gmailr::html_body(emailText) -> text_msg

  if (!is.null(emailAttachFiles)) {
    text_msg %>% gmailr::attach_part(emailText, content_type = "text/html") -> text_msg
    for (i in emailAttachFiles) {
      text_msg %>% gmailr::attach_file(i) -> text_msg
    }
  }

  currentWD <- getwd()
  tmp <- tempdir()
  file.copy(OAUTHLocation, file.path(tmp, ".httr-oauth"))
  setwd(tmp)
  gmailr::gmail_auth()
  gmailr::send_message(text_msg)
  setwd(currentWD)
}
