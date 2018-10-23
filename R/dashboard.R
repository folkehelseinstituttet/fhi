#' Is the dashboard in production?
#' @export DashboardIsProduction
DashboardIsProduction <- function(){
  return(PROJ$IS_PRODUCTION)
}

#' Is the dashboard in development?
#' @export DashboardIsDev
DashboardIsDev <- function(){
  return(PROJ$IS_DEV)
}

#' If folders are setup according to the
#' dashboard philosophy, then this function
#' sets RPROJ
#' @param STUB a
#' @param SRC a
#' @param NAME a
#' @param changeWorkingDirToTmp a
#' @export DashboardInitialise
DashboardInitialise <- function(
  STUB="/",
  SRC="src",
  NAME=NULL,
  changeWorkingDirToTmp=TRUE
  ){

  con <- file("/tmp/computer","r")
  COMPUTER_NAME <- readLines(con,n=1)
  close(con)
  Sys.setenv(COMPUTER=COMPUTER_NAME)

  PROJ$COMPUTER_NAME <- COMPUTER_NAME

  PROJ$STUB <- STUB
  PROJ$SRC <- SRC
  PROJ$NAME <- NAME

  if(changeWorkingDirToTmp){
    setwd(tempdir())
  }
  PROJ$IS_INITIALIZED <- TRUE
}

#' Messaging
#' @param txt a
#' @export DashboardMsg
DashboardMsg <- function(txt){
  if(PROJ$IS_INITIALIZED){
    base::message(sprintf("%s/%s/%s %s",Sys.time(),PROJ$COMPUTER_NAME,PROJ$NAME,txt))
  } else {
    base::message(sprintf("%s %s",Sys.time(),txt))
  }
}

#' DashboardInitialiseOpinionated
#' @param NAME a
#' @param FORCE_DEV_PACKAGE_LOAD a
#' @param DEV_IF_RSTUDIO a
#' @param SILENT a
#' @importFrom devtools load_all
#' @export DashboardInitialiseOpinionated
DashboardInitialiseOpinionated <- function(NAME,FORCE_DEV_PACKAGE_LOAD=FALSE,DEV_IF_RSTUDIO=TRUE,SILENT=FALSE){
  DashboardInitialise(
    STUB="/",
    SRC="src",
    NAME=NAME
  )

  if(Sys.getenv("RSTUDIO") == "1" | FORCE_DEV_PACKAGE_LOAD){
    if(SILENT){
      suppressPackageStartupMessages(devtools::load_all(sprintf("/packages/dashboards_%s/",PROJ$NAME), export_all=FALSE, quiet=TRUE))
    } else {
      devtools::load_all(sprintf("/packages/dashboards_%s/",PROJ$NAME), export_all=FALSE)
    }

    if(DEV_IF_RSTUDIO){
      PROJ$IS_DEV <- TRUE
    }

  } else {
    if(SILENT){
      suppressPackageStartupMessages(library(NAME,character.only = TRUE))
    } else {
      library(NAME,character.only = TRUE)
    }

    if(PROJ$COMPUTER_NAME==PROJ$PRODUCTION_NAME){
      PROJ$IS_PRODUCTION <- TRUE
    }
  }


}

#' If folders are setup according to the
#' dashboard philosophy, then this function
#' finds folders according to the dashboard
#' @param inside where it is inside
#' @param f an optional file
#' @export DashboardFolder
DashboardFolder <- function(inside="data_raw",f=NULL){
  retVal <- file.path(PROJ$STUB,inside,PROJ$NAME)
  retVal <- paste0(retVal,"/")
  if(!is.null(f)){
    retVal <- file.path(retVal,f)
  }
  return(retVal)
}

#' Sends out mass emails that are stored in an xlsx file
#' @param emailBCC a
#' @param emailSubject a
#' @param emailText a
#' @param emailAttachFiles a
#' @param emailFooter a
#' @param BCC a
#' @param XLSXLocation a
#' @param OAUTHLocation a
#' @import gmailr
#' @importFrom magrittr %>%
#' @export DashboardEmail
DashboardEmail <- function(emailBCC,
                           emailSubject,
                           emailText,
                           emailAttachFiles=NULL,
                           emailFooter=TRUE,
                           BCC=TRUE,
                           XLSXLocation=file.path("/etc","gmailr","emails.xlsx"),
                           OAUTHLocation=file.path("/etc","gmailr",".httr-oauth")){
  emails <- readxl::read_excel(XLSXLocation)
  emails <- stats::na.omit(emails[[emailBCC]])

  if(emailFooter) emailText <- paste0(emailText,
                                      "<br><br><br>
    ------------------------
    <br>
    DO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!
    <br>
    To add or remove people to/from this notification list, send their details to richard.white@fhi.no
    ")

  text_msg <- gmailr::mime()

  sendEmailsBCC <- sendEmailsTo <- NULL
  if(BCC){
    sendEmailsTo <- "dashboards@fhi.no"
    sendEmailsBCC <- paste0(emails,collapse=",")
  } else {
    sendEmailsTo <- paste0(emails,collapse=",")
    sendEmailsBCC <- "dashboards@fhi.no"
  }

  gmailr::mime() %>%
    gmailr::to(sendEmailsTo) %>%
    gmailr::from("Dashboards FHI <dashboardsfhi@gmail.com>") %>%
    gmailr::bcc(sendEmailsBCC) %>%
    gmailr::subject(emailSubject) %>%
    gmailr::html_body(emailText) -> text_msg

  if(!is.null(emailAttachFiles)){
    text_msg %>% gmailr::attach_part(emailText, content_type="text/html") -> text_msg
    for(i in emailAttachFiles){
      text_msg %>% gmailr::attach_file(i) -> text_msg
    }
  }

  currentWD <- getwd()
  tmp <- tempdir()
  file.copy(OAUTHLocation,paste0(tmp,"/.httr-oauth"))
  setwd(tmp)
  gmailr::gmail_auth()
  gmailr::send_message(text_msg)
  setwd(currentWD)
}

#' Sends out mass emails that are stored in an xlsx file
#' @param emailBCC a
#' @param emailSubject a
#' @param emailText a
#' @param emailFooter a
#' @param OAUTHLocation a
#' @importFrom magrittr %>%
#' @export DashboardEmailSpecific
DashboardEmailSpecific <- function(emailBCC,
                           emailSubject,
                           emailText,
                           emailFooter=TRUE,
                           OAUTHLocation=file.path("/etc","gmailr",".httr-oauth")){
  if(length(emailBCC)>1) emailBCC <- paste0(emailBCC,collapse=",")

  if(emailFooter) emailText <- paste0(emailText,
                      "<br><br><br>
    ------------------------
    <br>
    DO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!
    <br>
    To add or remove people to/from this notification list, send their details to richard.white@fhi.no
    ")

  gmailr::mime() %>%
    gmailr::to("dashboards@fhi.no") %>%
    gmailr::from("Dashboards FHI <dashboardsfhi@gmail.com>") %>%
    gmailr::bcc(emailBCC) %>%
    gmailr::subject(emailSubject) %>%
    gmailr::html_body(emailText) -> text_msg

  currentWD <- getwd()
  tmp <- tempdir()
  file.copy(OAUTHLocation,paste0(tmp,"/.httr-oauth"))
  setwd(tmp)
  gmailr::gmail_auth()
  gmailr::send_message(text_msg)
  setwd(currentWD)
}

