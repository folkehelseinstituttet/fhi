#' Logs values to LOGDATA$log
#' @param name Name of value being logged
#' @param val Value being logged
#' @param log Name of log
#' @export
Log <- function(name, val = Sys.time(), log = "log1") {
  if (is.null(LOGDATA[[log]])) {
    LOGDATA[[log]] <- data.frame(x = 1)
    LOGDATA[[log]]$x <- NULL
  }
  LOGDATA[[log]][[name]] <- val
}

#' Gets the log
#' @param log Name of log
#' @export
LogGet <- function(log = "log1") {
  return(LOGDATA[[log]])
}
