#' Logs values to LOG$log
#' @param name Name of value being logged
#' @param val Value being logged
#' @param log Name of log
#' @export
Log <- function(name, val=Sys.time(), log="log1") {
  if(is.null(LOG[[log]])){
    LOG[[log]] <- data.frame(x=1)
    LOG[[log]]$x <- NULL
  }
  LOG[[log]][[name]] <- val
}

#' Gets the log
#' @param log Name of log
#' @export
LogGet <- function(log="log1") {
  return(LOG[[log]])
}
