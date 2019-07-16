file_open <- function(file = tempfile()) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(file, open = "w"), silent = TRUE)
    )
  )
}

#' Is the file stable (size not increasing/decreasing)?
#'
#' @param file The file of interest
#' @param delay Number of seconds between checking file size a second time
#' @export
file_stable <- function(file = tempfile(), delay = 10) {
  if (file.exists(file)) {
    size1 <- file.info(file)$size[1]
    Sys.sleep(delay)
    size2 <- file.info(file)$size[1]
    if (size1 == size2) {
      if (!file_open(file)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}
