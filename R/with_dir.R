#' with_dir
#' Taken from https://stackoverflow.com/questions/26825000/in-r-do-an-operation-temporarily-using-a-setting-such-as-working-directory
#' @param dir Working directory
#' @param expr Expression
#' @export
with_dir <- function(dir, expr) {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(dir)
  evalq(expr)
}
