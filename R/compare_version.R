#' compare_version
#'
#' @param package The package name
#' @param version The version to compare it against
#' @export
compare_version <- function(
  package,
  version
){
  compareVersion(
    gsub("'","", packageVersion(package)),
    version
  )
}
