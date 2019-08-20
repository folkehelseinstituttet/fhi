#' sykdompuls_mage_document
#' @param ... Arguments
#' @importFrom rmarkdown pandoc_available pdf_document
#' @export msisannualreport_document
msisannualreport_document <- function(...) {
  rmarkdown::pandoc_available("1.9", TRUE)
  template <- system.file("rmarkdown", "templates", "sykdompuls_mage_document", "resources", "template.latex",
    package = "fhi"
  )
  base <- rmarkdown::pdf_document(..., template = template, latex_engine = "xelatex")
  base$inherits <- "pdf_document"

  base
}


#' Copies all necessary sykdompuls_mage_document and sykdompuls_luft_document resources
#' @param output_dir Folder to copy resources to
#' @export msisannualreport_resources_copy
msisannualreport_resources_copy <- function(output_dir) {
  dir <- system.file("rmarkdown", "templates", "msis_annualreport", "skeleton",
    package = "fhi"
  )
  files <- list.files(dir, pattern = "^_skeleton")
  for (f in files) {
    file.copy(
      from = file.path(dir, f),
      to = file.path(output_dir, f)
    )
  }
}


#' Removes all necessary sykdompuls_mage_document and sykdompuls_luft_document resources
#' @param output_dir Folder to remove resources from
#' @export msisannualreport_resources_remove
msisannualreport_resources_remove <- function(output_dir) {
  dir <- system.file("rmarkdown", "templates", "msis_annualreport", "skeleton",
    package = "fhi"
  )
  files <- list.files(dir, pattern = "^_skeleton")

  for (f in files) {
    file.remove(file.path(output_dir, f))
  }
}
