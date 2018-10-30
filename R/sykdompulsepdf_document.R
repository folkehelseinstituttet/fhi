#' hi
#' @param ... test
#' @importFrom rmarkdown pandoc_available pdf_document
#' @export sykdompulsepdf_document_mage
sykdompulsepdf_document_mage <- function(...) {
  rmarkdown::pandoc_available("1.9", TRUE)

  template <- system.file("rmarkdown", "templates", "sykdomspulspdf_mage", "resources", "template.latex",
    package = "fhi"
  )

  base <- rmarkdown::pdf_document(..., template = template, latex_engine = "xelatex")
  base$inherits <- "pdf_document"

  base
}


#' hi
#' @param ... test
#' @importFrom rmarkdown pandoc_available pdf_document
#' @export sykdompulsepdf_document_luft
sykdompulsepdf_document_luft <- function(...) {
  rmarkdown::pandoc_available("1.9", TRUE)

  template <- system.file("rmarkdown", "templates", "sykdomspulspdf_luft", "resources", "template.latex",
    package = "fhi"
  )

  base <- rmarkdown::pdf_document(..., template = template, latex_engine = "xelatex")
  base$inherits <- "pdf_document"

  base
}
