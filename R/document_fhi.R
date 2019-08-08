#' fhi_document
#' @param ... Arguments
#' @importFrom rmarkdown pandoc_available pdf_document
#' @export
fhi_document <- function(...) {
  rmarkdown::pandoc_available("1.9", TRUE)

  template <- system.file("rmarkdown", "templates", "fhi", "resources", "template.latex",
    package = "fhi"
  )

  base <- rmarkdown::pdf_document(..., template = template, latex_engine = "xelatex")
  base$inherits <- "pdf_document"

  base
}
