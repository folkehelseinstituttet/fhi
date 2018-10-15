#' hi
#' @param ... test
#' @export noispiah_document
noispiah_document <- function(...) {

  rmarkdown::pandoc_available('1.9', TRUE)

  template <- system.file("rmarkdown", "templates", "noispiah", "resources", "template.latex",
                                      package = "fhi")

  base <- rmarkdown::pdf_document(... , template=template)
  base$inherits <- "pdf_document"
  base

  base
}
