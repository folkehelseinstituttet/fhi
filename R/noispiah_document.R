#' hi
#' @param ... test
#' @importFrom rmarkdown pandoc_available pdf_document
#' @export noispiah_document
noispiah_document <- function(...) {
  rmarkdown::pandoc_available("1.9", TRUE)

  template <- system.file("rmarkdown", "templates", "noispiah", "resources", "template.latex",
    package = "fhi"
  )

  base <- rmarkdown::pdf_document(..., template = template, latex_engine = "xelatex")
  base$inherits <- "pdf_document"

  base
}

#' hi
#' @param output_dir a
#' @export noispiah_resources_copy
noispiah_resources_copy <- function(output_dir) {
  dir <- system.file("rmarkdown", "templates", "noispiah", "skeleton",
    package = "fhi"
  )
  files <- list.files(dir)
  files <- files[tolower(files) != "skeleton.rmd"]

  for (f in files) {
    file.copy(
      from = file.path(dir, f),
      to = file.path(output_dir, f)
    )
  }
}


#' hi
#' @param output_dir a
#' @export noispiah_resources_remove
noispiah_resources_remove <- function(output_dir) {
  dir <- system.file("rmarkdown", "templates", "noispiah", "skeleton",
    package = "fhi"
  )
  files <- list.files(dir)
  files <- files[tolower(files) != "skeleton.rmd"]

  for (f in files) {
    file.remove(file.path(output_dir, f))
  }
}
