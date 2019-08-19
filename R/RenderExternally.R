#' Render a markdown file in an external R process
#' @param input Rmd file to be rendered
#' @param output_file Output file
#' @param output_dir Output directory
#' @param params Any params that need to be passed to the Rmd file
#' @export RenderExternally
RenderExternally <- function(input, output_file, output_dir, params = "x=1") {
  file_path <- file.path(output_dir, output_file)
  if(fs::file_exists(file_path)){
    fs::file_delete(file_path)
  }
  tmp_dir <- file.path(tempdir(), uuid::UUIDgenerate())
  dir.create(tmp_dir)
  tmp_name <- sprintf("%s.pdf", uuid::UUIDgenerate())

  numberFails <- 0
  succeed <- FALSE
  while (numberFails < 3 & succeed == FALSE) {
    x <- processx::run(
      command = "R",
      args = c(
        "-e",
        sprintf(
          'rmarkdown::render(\"%s\",output_file=\"%s\",output_dir=\"%s\",intermediates_dir=\"%s\",params=list(%s))',
          input,
          tmp_name,
          tmp_dir,
          tmp_dir,
          params
        )
      ),
      error_on_status = F
    )
    if (x$status != 0) {
      print(x)
      print(tmp_dir)
      print(tmp_name)
      numberFails <- numberFails + 1
      print(sprintf("FAILED %s TIMES, RETRYING", numberFails))
    } else {
      succeed <- TRUE
    }
  }
  if (succeed) {
    file.copy(file.path(tmp_dir, tmp_name), file.path(output_dir, output_file))
    unlink(tmp_dir, recursive = TRUE)
  } else {
    stop("ERROR!!!")
  }
  # file.remove(file.path(tmp_dir,tmp_name))

  return(x)
}
