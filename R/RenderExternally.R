#' RenderExternally
#' @param input a
#' @param output_file a
#' @param output_dir a
#' @param params a
#' @importFrom processx run
#' @importFrom uuid UUIDgenerate
#' @importFrom rmarkdown render
#' @export RenderExternally
RenderExternally <- function(input, output_file, output_dir, params="x=1"){
  tmp_dir <- tempdir()
  tmp_name <- sprintf("%s.pdf",uuid::UUIDgenerate())

  numberFails <- 0
  succeed <- FALSE
  while(numberFails < 3 & succeed==FALSE){
    x <- processx::run(
      command='R',
      args=c(
        '-e',
        sprintf('rmarkdown::render(\"%s\",output_file=\"%s\",output_dir=\"%s\",params=list(%s))',
                input,
                tmp_name,
                tmp_dir,
                params
        )
      ),
      error_on_status = F
    )
    if(x$status!=0){
      print(x)
      print(tmp_dir)
      print(tmp_name)
      numberFails <- numberFails + 1
      print(sprintf("FAILED %s TIMES, RETRYING",numberFails))
    } else {
      succeed <- TRUE
    }
  }
  if(succeed){
    file.copy(file.path(tmp_dir,tmp_name),file.path(output_dir,output_file))
  } else {
    stop("ERROR!!!")
  }
  #file.remove(file.path(tmp_dir,tmp_name))

  return(x)
}
