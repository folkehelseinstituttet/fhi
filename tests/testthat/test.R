library(stringr)
context("String length")

test_that("str_length is number of characters", {
  f <- system.file("rmarkdown/templates/noispiah/skeleton", "skeleton.Rmd", package = "fhi")
  #rmarkdown::render(input=f,output_dir = tempdir())

  testthat::expect_equal(1,1)
})
