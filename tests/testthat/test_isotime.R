context("ISO Time")

library(fhi)

test_that("season", {

  testthat::expect_equal(season("2018-23"), c("2018-23"="2017/2018"))
  testthat::expect_equal(season(c("2018-23", "2018-31")),
                         c("2018-23"="2017/2018", "2018-31"="2018/2019"))
  testthat::expect_equal(season(c("2018-23", "2018-31", "2018-40"),start_week=40),
                         c("2018-23"="2017/2018", "2018-31"="2017/2018",
                           "2018-40"="2018/2019"))

})


test_that("start_of_season", {

  testthat::expect_equal(start_of_season("2018-23"), "2017-30")
  testthat::expect_equal(start_of_season("2018-32"), "2018-30")
  testthat::expect_equal(start_of_season("2018-32", start_week=40), "2017-40")
  testthat::expect_equal(start_of_season("2018-43", start_week=40), "2018-40")


})
