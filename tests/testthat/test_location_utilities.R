context("Location Utilities")

library(fhi)

test_that("get_location_name", {
  testthat::expect_equal(get_location_name("county01"), "Østfold")
  expect_equal(get_location_name("municip0213"), "Ski")
  expect_equal(get_location_name("municip1805"), "Narvik")
  expect_equal(get_location_name("norway"), "Norway")
  expect_equal(isTRUE(get_location_name("municip999")), FALSE)
})
