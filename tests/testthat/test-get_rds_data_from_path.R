test_that("get_rds_data_from_path() function exists", {
  expect_true(
    class(get_rds_data_from_path) == "function"
  )
})

test_that("get_rds_data_from_path() function returns a data frame", {
  result <- get_rds_data_from_path(path = here::here("inst/extdata"), filename = "bics_bridge.rds")
  expect_true(
    inherits(result, "data.frame")
  )
})
