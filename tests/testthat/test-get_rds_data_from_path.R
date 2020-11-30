test_that("get_rds_data_from_path() function exists", {
  expect_true(
    class(get_rds_data_from_path) == "function"
  )
})

test_that("get_rds_data_from_path() function returns a data frame", {
  skip_check_but_run_test()
  result <- get_rds_data_from_path(path = here::here("inst/extdata"), filename = "bics_bridge.rds")
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_rds_data_from_path() function stops if the file extension is not .rds", {
  expect_error(
    get_rds_data_from_path(path = "inst/extdata", filename = "bics_bridge.xls"),
    regexp = "file does not have the appropriate 'rds' extension"
  )
})

test_that("get_rds_data_from_path() function stops if the file does not exist", {
  expect_error(
    get_rds_data_from_path(path = "inst/extdata", filename = "does_not_exist.rds"),
    regexp = "file does not exist"
  )
})
