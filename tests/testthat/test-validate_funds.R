test_that("validate_funds() function exists", {
  expect_true(
    class(validate_funds) == "function"
  )
})

test_that("validate_funds() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^validate_", "", deparse(substitute(validate_funds))) %in% data_object_names
  )
})

test_that("validate_funds() returns TRUE for example data", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fund_data_2019Q4.rds"))
  expect_true(
    validate_funds(example_data)
  )
})

test_that("validate_funds() returns FALSE for data with no columns", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fund_data_2019Q4.rds"))
  expect_false(
    validate_funds(data.frame())
  )
})

test_that("validate_funds() returns FALSE for data with a specified column missing", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fund_data_2019Q4.rds"))
  expect_false(
    validate_funds(example_data[-1])
  )
})

test_that("validate_funds() returns FALSE for data with an unspecified column", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fund_data_2019Q4.rds"))
  example_data$XXX <- TRUE
  expect_false(
    validate_funds(example_data)
  )
})

test_that("validate_funds() returns FALSE for data with a column of a different type", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/fund_data_2019Q4.rds"))
  example_data[1] <- TRUE
  expect_false(
    validate_funds(example_data)
  )
})
