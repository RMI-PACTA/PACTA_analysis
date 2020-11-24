test_that("validate_currency_data() function exists", {
  expect_true(
    class(validate_currency_data) == "function"
  )
})

test_that("validate_currency_data() returns TRUE for example data", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/currencies.rds"))
  expect_true(
    validate_currency_data(example_data)
  )
})

test_that("validate_currency_data() returns FALSE for data with no columns", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/currencies.rds"))
  expect_false(
    validate_currency_data(data.frame())
  )
})

test_that("validate_currency_data() returns FALSE for data with a specified column missing", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/currencies.rds"))
  expect_false(
    validate_currency_data(example_data[-1])
  )
})

test_that("validate_currency_data() returns FALSE for data without an ExchangeRate_ column", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/currencies.rds"))
  expect_false(
    validate_currency_data(example_data[1:2])
  )
})

test_that("validate_currency_data() returns FALSE for data with a column of a different type", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/currencies.rds"))
  example_data[1] <- TRUE
  expect_false(
    validate_currency_data(example_data)
  )
})
