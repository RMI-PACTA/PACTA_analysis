test_that("validate_exchange_rates() function exists", {
  expect_true(
    class(validate_exchange_rates) == "function"
  )
})

test_that("validate_exchange_rates() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^validate_", "", deparse(substitute(validate_exchange_rates))) %in% data_object_names
  )
})

test_that("validate_exchange_rates() returns TRUE for example data", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/exchange_rates.rds"))
  expect_true(
    validate_exchange_rates(example_data)
  )
})

test_that("validate_exchange_rates() returns FALSE for data with no columns", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/exchange_rates.rds"))
  expect_false(
    validate_exchange_rates(data.frame())
  )
})

test_that("validate_exchange_rates() returns FALSE for data with a specified column missing", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/exchange_rates.rds"))
  expect_false(
    validate_exchange_rates(example_data[-1])
  )
})

test_that("validate_exchange_rates() returns FALSE for data without an ExchangeRate_ column", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/exchange_rates.rds"))
  expect_false(
    validate_exchange_rates(example_data[1:2])
  )
})

test_that("validate_exchange_rates() returns FALSE for data with a column of a different type", {
  skip_check_but_run_test()
  example_data <- readRDS(here::here("inst/extdata/exchange_rates.rds"))
  example_data[1] <- TRUE
  expect_false(
    validate_exchange_rates(example_data)
  )
})
