example_data <- readRDS(here::here("inst/extdata/currencies.rds"))

test_that("validate_currency_data() function exists", {
  expect_true(
    class(validate_currency_data) == "function"
  )
})

test_that("validate_currency_data() returns TRUE for example data", {
  expect_true(
    validate_currency_data(example_data)
  )
})

test_that("validate_currency_data() returns FALSE for data with no columns", {
  expect_false(
    validate_currency_data(data.frame())
  )
})

test_that("validate_currency_data() returns FALSE for data with a specified column missing", {
  expect_false(
    validate_currency_data(example_data[-1])
  )
})

test_that("validate_currency_data() returns FALSE for data without an ExchangeRate_ column", {
  expect_false(
    validate_currency_data(example_data[1:2])
  )
})

test_that("validate_currency_data() returns FALSE for data with a column of a different type", {
  example_data_local <- example_data
  example_data_local[1] <- TRUE
  expect_false(
    validate_currency_data(example_data_local)
  )
})
