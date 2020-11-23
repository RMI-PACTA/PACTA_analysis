# get_currency_data_for_timestamp() --------------------------------------------

setwd(here::here())
result <- get_currency_data_for_timestamp("2019Q4")

test_that("get_currency_data_for_timestamp() function exists", {
  expect_false(
    is.null(get_currency_data_for_timestamp)
  )
})

test_that("get_currency_data_for_timestamp() returns a data.frame", {
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_currency_data_for_timestamp() returns a data.frame with 2 columns", {
  expect_true(
    ncol(result) == 2
  )
})

test_that("get_currency_data_for_timestamp() returns a data.frame with the proper column names", {
  expect_equal(
    names(result),
    c("currency", "exchange_rate")
  )
})

test_that("get_currency_data_for_timestamp() returns a data.frame with a character class currency column", {
  expect_true(
    inherits(result$currency, "character")
  )
})

test_that("get_currency_data_for_timestamp() returns a data.frame with a currency column that does not contain any duplicates", {
  expect_false(
    any(duplicated(result$currency))
  )
})

test_that("get_currency_data_for_timestamp() returns a data.frame with a numeric class exchange_rate column", {
  expect_true(
    inherits(result$exchange_rate, "numeric")
  )
})
