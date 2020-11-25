test_that("get_exchange_rates_for_timestamp() function exists", {
  expect_true(
    class(get_exchange_rates_for_timestamp) == "function"
  )
})

test_that("get_exchange_rates_for_timestamp() returns a data.frame", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates_for_timestamp("2019Q4")
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_exchange_rates_for_timestamp() returns a data.frame with 2 columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates_for_timestamp("2019Q4")
  expect_true(
    ncol(result) == 2
  )
})

test_that("get_exchange_rates_for_timestamp() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates_for_timestamp("2019Q4")
  expect_equal(
    names(result),
    c("currency", "exchange_rate")
  )
})

test_that("get_exchange_rates_for_timestamp() returns a data.frame with a character class currency column", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates_for_timestamp("2019Q4")
  expect_true(
    inherits(result$currency, "character")
  )
})

test_that("get_exchange_rates_for_timestamp() returns a data.frame with a currency column that does not contain any duplicates", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates_for_timestamp("2019Q4")
  expect_false(
    any(duplicated(result$currency))
  )
})

test_that("get_exchange_rates_for_timestamp() returns a data.frame with a numeric class exchange_rate column", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates_for_timestamp("2019Q4")
  expect_true(
    inherits(result$exchange_rate, "numeric")
  )
})
