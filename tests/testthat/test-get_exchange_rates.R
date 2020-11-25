test_that("get_exchange_rates() function exists", {
  expect_true(
    class(get_exchange_rates) == "function"
  )
})

test_that("get_exchange_rates() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_", "", deparse(substitute(get_exchange_rates))) %in% data_object_names
  )
})

test_that("get_exchange_rates() returns a data.frame", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates()
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_exchange_rates() returns a data.frame with 7 columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates()
  expect_true(
    ncol(result) == 7
  )
})

test_that("get_exchange_rates() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates()
  expect_equal(
    names(result),
    c("Currency", "Currency_abbr", "ExchangeRate_2016Q4", "ExchangeRate_2017Q4",
      "ExchangeRate_2018Q4", "ExchangeRate_2018Q3", "ExchangeRate_2019Q4")
  )
})

test_that("get_exchange_rates() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates()
  char_cols <- c("Currency", "Currency_abbr")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that("get_exchange_rates() returns a data.frame with proper numeric class columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_exchange_rates()
  char_cols <- c("ExchangeRate_2016Q4", "ExchangeRate_2017Q4",
                 "ExchangeRate_2018Q4", "ExchangeRate_2018Q3",
                 "ExchangeRate_2019Q4")
  expect_true(
    all(sapply(result[char_cols], class) == "numeric")
  )
})
