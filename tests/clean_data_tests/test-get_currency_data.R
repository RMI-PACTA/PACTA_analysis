setwd(here::here())
function_name <- "get_currency_data"
result <- get_currency_data()

test_that(paste0(function_name, "() function exists"), {
  expect_false(
    is.null(get_currency_data)
  )
})

test_that(paste0(function_name, "() returns a data.frame"), {
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that(paste0(function_name, "() returns a data.frame with 7 columns"), {
  expect_true(
    ncol(result) == 7
  )
})

test_that(paste0(function_name, "() returns a data.frame with the proper column names"), {
  expect_equal(
    names(result),
    c("Currency", "Currency_abbr", "ExchangeRate_2016Q4", "ExchangeRate_2017Q4",
      "ExchangeRate_2018Q4", "ExchangeRate_2018Q3", "ExchangeRate_2019Q4")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper character class columns"), {
  char_cols <- c("Currency", "Currency_abbr")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper numeric class columns"), {
  char_cols <- c("ExchangeRate_2016Q4", "ExchangeRate_2017Q4",
                 "ExchangeRate_2018Q4", "ExchangeRate_2018Q3",
                 "ExchangeRate_2019Q4")
  expect_true(
    all(sapply(result[char_cols], class) == "numeric")
  )
})
