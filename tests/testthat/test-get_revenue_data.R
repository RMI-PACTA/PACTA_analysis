setwd(here::here())
function_name <- "get_revenue_data"
result <- get_revenue_data("inst/extdata")

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns a data.frame"), {
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that(paste0(function_name, "() returns a data.frame with 8 columns"), {
  expect_true(
    ncol(result) == 8
  )
})

test_that(paste0(function_name, "() returns a data.frame with the proper column names"), {
  expect_equal(
    names(result),
    c("company_id", "company_name", "bloomberg_id", "equity_ticker",
      "corporate_bond_ticker", "has_revenue_data", "sector", "tot_rev")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper character class columns"), {
  char_cols <- c("company_name", "equity_ticker", "corporate_bond_ticker", "sector")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper logical class columns"), {
  log_cols <- c("has_revenue_data")
  expect_true(
    all(sapply(result[log_cols], class) == "logical")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper numeric class columns"), {
  num_cols <- c("company_id", "bloomberg_id", "tot_rev")
  expect_true(
    all(sapply(result[num_cols], class) == "numeric")
  )
})
