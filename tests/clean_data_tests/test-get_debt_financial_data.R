setwd(here::here())
function_name <- "get_debt_financial_data"
result <- get_debt_financial_data("tests/clean_data_tests/example_data")

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

test_that(paste0(function_name, "() returns a data.frame with 4 columns"), {
  expect_true(
    ncol(result) == 4
  )
})

test_that(paste0(function_name, "() returns a data.frame with the proper column names"), {
  expect_equal(
    names(result),
    c("corporate_bond_ticker", "has_asset_level_data", "sectors_with_assets", "bics_sector")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper character class columns"), {
  char_cols <- c("corporate_bond_ticker", "sectors_with_assets", "bics_sector")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper logical class columns"), {
  log_cols <- c("has_asset_level_data")
  expect_true(
    all(sapply(result[log_cols], class) == "logical")
  )
})
