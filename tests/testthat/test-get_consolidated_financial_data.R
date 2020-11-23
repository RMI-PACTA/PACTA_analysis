setwd(here::here())
function_name <- "get_consolidated_financial_data"
result <- get_consolidated_financial_data("inst/extdata")

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

test_that(paste0(function_name, "() returns a data.frame with 19 columns"), {
  expect_true(
    ncol(result) == 19
  )
})

test_that(paste0(function_name, "() returns a data.frame with the proper column names"), {
  expect_equal(
    names(result),
    c("company_id", "bloomberg_id", "company_name", "country_of_domicile",
      "corporate_bond_ticker", "equity_ticker", "mapped_sector", "bics_sector",
      "bics_subgroup", "icb_subgroup", "bclass4", "has_asset_level_data",
      "has_assets_in_matched_sector", "sectors_with_assets", "market_cap",
      "free_float_shares", "current_shares_outstanding",
      "current_shares_outstanding_all_classes", "financial_timestamp")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper character class columns"), {
  char_cols <- c("company_name", "country_of_domicile", "corporate_bond_ticker",
                 "equity_ticker", "mapped_sector", "bics_sector",
                 "bics_subgroup", "icb_subgroup", "bclass4",
                 "sectors_with_assets", "financial_timestamp")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper numeric class columns"), {
  num_cols <- c("company_id", "bloomberg_id", "market_cap", "free_float_shares", "current_shares_outstanding", "current_shares_outstanding_all_classes")
  expect_true(
    all(sapply(result[num_cols], class) == "numeric")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper logical class columns"), {
  log_cols <- c("has_asset_level_data", "has_assets_in_matched_sector")
  expect_true(
    all(sapply(result[log_cols], class) == "logical")
  )
})
