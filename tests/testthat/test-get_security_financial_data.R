setwd(here::here())
function_name <- "get_security_financial_data"
result <- get_security_financial_data("inst/extdata")

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
      "ticker", "corporate_bond_ticker", "isin", "asset_type",
      "security_icb_subsector", "security_bics_subgroup", "security_bclass4",
      "security_mapped_sector", "security_type", "issue_date",
      "unit_share_price", "market_value", "current_shares_outstanding",
      "current_shares_outstanding_all_classes", "financial_timestamp")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper character class columns"), {
  char_cols <- c("company_name", "country_of_domicile", "ticker",
                 "corporate_bond_ticker", "isin", "asset_type",
                 "security_icb_subsector", "security_bics_subgroup",
                 "security_bclass4", "security_mapped_sector",
                 "security_type", "financial_timestamp")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper date class columns"), {
  date_cols <- c("issue_date")
  expect_true(
    all(sapply(result[date_cols], class) == "Date")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper numeric class columns"), {
  num_cols <- c("company_id", "bloomberg_id", "unit_share_price",
                "market_value", "current_shares_outstanding",
                "current_shares_outstanding_all_classes")
  expect_true(
    all(sapply(result[num_cols], class) == "numeric")
  )
})
