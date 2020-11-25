test_that("get_security_financial() function exists", {
  expect_true(
    class(get_security_financial) == "function"
  )
})

test_that("get_security_financial() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_", "", deparse(substitute(get_security_financial))) %in% data_object_names
  )
})

test_that("get_security_financial() returns a data.frame", {
  skip_check_but_run_test()
  result <- get_security_financial(here::here("inst/extdata"))
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_security_financial() returns a data.frame with 19 columns", {
  skip_check_but_run_test()
  result <- get_security_financial(here::here("inst/extdata"))
  expect_true(
    ncol(result) == 19
  )
})

test_that("get_security_financial() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  result <- get_security_financial(here::here("inst/extdata"))
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

test_that("get_security_financial() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  result <- get_security_financial(here::here("inst/extdata"))
  char_cols <- c("company_name", "country_of_domicile", "ticker",
                 "corporate_bond_ticker", "isin", "asset_type",
                 "security_icb_subsector", "security_bics_subgroup",
                 "security_bclass4", "security_mapped_sector",
                 "security_type", "financial_timestamp")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})

test_that("get_security_financial() returns a data.frame with proper date class columns", {
  skip_check_but_run_test()
  result <- get_security_financial(here::here("inst/extdata"))
  date_cols <- c("issue_date")
  expect_true(
    all(sapply(result[date_cols], class) == "Date")
  )
})

test_that("get_security_financial() returns a data.frame with proper numeric class columns", {
  skip_check_but_run_test()
  result <- get_security_financial(here::here("inst/extdata"))
  num_cols <- c("company_id", "bloomberg_id", "unit_share_price",
                "market_value", "current_shares_outstanding",
                "current_shares_outstanding_all_classes")
  expect_true(
    all(sapply(result[num_cols], class) == "numeric")
  )
})
