test_that("`clean_unmatched_holdings()` works as expected", {
  skip_if(nzchar(Sys.getenv("R_CMD")), "In R CMD check")
  # replace with skip_if_R_CMD_check() when #547 merges

  portfolio <- tibble::tribble(
    ~isin,          ~asset_type, ~security_mapped_sector,
    "JP3868400007", "Equity",    "Power",
    "US4533863451", "Equity",    "Coal",
    "DE3903824093", "Equity",    "",
    "FR9898439002", "Equity",    NA_character_,
    "JP3868400007", "Bond",      "Power",
    "US4533863451", "Bond",      "Coal",
    "DE3903824093", "Bond",      "",
    "FR9898439002", "Bond",      NA_character_
  )

  source(here::here("0_global_functions.R"))  # for data_check()
  source(here::here("0_portfolio_input_check_functions.R"))

  # test that clean_unmatched_holdings() does not add any rows, that the number
  # of `security_mapped_sector` values that are NA in the result is equal to the
  # number of `security_mapped_sector` values that are NA in the portfolio, and
  # the that number of `asset_type` values that are NA in the result is equal or
  # greater than (because it may have had additional NAs on different rows than
  # in `security_mapped_sector` before the cleanup) to the number of
  # `security_mapped_sector` values that are NA in the portfolio
  result <- clean_unmatched_holdings(portfolio)
  expect_equal(nrow(result), nrow(portfolio))
  expect_equal(
    sum(result$security_mapped_sector == "Unclassifiable"),
    sum(is.na(portfolio$security_mapped_sector))
  )
  expect_gte(
    sum(result$asset_type == "Unclassifiable"),
    sum(is.na(portfolio$security_mapped_sector))
  )
})
