test_that("`set_portfolio_parameters()` works as expected", {
  skip_if_R_CMD_check()

  source(here::here("0_portfolio_input_check_functions.R"))

  portfolio <- tibble::tribble(
    ~isin,          ~market_value, ~currency, ~portfolio_name,       ~investor_name,
    "JP3868400007", 50000,         "GBP",     "TestPortfolio_Input", "Test",
    "",             35184,         "GBP",     "TestPortfolio_Input", "Test",
    NA,             28136,         "GBP",     "TestPortfolio_Input", "Test"
  )

  fin_data <- tibble::tribble(
    ~isin,          ~company_id, ~bloomberg_id,
    "JP3868400007", 5643,        114116
  )

  # test that when portfolio data that has some unmatchable ISINs and it is
  # merged with fin data that add_fin_data() returns a data frame with the same
  # number of rows as the original portfolio data and that all the data stays
  # the same except for the new columns merged from the fin data; unmatched rows
  # should have appropriate NA values in the newly merged columns
  result <- add_fin_data(portfolio, fin_data)
  expect_equal(nrow(portfolio), nrow(result))
  expect_equal(result$isin, portfolio$isin)
  expect_equal(result$market_value, portfolio$market_value)
  expect_equal(result$currency, portfolio$currency)
  expect_equal(result$portfolio_name, portfolio$portfolio_name)
  expect_equal(result$investor_name, portfolio$investor_name)
  expect_equal(result$company_id, c(fin_data$company_id, NA_real_, NA_real_))
  expect_equal(result$bloomberg_id, c(fin_data$bloomberg_id, NA_real_, NA_real_))
})
