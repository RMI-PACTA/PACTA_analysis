portfolio <- tibble::tribble(
  ~isin,          ~market_value, ~currency, ~portfolio_name,       ~investor_name,
  "JP3868400007", 50000,         "GBP",     "TestPortfolio_Input", "Test",
  NA,             28136,         "GBP",     "TestPortfolio_Input", "Test"
)

fin_data <- tibble::tribble(
  ~isin,          ~company_id, ~bloomberg_id,
  "JP3868400007", 5643,        114116
)


test_that("`add_fin_data()` works as expected", {
  skip_if(nzchar(Sys.getenv("R_CMD")))

  source("../../0_portfolio_input_check_functions.R")

  result <- add_fin_data(portfolio, fin_data)
  expect_equal(result$isin, portfolio$isin)
  expect_equal(result$market_value, portfolio$market_value)
  expect_equal(result$currency, portfolio$currency)
  expect_equal(result$portfolio_name, portfolio$portfolio_name)
  expect_equal(result$investor_name, portfolio$investor_name)
  expect_equal(result$company_id, c(fin_data$company_id, NA))
  expect_equal(result$bloomberg_id, c(fin_data$bloomberg_id, NA))
})
