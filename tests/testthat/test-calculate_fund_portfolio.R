test_that("`calculate_fund_portfolio()` works as expected", {
  skip_if_R_CMD_check()

  fund_portfolio <-
    tibble::tribble(
      ~isin,          ~market_value, ~currency, ~portfolio_name, ~investor_name, ~holding_id, ~number_of_shares, ~exchange_rate, ~value_usd, ~company_id, ~company_name,                   ~bloomberg_id, ~corporate_bond_ticker, ~country_of_domicile, ~unit_share_price, ~exchange_rate_usd, ~asset_type, ~security_type,   ~security_mapped_sector, ~security_icb_subsector, ~security_bics_subgroup, ~bics_sector,  ~maturity_date, ~coupon_value, ~amount_issued, ~current_shares_outstanding_all_classes, ~sector_override, ~sector_boe, ~subsector_boe, ~sector_dnb, ~sector_ipr,   ~subsector_ipr, ~is_sb,
      "IE00B9346255", 500,           "USD",     "TestPortfolio", "Test",         "58",        NA_real_,          1,              500,        28581,       "Institutional Cash Series Plc", 34841267,      NA_character_,          "IE",                 121.4076252,       1.2253,             "Funds",     "Open-End Funds", "Other",                 NA_character_,           "Unclassified",          NA_character_, NA_character_,  NA_real_,      NA_real_,       NA_real_,                                FALSE,            "Other",     NA_character_,  "Other",     NA_character_, NA_character_,  FALSE
    )
  fund_data <-
    tibble::tribble(
      ~fund_isin,     ~holding_isin,  ~isin_weight,
      "IE00B9346255", "XS2260177394", 0.2,
      "IE00B9346255", "XS2260147306", 0.2,
      "IE00B9346255", "XS2264058442", 0.2,
      "IE00B9346255", "XS2275622269", 0.2,
      "IE00B9346255", "XS2266971444", 0.2
    )
  cols_portfolio <- c("isin", "market_value", "currency", "portfolio_name", "investor_name", "holding_id", "number_of_shares", "exchange_rate", "value_usd")
  cols_of_funds <- c("direct_holding", "fund_isin", "original_value_usd")

  result <- calculate_fund_portfolio(fund_portfolio, fund_data, cols_portfolio_no_bbg = cols_portfolio, cols_funds = cols_of_funds)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5L)
  expect_equal(sum(result$value_usd), sum(fund_portfolio$value_usd))
})
