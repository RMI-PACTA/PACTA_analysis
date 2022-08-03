calculate_fund_portfolio <- function(fund_portfolio, fund_data, cols_portfolio_no_bbg = cols_portfolio, cols_funds = cols_of_funds) {
  if (data_check(fund_portfolio)) {
    fund_portfolio <- left_join(fund_portfolio, fund_data, by = c("isin" = "fund_isin"), all.x = T)
    fund_portfolio$direct_holding <- FALSE

    fund_portfolio$original_value_usd <- fund_portfolio$value_usd
    fund_portfolio$value_usd <- fund_portfolio$isin_weight * fund_portfolio$value_usd
    fund_portfolio$fund_isin <- fund_portfolio$isin
    fund_portfolio$isin <- fund_portfolio$holding_isin

    # If there is no fund breakdown available, return the "original isin data" to the original locations
    fund_portfolio <- fund_portfolio %>%
      mutate(
        value_usd = if_else(!fund_isin %in% fund_data$fund_isin, original_value_usd, value_usd),
        isin = if_else(!fund_isin %in% fund_data$fund_isin, fund_isin, isin),
        direct_holding = if_else(!fund_isin %in% fund_data$fund_isin, TRUE, direct_holding),
      )
  } else {
    fund_portfolio <- fund_portfolio %>% bind_cols(data.frame(direct_holding = integer(0), fund_isin = character(0), original_value_usd = numeric(0)))
  }

  fund_portfolio <- fund_portfolio %>% select(all_of(cols_portfolio_no_bbg), all_of(cols_funds))

  fund_portfolio
}
