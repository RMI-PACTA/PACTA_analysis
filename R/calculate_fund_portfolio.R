calculate_fund_portfolio <- function(fund_portfolio, fund_data, cols_portfolio_no_bbg = cols_portfolio, cols_funds = cols_of_funds) {
  if (data_check(fund_portfolio)) {
    fund_portfolio <- left_join(fund_portfolio, fund_data, by = "factset_fund_id")
    fund_portfolio$direct_holding <- FALSE

    fund_portfolio$original_value_usd <- fund_portfolio$value_usd
    fund_portfolio$fund_holding_weight <- fund_portfolio$holding_reported_mv / fund_portfolio$total_reported_mv
    fund_portfolio$value_usd <- fund_portfolio$fund_holding_weight * fund_portfolio$value_usd
    fund_portfolio$fund_isin <- fund_portfolio$isin
    fund_portfolio$isin <- fund_portfolio$holding_isin

    # FIXME - I don't think this is necessary anymore because I can't think of a
    #        reaon why FactSet would have a fund ISIN that doesn't have holdings
    # # If there is no fund breakdown available, return the "original isin data" to the original locations
    # fund_portfolio <- fund_portfolio %>%
    #   mutate(
    #     value_usd = if_else(!fund_isin %in% fund_data$fund_isin, original_value_usd, value_usd),
    #     isin = if_else(!fund_isin %in% fund_data$fund_isin, fund_isin, isin),
    #     direct_holding = if_else(!fund_isin %in% fund_data$fund_isin, TRUE, direct_holding),
    #   )
  } else {
    fund_portfolio <- fund_portfolio %>% bind_cols(data.frame(direct_holding = integer(0), fund_isin = character(0), original_value_usd = numeric(0)))
  }

  fund_portfolio <- fund_portfolio %>% select(all_of(cols_portfolio_no_bbg), all_of(cols_funds))

  fund_portfolio
}
