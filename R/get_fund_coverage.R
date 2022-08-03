get_fund_coverage <- function(portfolio_raw,
                              fin_data,
                              fund_data,
                              currencies,
                              grouping_variables) {
  portfolio <- clean_colnames_portfolio_input_file(portfolio_raw)

  portfolio <- clear_portfolio_input_blanks(portfolio)

  portfolio <- add_meta_portfolio(portfolio, inc_meta_portfolio)

  start_port_rows <- nrow(portfolio)

  portfolio <- add_holding_id(portfolio)

  portfolio <- check_missing_cols(portfolio, grouping_variables)

  portfolio <- clean_portfolio_col_types(portfolio, grouping_variables)

  portfolio <- convert_currencies(portfolio, currencies)

  cols_portfolio <- colnames(portfolio)

  cols_of_funds <- c("direct_holding", "fund_isin", "original_value_usd")

  # Add financial data
  # Merges in the clean data and calculates the marketvalue and number of shares
  portfolio <- add_fin_data(portfolio, fin_data)

  if (nrow(portfolio) != start_port_rows) {
    stop("Portfolio lines changing unexpectedly")
  }

  portfolio <- calculate_value_usd_with_fin_data(portfolio)

  portfolio <- calculate_number_of_shares(portfolio)

  original_value_usd <- sum(portfolio$value_usd, na.rm = TRUE)

  # identify funds in the portfolio
  fund_portfolio <- identify_fund_portfolio(portfolio)

  # the raw portfolio will be compared with the merged portfolio
  fund_portfolio_raw <- fund_portfolio

  if (data_check(fund_data)) {
    # Creates the fund_portfolio to match the original portfolio
    fund_portfolio <- calculate_fund_portfolio(fund_portfolio, fund_data, cols_portfolio, cols_of_funds)

    # Merges in the bbg data to the fund portfolio
    fund_portfolio <- add_fin_data(fund_portfolio, fin_data)

    # add fund_portfolio and check that the total value is the same
    portfolio_total <- add_fund_portfolio(portfolio, fund_portfolio, cols_of_funds)
  } else {
    portfolio_total <- as_tibble(portfolio)
    portfolio_total$direct_holding <- TRUE
    portfolio_total$fund_isin <- NA
  }


  fund_portfolio_total <- portfolio_total %>% filter(!is.na(fund_isin))

  fund_portfolio_total_mapped_value_usd <- fund_portfolio_total  %>%
    group_by(holding_id) %>%
    summarize(total_mapped_value_usd = sum(value_usd), .groups = "drop")


  fund_portfolio_missing_value_usd <- fund_portfolio_total  %>%
    filter(nchar(isin)!=12) %>%
    group_by(holding_id) %>%
    summarize(missing_value_usd = sum(value_usd), .groups = "drop")


  fund_portfolio_funds_in_funds_not_mapped_value_usd <-  fund_portfolio_total  %>%
    filter(nchar(isin)==12 & asset_type == "Funds") %>%
    group_by(holding_id) %>%
    summarize(funds_in_funds_not_mapped = sum(value_usd), .groups = "drop")



  fund_portfolio <- fund_portfolio_raw
  fund_portfolio <- left_join(fund_portfolio, fund_portfolio_total_mapped_value_usd, by = "holding_id")
  fund_portfolio$total_mapped_value_usd[is.na(fund_portfolio$total_mapped_value_usd)] <- 0


  fund_portfolio <- left_join(fund_portfolio, fund_portfolio_missing_value_usd, by = "holding_id")
  fund_portfolio$missing_value_usd[is.na(fund_portfolio$missing_value_usd)] <- 0

  fund_portfolio <- left_join(fund_portfolio, fund_portfolio_funds_in_funds_not_mapped_value_usd, by = "holding_id")
  fund_portfolio$funds_in_funds_not_mapped[is.na(fund_portfolio$funds_in_funds_not_mapped)] <- 0



  fund_portfolio <- fund_portfolio %>% mutate(effective_coverage = (total_mapped_value_usd - missing_value_usd - funds_in_funds_not_mapped) / value_usd)
  fund_portfolio <- fund_portfolio %>% mutate(fund_data_file_coverage = (total_mapped_value_usd) / value_usd)
  fund_portfolio <- fund_portfolio %>% mutate(lipper_data_coverage = (total_mapped_value_usd - missing_value_usd) / total_mapped_value_usd)
  fund_portfolio <- fund_portfolio %>% mutate(lost_coverage_fif = 1 - (total_mapped_value_usd - missing_value_usd - funds_in_funds_not_mapped) / (total_mapped_value_usd-missing_value_usd))





  return(fund_portfolio)
}
