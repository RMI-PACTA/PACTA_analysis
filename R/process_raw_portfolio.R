process_raw_portfolio <- function(portfolio_raw,
                                  fin_data,
                                  fund_data,
                                  currencies,
                                  grouping_variables,
                                  total_fund_list = NULL) {
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

  original_value_usd <- sum(portfolio$value_usd, na.rm = T)

  # correct Funds classification by comparing isin to the list of all known funds isins
  if (!is.null(total_fund_list)) {
    portfolio <- portfolio %>%
      mutate(asset_type = ifelse(
        is.element(isin, total_fund_list$fund_isin), "Funds", asset_type
      ))
  }
  # identify fund in the portfolio
  fund_portfolio <- identify_fund_portfolio(portfolio)

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
  }

  portfolio_total <- clean_unmatched_holdings(portfolio_total)

  if (!all.equal(sum(portfolio_total$value_usd, na.rm = TRUE), original_value_usd, tolerance = 1e-3)) {
    stop("Fund Portfolio introducing errors in total value")
  }


  ### TODO
  # summarise fund results
  # identify missing funds and isins
  ###

  return(portfolio_total)
}
