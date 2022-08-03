add_fund_portfolio <- function(portfolio, fund_portfolio, cols_of_funds) {

  # Remove the fund lines from the portfolio
  portfolio_no_funds <- portfolio %>% filter(!isin %in% fund_portfolio$fund_isin)

  # Check that there are the correct number of isins in both portfolios
  if (nrow(portfolio_no_funds) + length(unique(fund_portfolio$holding_id)) != nrow(portfolio)) {
    stop("Something unexpected with fund portfolio merge")
  }

  # Add additional fund relevant lines to original portfolio
  portfolio_no_funds <- portfolio_no_funds %>%
    mutate(
      direct_holding = TRUE,
      fund_isin = NA,
      original_value_usd = value_usd
    )

  # select same columns for both portfolios
  portfolio_no_funds <- portfolio_no_funds %>% select(colnames(portfolio), all_of(cols_of_funds))
  fund_portfolio <- fund_portfolio %>% select(colnames(portfolio), all_of(cols_of_funds))

  if (!identical(colnames(portfolio_no_funds), colnames(fund_portfolio))) {
    stop("Colnames not equal, funds vs no funds")
  }

  # Merge in the results

  portfolio_total <- rbind(portfolio_no_funds, fund_portfolio)

  portfolio_total <- as_tibble(portfolio_total)

  portfolio_total
}
