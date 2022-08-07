add_portfolio_flags <- function(portfolio) {
  ### FLAGS/Exclusions

  portfolio <- check_isin_format(portfolio)
  portfolio <- check_missing_currency(portfolio)
  portfolio <- check_valid_input_value(portfolio)
  portfolio <- check_financial_data(portfolio)

  portfolio <- add_flags(portfolio)
  portfolio <- overall_validity_flag(portfolio)

  return(portfolio)
}
