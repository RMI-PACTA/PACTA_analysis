calculate_value_usd_with_fin_data <- function(portfolio) {

  # check correct inputs
  necessary_columns <- c("currency", "unit_share_price")

  ### TEST
  if (!any(necessary_columns %in% colnames(portfolio))) {
    stop("Portfolio not structured correctly")
  }


  # add missing currency for number of shares
  portfolio <- portfolio %>%
    mutate(currency = if_else(!is.na(number_of_shares), "USD", currency))

  # calculates the value_usd where number of shares are given
  portfolio <- portfolio %>%
    mutate(value_usd = if_else(
      asset_type %in% c("Equity", "Funds") & is.na(value_usd),
      number_of_shares * unit_share_price,
      value_usd
    ))

  portfolio
}
