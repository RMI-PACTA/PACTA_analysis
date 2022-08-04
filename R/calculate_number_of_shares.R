calculate_number_of_shares <- function(portfolio) {
  portfolio <- portfolio %>%
    mutate(number_of_shares = ifelse(is.na(number_of_shares) & asset_type == "Equity", value_usd / unit_share_price, number_of_shares))

  return(portfolio)
}
