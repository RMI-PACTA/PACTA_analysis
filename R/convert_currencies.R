convert_currencies <- function(portfolio, currencies) {
  portfolio <- left_join(portfolio, currencies, by = "currency")

  portfolio$value_usd <- portfolio$market_value * portfolio$exchange_rate

  portfolio
}
