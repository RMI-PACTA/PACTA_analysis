identify_fund_portfolio <- function(portfolio) {
  fund_portfolio <- portfolio %>% filter(asset_type == "Funds", !is.na(isin))

  fund_portfolio
}
