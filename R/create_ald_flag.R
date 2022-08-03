create_ald_flag <- function(portfolio, comp_fin_data, debt_fin_data) {
  portfolio_eq <- portfolio %>% filter(asset_type == "Equity")
  portfolio_cb <- portfolio %>% filter(asset_type == "Bonds")
  portfolio_other <- portfolio %>% filter(!asset_type %in% c("Equity", "Bonds"))

  portfolio_eq <- check_for_ald(portfolio_eq, "Equity", comp_fin_data)
  portfolio_cb <- check_for_ald(portfolio_cb, "Bonds", debt_fin_data)

  if (data_check(portfolio_other)) {
    portfolio_other <- portfolio_other %>% mutate(
      has_asset_level_data = NA,
      sectors_with_assets = NA,
      has_ald_in_fin_sector = NA
    )
  } else {
    portfolio_other <- portfolio_other %>% add_column("has_asset_level_data", "sectors_with_assets", "has_ald_in_fin_sector")
  }

  rbind(portfolio_eq, portfolio_cb, portfolio_other)
}
