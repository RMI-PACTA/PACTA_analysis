check_for_ald <- function(portfolio_subset, portfolio_type, relevant_fin_data) {
  if (data_check(portfolio_subset)) {
    initial_port_value <- sum(portfolio_subset$value_usd, na.rm = T)

    if (portfolio_type == "Equity") {
      joining_id <- "company_id"
    } else if (portfolio_type == "Bonds") {
      joining_id <- "company_id"
    }

    ald_markers <- relevant_fin_data %>%
      select(all_of(joining_id), has_asset_level_data, sectors_with_assets) %>%
      distinct()

    portfolio_subset <- left_join(portfolio_subset, ald_markers, by = joining_id)

    portfolio_subset <- portfolio_subset %>%
      rowwise() %>%
      mutate(has_ald_in_fin_sector = if_else(grepl(financial_sector, sectors_with_assets), TRUE, FALSE)) %>%
      ungroup()

    if (sum(portfolio_subset$value_usd, na.rm = T) != initial_port_value) {
      stop("Merge over company id changes portfolio value")
    }
  } else {
    portfolio_subset <- portfolio_subset %>% add_column("has_asset_level_data", "sectors_with_assets", "has_ald_in_fin_sector")
  }
  return(portfolio_subset)
}
