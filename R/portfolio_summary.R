portfolio_summary <- function(portfolio_total) {
  overview_data <- portfolio_total %>%
    ungroup() %>%
    group_by(!!!rlang::syms(grouping_variables), asset_type, financial_sector, valid_input) %>%
    mutate(valid_value_usd = sum(value_usd, na.rm = T)) %>%
    ungroup() %>%
    group_by(!!!rlang::syms(grouping_variables), asset_type, valid_input) %>%
    mutate(asset_value_usd = sum(value_usd, na.rm = T)) %>%
    ungroup() %>%
    group_by(!!!rlang::syms(grouping_variables), valid_input) %>%
    mutate(portfolio_value_usd = sum(value_usd, na.rm = T)) %>%
    ungroup() %>%
    select(
      !!!rlang::syms(grouping_variables), asset_type, financial_sector, valid_input,
      valid_value_usd, asset_value_usd, portfolio_value_usd
    ) %>%
    distinct()

  return(overview_data)
}
