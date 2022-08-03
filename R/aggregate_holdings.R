aggregate_holdings <- function(portfolio) {
  portfolio <- portfolio %>%
    ungroup() %>%
    # group_by(vars(all_of(grouping_variables))) %>%
    # group_by(holding_id, id, financial_sector, add = T) %>%
    group_by(!!!rlang::syms(grouping_variables), company_name, id, financial_sector, current_shares_outstanding_all_classes, has_ald_in_fin_sector) %>%
    summarise(
      number_holdings = n_distinct(holding_id),
      value_usd = sum(value_usd, na.rm = T),
      number_of_shares = sum(number_of_shares, na.rm = T),
      port_weight = sum(port_weight, na.rm = TRUE),
      .groups = "drop_last"
    )

  return(portfolio)
}
