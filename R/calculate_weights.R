calculate_weights <- function(portfolio, portfolio_type, grouping_variables) {
  port_sub <- portfolio %>%
    select(
      all_of(grouping_variables), holding_id, id = company_id, id_name = "company_id", company_name, value_usd, number_of_shares,
      current_shares_outstanding_all_classes, financial_sector, has_ald_in_fin_sector
    ) %>%
    mutate(id = as.character(id))

  port_sub <- calculate_port_weight(port_sub, grouping_variables)

  port_sub <- aggregate_holdings(port_sub)


  if (portfolio_type == "Equity") {
    port_sub <- calculate_ownership_weight(port_sub)
  }


  return(port_sub)
}
