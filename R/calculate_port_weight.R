calculate_port_weight <- function(portfolio, grouping_variables) {

  portfolio <- portfolio %>%
    ungroup() %>%
    group_by(!!!rlang::syms(grouping_variables)) %>%
    mutate(
      port_total_aum = sum(value_usd, na.rm = T),
      port_weight = value_usd / port_total_aum
    )

  portfolio
}
