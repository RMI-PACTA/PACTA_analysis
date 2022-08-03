ownership_allocation <- function(portfolio) {

  # Only for equity portfolios

  port_ald_own <- calculate_with_weights(portfolio, "ownership_weight", "ownership_weight")

  port_ald_own <- port_ald_own %>% mutate(
    plan_carsten = NA,
    scen_carsten = NA
  )

  return(port_ald_own)
}
