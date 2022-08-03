aggregate_map_data <- function(portfolio) {
  portfolio <- portfolio %>%
    ungroup() %>%
    group_by(
      !!!rlang::syms(grouping_variables), # allocation,
      ald_location, year,
      ald_sector, technology,
      financial_sector, allocation, allocation_weight, ald_production_unit
    ) %>%
    summarise(plan_alloc_wt_tech_prod = sum(plan_alloc_wt_tech_prod, na.rm = TRUE),
              .groups = "drop_last") %>%
    mutate(plan_alloc_wt_sec_prod = sum(plan_alloc_wt_tech_prod))

  if (data_check(portfolio)) {
    portfolio$equity_market <- "Global"
    portfolio$scenario <- NA # this is current plans only
    portfolio$scenario_geography <- NA
  }

  return(portfolio)
}
