aggregate_company <- function(df) {

  ### actually data is already at the company level
  ### all we are doing here is getting sector totals
  if (data_check(df)) {
    df <- df %>%
      ungroup() %>%
      select(
        all_of(grouping_variables), scenario_source, scenario, allocation,
        id, company_name, financial_sector, port_weight,
        allocation_weight, plan_br_dist_alloc_wt, scen_br_dist_alloc_wt,
        equity_market, scenario_geography, year,
        ald_sector, technology,
        plan_tech_prod, plan_alloc_wt_tech_prod, plan_carsten, plan_emission_factor,
        scen_tech_prod, scen_alloc_wt_tech_prod, scen_carsten, scen_emission_factor
      ) %>%
      group_by(
        !!!rlang::syms(grouping_variables), scenario_source, scenario, allocation,
        id, company_name, financial_sector,
        allocation_weight, plan_br_dist_alloc_wt, scen_br_dist_alloc_wt,
        equity_market, scenario_geography, year,
        ald_sector
      ) %>%
      mutate(
        plan_sec_prod = sum(plan_tech_prod, na.rm = TRUE),
        plan_alloc_wt_sec_prod = sum(plan_alloc_wt_tech_prod, na.rm = TRUE),
        plan_sec_carsten = sum(plan_carsten, na.rm = TRUE),
        plan_sec_emissions_factor = weighted.mean(plan_emission_factor, plan_alloc_wt_tech_prod, na.rm = TRUE),
        scen_sec_prod = sum(scen_tech_prod, na.rm = TRUE),
        scen_alloc_wt_sec_prod = sum(scen_alloc_wt_tech_prod, na.rm = TRUE),
        scen_sec_carsten = ifelse(all(is.na(scen_carsten)), NA, sum(scen_carsten, na.rm = TRUE)), ### this is a random case where if all SCen.carsten are NA, it will total to zero, when I want it to be NA
        scen_sec_emissions_factor = weighted.mean(scen_emission_factor, scen_alloc_wt_tech_prod, na.rm = TRUE)
      ) %>%
      ungroup()
  } else {
    # TODO: check that this is a necessary solution, else just return df
    df <- data.frame()
  }


  return(df)
}
