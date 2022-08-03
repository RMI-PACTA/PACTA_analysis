calculate_technology_share <- function(df) {
  df <- df %>%
    ungroup() %>%
    mutate(
      plan_tech_share = plan_alloc_wt_tech_prod / plan_alloc_wt_sec_prod,
      scen_tech_share = scen_alloc_wt_tech_prod / scen_alloc_wt_sec_prod
    )
  df
}
