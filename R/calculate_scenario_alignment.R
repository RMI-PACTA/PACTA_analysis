calculate_scenario_alignment <- function(df) {
  browntechs <- c("Oil", "Gas", "Coal", "CoalCap", "GasCap", "OilCap", "ICE")

  df$trajectory_deviation <- (df$plan_alloc_wt_tech_prod - df$scen_alloc_wt_tech_prod) / df$scen_alloc_wt_tech_prod
  df$trajectory_deviation <- ifelse(df$scen_alloc_wt_tech_prod == 0, ifelse(df$plan_alloc_wt_tech_prod == 0, 0, -1), df$trajectory_deviation)

  df$trajectory_alignment <- ifelse(!df$technology %in% browntechs, 1 * df$trajectory_deviation, -1 * df$trajectory_deviation)

  df
}
