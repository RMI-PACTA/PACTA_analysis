calculate_with_weights <- function(df, weight_col_name, weight_method_name) {
  df[, "plan_br_dist_alloc_wt"] <- df[, weight_col_name] * df$plan_br_wt_factor
  df[, "plan_carsten"] <- df[, weight_col_name] * df$plan_br_wt_techshare
  df[, "scen_br_dist_alloc_wt"] <- df[, weight_col_name] * df$scen_br_wt_factor
  df[, "scen_carsten"] <- df[, weight_col_name] * df$scen_br_wt_techshare

  df[, "plan_alloc_wt_tech_prod"] <- df[, weight_col_name] * df$plan_tech_prod
  df[, "scen_alloc_wt_tech_prod"] <- df[, weight_col_name] * df$scen_tech_prod

  df[, "allocation"] <- weight_method_name
  df[, "allocation_weight"] <- df[, weight_col_name]
  df
}
