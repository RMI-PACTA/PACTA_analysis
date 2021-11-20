tdm_scenarios <- function() {
  c("IPR2021_IPR FPS 2021")
}

analysis_inputs_with_tdm_scenarios <- function() {
  c("../pacta-data/2020Q4")
}

data_includes_tdm_scenarios <- function() {
  analysis_inputs_path %in% analysis_inputs_with_tdm_scenarios()
}

tdm_conditions_met <- function () {
  project_code == "GENERAL" && data_includes_tdm_scenarios()
}

determine_tdm_variables <- function () {
  list(
    t0 = start_year,
    delta_t1 = 5,
    delta_t2 = 9,
    additional_groups = c("investor_name", "portfolio_name", "scenario_source", "scenario", "allocation", "equity_market", "scenario_geography"),
    scenarios = tdm_scenarios()
  )
}
