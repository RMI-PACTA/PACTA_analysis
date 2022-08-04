get_ald_raw <- function(portfolio_type, supervisor_workflow = FALSE) {
  if (supervisor_workflow == TRUE) {
    filename_eq_raw <- "equity_ald_scenario_map.rds"
    filename_cb_raw <- "bonds_ald_scenario_map.rds"
  } else {
    filename_eq_raw <- "masterdata_ownership_datastore.rds"
    filename_cb_raw <- "masterdata_debt_datastore.rds"
  }

  if (portfolio_type == "Equity") {
    ald_raw <- read_rds(file.path(analysis_inputs_path, filename_eq_raw))
  }

  if (portfolio_type == "Bonds") {
    ald_raw <- read_rds(file.path(analysis_inputs_path, filename_cb_raw))
  }


  ald_raw <- ald_raw %>%
    filter(year %in% seq(start_year, start_year + time_horizon)) %>%
    filter(ald_sector %in% sector_list) %>%
    mutate(
      ald_sector = if_else(technology == "Coal", "Coal", ald_sector),
      ald_sector = if_else(technology %in% c("Oil", "Gas"), "Oil&Gas", ald_sector),
      ald_production = if_else(technology == "Gas" & grepl("GJ", ald_production_unit), ald_production * (1 / 0.0372), ald_production),
      ald_production = if_else(technology == "Oil" & grepl("GJ", ald_production_unit), ald_production * (1 / 6.12), ald_production),
      ald_production_unit = if_else(technology == "Gas" & grepl("GJ", ald_production_unit), "m3 per day", ald_production_unit),
      ald_production_unit = if_else(technology == "Oil" & grepl("GJ", ald_production_unit), "boe per day", ald_production_unit)
    )

  return(ald_raw)
}
