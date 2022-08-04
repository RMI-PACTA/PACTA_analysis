get_ald_scen <- function(portfolio_type) {
  if (portfolio_type == "Equity") {
    ald <- read_rds(file.path(analysis_inputs_path, "equity_abcd_scenario.rds"))
    ald <- ald %>%
      filter(equity_market %in% equity_market_list)

    if (data_check(ald) == FALSE) {
      stop(" equity market list filtered out all ald_eq")
    }
  }
  if (portfolio_type == "Bonds") {
    ald <- read_rds(file.path(analysis_inputs_path, "bonds_abcd_scenario.rds"))
  }

  ald <- ald %>%
    filter(
      scenario_source %in% scenario_sources_list,
      scenario_geography %in% scenario_geographies_list,
      ald_sector %in% sector_list
    ) %>%
    mutate(mapped_ald = 1)



  return(ald)
}
