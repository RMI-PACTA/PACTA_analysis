# TODO:
# Clean up sectors options

port_col_types <- set_col_types(grouping_variables, "ddddccccddcl")
##################
##### EQUITY #####
##################

equity_input_file <- paste0(proc_input_path, "/", project_name, "_equity_portfolio.rda")

if (file.exists(equity_input_file)) {
  port_raw_all_eq <- read_rds(equity_input_file) %>%
    mutate(id = as.character(id))

  if (length(colnames(port_raw_all_eq)) != nchar(port_col_types)) {
    stop("Check port_col_types: difference in length")
  }

  ald_scen_eq <- get_ald_scen("Equity")

  ald_raw_eq <- get_ald_raw("Equity")

  list_investors_eq <- unique(port_raw_all_eq$investor_name)

  for (e in 1:length(list_investors_eq)) {
    map_eq <- NA
    company_all_eq <- NA
    port_all_eq <- NA

    investor_name_select <- list_investors_eq[e]
    print(paste0(e, ": ", investor_name_select))

    port_raw_eq <- port_raw_all_eq %>% filter(investor_name == investor_name_select)

    port_eq <- calculate_weights(port_raw_eq, "Equity", grouping_variables)

    port_eq <- port_eq %>% filter(port_weight > 1e-6)

    port_eq <- merge_in_ald(port_eq, ald_scen_eq)

    # Portfolio weight methodology
    port_pw_eq <- port_weight_allocation(port_eq)

    company_pw_eq <- aggregate_company(port_pw_eq)

    port_pw_eq <- aggregate_portfolio(company_pw_eq)

    # Ownership weight methodology
    port_own_eq <- ownership_allocation(port_eq)

    company_own_eq <- aggregate_company(port_own_eq)

    port_own_eq <- aggregate_portfolio(company_own_eq)

    # Create combined outputs
    company_all_eq <- bind_rows(company_pw_eq, company_own_eq)

    port_all_eq <- bind_rows(port_pw_eq, port_own_eq)

    if (has_map) {
      map_eq <- merge_in_geography(company_all_eq, ald_raw_eq)

      map_eq <- aggregate_map_data(map_eq)
    }

    # Technology Share Calculation
    port_all_eq <- calculate_technology_share(port_all_eq)

    company_all_eq <- calculate_technology_share(company_all_eq)

    # Scenario alignment calculations
    port_all_eq <- calculate_scenario_alignment(port_all_eq)

    company_all_eq <- calculate_scenario_alignment(company_all_eq)

    investor_results_path <- paste0(results_path, "/", investor_name_select, "/")
    if (!dir.exists(investor_results_path)) {
      dir.create(investor_results_path)
    }

    if (data_check(company_all_eq)) {
      write_rds(company_all_eq, paste0(investor_results_path, "Equity_results_company.rda"))
    }
    if (data_check(port_all_eq)) {
      write_rds(port_all_eq, paste0(investor_results_path, "Equity_results_portfolio.rda"))
    }
    if (has_map) {
      if (data_check(map_eq)) {
        write_rds(map_eq, paste0(investor_results_path, "Equity_results_map.rda"))
      }
    }
  }
}

#################
##### BONDS #####
#################

bonds_inputs_file <- paste0(proc_input_path, "/", project_name, "_bonds_portfolio.rda")

if (file.exists(bonds_inputs_file)) {
  port_raw_all_cb <- read_rds(bonds_inputs_file) %>%
    mutate(id = as.character(id))

  if (length(colnames(port_raw_all_cb)) != nchar(port_col_types)) {
    stop("Check port_col_types: difference in length")
  }

  ald_scen_cb <- get_ald_scen("Bonds")

  ald_raw_cb <- get_ald_raw("Bonds")

  list_investors_cb <- unique(port_raw_all_cb$investor_name)

  for (b in 1:length(list_investors_cb)) {
    map_cb <- NA
    company_all_cb <- NA
    port_all_cb <- NA

    investor_name_select <- list_investors_cb[b]

    print(paste0(b, ": ", investor_name_select))

    port_raw_cb <- port_raw_all_cb %>% filter(investor_name == investor_name_select)

    port_cb <- calculate_weights(port_raw_cb, "Bonds", grouping_variables)

    port_cb <- merge_in_ald(port_cb, ald_scen_cb)

    # Portfolio weight methodology
    port_pw_cb <- port_weight_allocation(port_cb)

    company_pw_cb <- aggregate_company(port_pw_cb)

    port_pw_cb <- aggregate_portfolio(company_pw_cb)

    # Create combined outputs
    company_all_cb <- company_pw_cb

    port_all_cb <- port_pw_cb

    if (has_map) {
      if (data_check(company_all_cb)) {
        map_cb <- merge_in_geography(
          portfolio = company_all_cb,
          ald_raw = ald_raw_cb
        )

        map_cb <- aggregate_map_data(map_cb)
      }
    }

    # Technology Share Calculation
    if (nrow(port_all_cb) > 0) {
      port_all_cb <- calculate_technology_share(port_all_cb)
    }

    if (nrow(company_all_cb) > 0) {
      company_all_cb <- calculate_technology_share(company_all_cb)
    }

    # Scenario alignment calculations
    port_all_cb <- calculate_scenario_alignment(port_all_cb)

    company_all_cb <- calculate_scenario_alignment(company_all_cb)

    investor_results_path <- paste0(results_path, "/", investor_name_select, "/")
    if (!dir.exists(investor_results_path)) {
      dir.create(investor_results_path)
    }

    if (data_check(company_all_cb)) {
      write_rds(company_all_cb, paste0(investor_results_path, "Bonds_results_company.rda"))
    }
    if (data_check(port_all_cb)) {
      write_rds(port_all_cb, paste0(investor_results_path, "Bonds_results_portfolio.rda"))
    }
    if (has_map) {
      if (data_check(map_cb)) {
        write_rds(map_cb, paste0(investor_results_path, "Bonds_results_map.rda"))
      }
    }
  }
}


#####################
##### AGGREGATE #####
#####################

gather_and_save_project_results(results_path, aggregation_level = "portfolio")
gather_and_save_project_results(results_path,
  aggregation_level = "company",
  year_filter = c(start_year, start_year + 5),
  allocation_filter = "portfolio_weight",
  portfolios_per_file = 250
)
