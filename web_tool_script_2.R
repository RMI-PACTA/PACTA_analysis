devtools::load_all(quiet = TRUE)
use_r_packages()

cli::cli_h1("web_tool_script_2.R{get_build_version_msg()}")

#########################################################################
# START RUN ANALYIS
#########################################################################

source("0_portfolio_test.R")
source("0_global_functions.R")
source("0_web_functions.R")

if (!exists("portfolio_name_ref_all")) { portfolio_name_ref_all <- "TestPortfolio_Input" }
if (!exists("portfolio_root_dir")) { portfolio_root_dir <- "working_dir" }

setup_project()

working_location <- file.path(working_location)

set_webtool_paths(portfolio_root_dir)

set_portfolio_parameters(file_path = fs::path(par_file_path, paste0(portfolio_name_ref_all, "_PortfolioParameters.yml")))

set_project_parameters(file.path(working_location, "parameter_files", paste0("ProjectParameters_", project_code, ".yml")))

# need to define an alternative location for data files
analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

# delete all results files within the current portfolio folder
unlink(file.path(results_path, portfolio_name_ref_all, "*"), force = TRUE, recursive = TRUE)

# run again so output folders are available after deleting past results
create_portfolio_subfolders(portfolio_name_ref_all)

port_col_types <- set_col_types(grouping_variables, "ddddccccddclc")


# quit if there's no relevant PACTA assets --------------------------------

total_portfolio_path <- file.path(proc_input_path, portfolio_name_ref_all, "total_portfolio.rda")
if (file.exists(total_portfolio_path)) {
  total_portfolio <- readRDS(total_portfolio_path)
  quit_if_no_pacta_relevant_data(total_portfolio)
} else {
  warning("This is weird... the `total_portfolio.rda` file does not exist in the `30_Processed_inputs` directory.")
}


##################
##### EQUITY #####
##################

equity_input_file <- file.path(proc_input_path, portfolio_name_ref_all, "equity_portfolio.rda")

if (file.exists(equity_input_file)) {
  ald_scen_eq <- get_ald_scen("Equity")
  ald_raw_eq <- get_ald_raw("Equity")

  port_raw_all_eq <- read_rds(equity_input_file) %>%
    mutate(id = as.character(id))

  if (length(colnames(port_raw_all_eq)) != nchar(port_col_types)) {
    stop("Check port_col_types: difference in length")
  }

  list_investors_eq <- unique(port_raw_all_eq$investor_name)

  for (e in 1:length(list_investors_eq)) {
    map_eq <- NA
    company_all_eq <- NA
    port_all_eq <- NA

    investor_name_select <- list_investors_eq[e]

    port_raw_eq <- port_raw_all_eq %>% filter(investor_name == investor_name_select)

    port_eq <- calculate_weights(port_raw_eq, "Equity", grouping_variables)

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

    pf_file_results_path <- file.path(results_path, portfolio_name_ref_all)
    if (!dir.exists(pf_file_results_path)) {
      dir.create(pf_file_results_path)
    }

    if (data_check(company_all_eq)) {
      write_rds(company_all_eq, file.path(pf_file_results_path, "Equity_results_company.rda"))
    }
    if (data_check(port_all_eq)) {

      if (tdm_conditions_met(analysis_inputs_path)) {
        tdm_vars <- determine_tdm_variables(start_year)

        equity_tdm <-
          calculate_tdm(
            port_all_eq,
            t0 = tdm_vars$t0,
            delta_t1 = tdm_vars$delta_t1,
            delta_t2 = tdm_vars$delta_t2,
            additional_groups = tdm_vars$additional_groups,
            scenarios = tdm_vars$scenarios
          )

        saveRDS(equity_tdm, file.path(pf_file_results_path, "Equity_tdm.rds"))
      }

      # filter out scenarios used only for TDM, if they exist
      if (data_includes_tdm_scenarios(analysis_inputs_path)) {
        port_all_eq <- filter(port_all_eq, ! scenario %in% tdm_scenarios())
      }

      saveRDS(port_all_eq, file.path(pf_file_results_path, "Equity_results_portfolio.rda"))
    }
    if (has_map) {
      if (data_check(map_eq)) {
        write_rds(map_eq, file.path(pf_file_results_path, "Equity_results_map.rda"))
      }
    }
  }
}


#################
##### BONDS #####
#################

bonds_inputs_file <- file.path(proc_input_path, portfolio_name_ref_all, "bonds_portfolio.rda")
# portfolio_name <- file_names$portfolio_name

if (file.exists(bonds_inputs_file)) {

  ald_scen_cb <- get_ald_scen("Bonds")
  ald_raw_cb <- get_ald_raw("Bonds")

  port_raw_all_cb <- read_rds(bonds_inputs_file) %>%
    mutate(id = as.character(id))

  if (length(colnames(port_raw_all_cb)) != nchar(port_col_types)) {
    stop("Check port_col_types: difference in length")
  }

  list_investors_cb <- unique(port_raw_all_cb$investor_name)

  for (b in 1:length(list_investors_cb)) {
    map_cb <- NA
    company_all_cb <- NA
    port_all_cb <- NA

    investor_name_select <- list_investors_cb[b]

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
        map_cb <- merge_in_geography(company_all_cb, ald_raw_cb)

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

    pf_file_results_path <- file.path(results_path, portfolio_name_ref_all)
    if (!dir.exists(pf_file_results_path)) {
      dir.create(pf_file_results_path)
    }

    if (data_check(company_all_cb)) {
      write_rds(company_all_cb, file.path(pf_file_results_path, "Bonds_results_company.rda"))
    }
    if (data_check(port_all_cb)) {
      if (tdm_conditions_met(analysis_inputs_path)) {
        tdm_vars <- determine_tdm_variables(start_year)

        bonds_tdm <-
          calculate_tdm(
            port_all_cb,
            t0 = tdm_vars$t0,
            delta_t1 = tdm_vars$delta_t1,
            delta_t2 = tdm_vars$delta_t2,
            additional_groups = tdm_vars$additional_groups,
            scenarios = tdm_vars$scenarios
          )

        saveRDS(bonds_tdm, file.path(pf_file_results_path, "Bonds_tdm.rds"))
      }

      # filter out scenarios used only for TDM, if they exist
      if (data_includes_tdm_scenarios(analysis_inputs_path)) {
        port_all_cb <- filter(port_all_cb, ! scenario %in% tdm_scenarios())
      }

      saveRDS(port_all_cb, file.path(pf_file_results_path, "Bonds_results_portfolio.rda"))
    }
    if (has_map) {
      if (data_check(map_cb)) {
        write_rds(map_cb, file.path(pf_file_results_path, "Bonds_results_map.rda"))
      }
    }
  }
}


remove_if_exists(port_raw_all_eq)
remove_if_exists(port_raw_all_cb)
remove_if_exists(port_raw_eq)
remove_if_exists(port_raw_cb)
remove_if_exists(ald_scen_eq)
remove_if_exists(ald_scen_cb)
remove_if_exists(company_all_eq)
remove_if_exists(company_all_cb)
remove_if_exists(port_eq)
remove_if_exists(port_cb)
remove_if_exists(company_own_eq)
