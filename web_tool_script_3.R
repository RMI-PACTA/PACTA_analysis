devtools::load_all()
use_r_packages()

source("0_global_functions.R")
source("0_web_functions.R")

setup_project()

working_location <- file.path(working_location)

set_webtool_paths()

options(r2dii_config = file.path(par_file_path, "AnalysisParameters.yml"))

set_global_parameters(file.path(par_file_path, "AnalysisParameters.yml"))

set_portfolio_parameters(file_path = fs::path(par_file_path, paste0(portfolio_name_ref_all, "_PortfolioParameters.yml")))

# need to define an alternative location for data files
analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

source(file.path(template_path, "create_interactive_report.R"))
source(file.path(template_path, "create_executive_summary.R"))
source(file.path(template_path, "useful_functions.R"))

# file_names <- read_csv(file.path(proc_input_path, portfolio_name_ref_all, "file_names.csv"))

repo_path <- template_path
template_name <- paste0("template_", tolower(language_select))
template_dir <- fs::path(template_path, template_name)
template_dir <- paste0(repo_path,"swiss_en_template/_book/")

company_charts_dir <- fs::path(template_path, "company_charts", "Mixed_Portfolio")
output_dir <- file.path(outputs_path, portfolio_name_ref_all)
project_name <- "working_dir"
scenario <- "B2DS"
portfolio_allocation_method <- "portfolio_weight"
scenario_geography <- "Global"

audit_file <- read_csv(file.path(proc_input_path, portfolio_name, "audit_file.csv"))
emissions <- read_rds(file.path(proc_input_path, portfolio_name, "emissions.rda"))

if (file.exists(file.path(results_path, portfolio_name, "Equity_results_portfolio.rda"))) {
  equity_results_portfolio <- read_rds(file.path(results_path, portfolio_name, "Equity_results_portfolio.rda"))
} else {
  equity_results_portfolio <- tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "scenario" = NA_character_, "allocation" = NA_character_,
    "equity_market" = NA_character_, "scenario_geography" = NA_character_,
    "year" = NA_integer_, "ald_sector" = NA_character_, "technology" = NA_character_,
    "plan_tech_prod" = NA_integer_, "plan_alloc_wt_tech_prod" = NA_integer_,
    "plan_carsten" = NA_integer_, "plan_emission_factor" = NA_integer_,
    "scen_tech_prod" = NA_integer_, "scen_alloc_wt_tech_prod" = NA_integer_,
    "scen_carsten" = NA_integer_, "scen_emission_factor" = NA_integer_,
    "plan_sec_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "plan_sec_carsten" = NA_integer_, "plan_sec_emissions_factor" = NA_integer_,
    "scen_sec_prod" = NA_integer_, "scen_alloc_wt_sec_prod" = NA_integer_,
    "scen_sec_carsten" = NA_integer_, "scen_sec_emissions_factor" = NA_integer_,
    "plan_tech_share" = NA_integer_, "scen_tech_share" = NA_integer_,
    "trajectory_deviation" = NA_integer_, "trajectory_alignment" = NA_integer_
  )
}

if (file.exists(file.path(results_path, portfolio_name, "Bonds_results_portfolio.rda"))) {
  bonds_results_portfolio <- read_rds(file.path(results_path, portfolio_name, "Bonds_results_portfolio.rda"))
} else {
  bonds_results_portfolio <- tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "scenario" = NA_character_, "allocation" = NA_character_,
    "equity_market" = NA_character_, "scenario_geography" = NA_character_,
    "year" = NA_integer_, "ald_sector" = NA_character_, "technology" = NA_character_,
    "plan_tech_prod" = NA_integer_, "plan_alloc_wt_tech_prod" = NA_integer_,
    "plan_carsten" = NA_integer_, "plan_emission_factor" = NA_integer_,
    "scen_tech_prod" = NA_integer_, "scen_alloc_wt_tech_prod" = NA_integer_,
    "scen_carsten" = NA_integer_, "scen_emission_factor" = NA_integer_,
    "plan_sec_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "plan_sec_carsten" = NA_integer_, "plan_sec_emissions_factor" = NA_integer_,
    "scen_sec_prod" = NA_integer_, "scen_alloc_wt_sec_prod" = NA_integer_,
    "scen_sec_carsten" = NA_integer_, "scen_sec_emissions_factor" = NA_integer_,
    "plan_tech_share" = NA_integer_, "scen_tech_share" = NA_integer_,
    "trajectory_deviation" = NA_integer_, "trajectory_alignment" = NA_integer_
  )
}

if (file.exists(file.path(results_path, portfolio_name, "Equity_results_company.rda"))) {
  equity_results_company <- read_rds(file.path(results_path, portfolio_name, "Equity_results_company.rda"))
} else {
  equity_results_company <- tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "scenario" = NA_character_, "allocation" = NA_character_,
    "id" = NA_character_, "company_name" = NA_character_,
    "financial_sector" = NA_character_, "port_weight" = NA_integer_,
    "allocation_weight" = NA_integer_, "plan_br_dist_alloc_wt" = NA_integer_,
    "scen_br_dist_alloc_wt" = NA_integer_,
    "equity_market" = NA_character_, "scenario_geography" = NA_character_,
    "year" = NA_integer_, "ald_sector" = NA_character_, "technology" = NA_character_,
    "plan_tech_prod" = NA_integer_, "plan_alloc_wt_tech_prod" = NA_integer_,
    "plan_carsten" = NA_integer_, "plan_emission_factor" = NA_integer_,
    "scen_tech_prod" = NA_integer_, "scen_alloc_wt_tech_prod" = NA_integer_,
    "scen_carsten" = NA_integer_, "scen_emission_factor" = NA_integer_,
    "plan_sec_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "plan_sec_carsten" = NA_integer_, "plan_sec_emissions_factor" = NA_integer_,
    "scen_sec_prod" = NA_integer_, "scen_alloc_wt_sec_prod" = NA_integer_,
    "scen_sec_carsten" = NA_integer_, "scen_sec_emissions_factor" = NA_integer_,
    "plan_tech_share" = NA_integer_, "scen_tech_share" = NA_integer_,
    "trajectory_deviation" = NA_integer_, "trajectory_alignment" = NA_integer_
  )
}
if (file.exists(file.path(results_path, portfolio_name, "Bonds_results_company.rda"))) {
  bonds_results_company <- read_rds(file.path(results_path, portfolio_name, "Bonds_results_company.rda"))
} else {
  bonds_results_company <- tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "scenario" = NA_character_, "allocation" = NA_character_,
    "id" = NA_character_, "company_name" = NA_character_,
    "financial_sector" = NA_character_, "port_weight" = NA_integer_,
    "allocation_weight" = NA_integer_, "plan_br_dist_alloc_wt" = NA_integer_,
    "scen_br_dist_alloc_wt" = NA_integer_,
    "equity_market" = NA_character_, "scenario_geography" = NA_character_,
    "year" = NA_integer_, "ald_sector" = NA_character_, "technology" = NA_character_,
    "plan_tech_prod" = NA_integer_, "plan_alloc_wt_tech_prod" = NA_integer_,
    "plan_carsten" = NA_integer_, "plan_emission_factor" = NA_integer_,
    "scen_tech_prod" = NA_integer_, "scen_alloc_wt_tech_prod" = NA_integer_,
    "scen_carsten" = NA_integer_, "scen_emission_factor" = NA_integer_,
    "plan_sec_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "plan_sec_carsten" = NA_integer_, "plan_sec_emissions_factor" = NA_integer_,
    "scen_sec_prod" = NA_integer_, "scen_alloc_wt_sec_prod" = NA_integer_,
    "scen_sec_carsten" = NA_integer_, "scen_sec_emissions_factor" = NA_integer_,
    "plan_tech_share" = NA_integer_, "scen_tech_share" = NA_integer_,
    "trajectory_deviation" = NA_integer_, "trajectory_alignment" = NA_integer_
  )
}

if (file.exists(file.path(results_path, portfolio_name, "Equity_results_map.rda"))) {
  equity_results_map <- read_rds(file.path(results_path, portfolio_name, "Equity_results_map.rda"))
} else {
  equity_results_map <- tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "ald_location" = NA_character_, "year" = NA_integer_,
    "ald_sector" = NA_character_, "technology" = NA_character_,
    "financial_sector" = NA_character_, "allocation" = NA_character_,
    "allocation_weight" = NA_integer_, "ald_production_unit" = NA_character_,
    "plan_alloc_wt_tech_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "equity_market" = NA_character_, "scenario" = NA_character_,
    "scenario_geography" = NA_character_
  )
}

if (file.exists(file.path(results_path, portfolio_name, "Bonds_results_map.rda"))) {
  bonds_results_map <- read_rds(file.path(results_path, portfolio_name, "Bonds_results_map.rda"))
} else {
  bonds_results_map <- tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "ald_location" = NA_character_, "year" = NA_integer_,
    "ald_sector" = NA_character_, "technology" = NA_character_,
    "financial_sector" = NA_character_, "allocation" = NA_character_,
    "allocation_weight" = NA_integer_, "ald_production_unit" = NA_character_,
    "plan_alloc_wt_tech_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "equity_market" = NA_character_, "scenario" = NA_character_,
    "scenario_geography" = NA_character_
  )
}


indicies_equity_results_portfolio <- read_rds(file.path(data_location_ext, "Indices_equity_portfolio.rda"))
indicies_bonds_results_portfolio <- read_rds(file.path(data_location_ext, "Indices_bonds_portfolio.rda"))
peers_equity_results_user <- read_rds(file.path(data_location_ext, "Peers_equity_results_portfolio_ind.rda")) %>% filter(investor_name == peer_group)
peers_bonds_results_user <- read_rds(file.path(data_location_ext, "Peers_bonds_results_portfolio_ind.rda")) %>% filter(investor_name == peer_group)
peers_equity_results_portfolio <- read_rds(file.path(data_location_ext, "Peers_equity_results_portfolio.rda")) %>% filter(investor_name == peer_group)
peers_bonds_results_portfolio <- read_rds(file.path(data_location_ext, "Peers_bonds_results_portfolio.rda")) %>% filter(investor_name == peer_group)



create_interactive_report(
  repo_path,
  template_dir,
  company_charts_dir,
  output_dir,
  language_select,
  project_name,
  investor_name,
  portfolio_name,
  peer_group,
  start_year,
  select_scenario = scenario,
  portfolio_allocation_method,
  scenario_geography,
  twodi_sectors = c("Power", "Automotive", "Shipping", "Oil&Gas", "Coal", "Steel", "Cement", "Aviation"),
  gbtech_sectors = c("Power", "Automotive", "Oil&Gas", "Coal"),
  green_techs = c("RenewablesCap", "HydroCap", "NuclearCap", "Hybrid", "Electric", "FuelCell", "Hybrid_HDV", "Electric_HDV", "FuelCell_HDV"),
  tech_roadmap_sectors = c("Automotive", "Power", "Oil&Gas", "Coal"),
  audit_file,
  emissions,
  equity_results_portfolio,
  bonds_results_portfolio,
  equity_results_company,
  bonds_results_company,
  equity_results_map,
  bonds_results_map,
  indicies_equity_results_portfolio,
  indicies_bonds_results_portfolio,
  peers_equity_results_portfolio,
  peers_bonds_results_portfolio,
  peers_equity_results_user,
  peers_bonds_results_user

)
