cli::cli_h1("web_tool_script_3.R")

devtools::load_all(quiet = TRUE)
use_r_packages()

source("0_global_functions.R")
source("0_web_functions.R")

if (!exists("portfolio_name_ref_all")) { portfolio_name_ref_all <- "OnlyAutomotive_ALL" }
if (!exists("portfolio_root_dir")) { portfolio_root_dir <- "working_dir" }

setup_project()

set_webtool_paths(portfolio_root_dir)

set_portfolio_parameters(file_path = fs::path(par_file_path, paste0(portfolio_name_ref_all, "_PortfolioParameters.yml")))

set_project_parameters(file.path(par_file_path,paste0("ProjectParameters_", project_code, ".yml")))

analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

source(file.path(template_path, "create_interactive_report.R"))
source(file.path(template_path, "create_executive_summary.R"))
source(file.path(template_path, "useful_functions.R"))

# TODO: Deduplicate this step
report_name = select_report_template(project_code = project_code,
                                     language_select = language_select)

exec_summary_name = select_exec_summary_template(project_code = project_code,
                                                 language_select = language_select)

template_dir <- paste0(template_path, report_name,"/_book/")
exec_summary_dir <- paste0(template_path, exec_summary_name,"/")

survey_dir <- path(user_results_path, project_code, "survey")
real_estate_dir <- path(user_results_path, project_code, "real_estate")

output_dir <- file.path(outputs_path, portfolio_name_ref_all)

if (file.exists(file.path(proc_input_path, portfolio_name_ref_all, "audit_file.csv"))){
  audit_file <- read_csv(file.path(proc_input_path, portfolio_name_ref_all, "audit_file.csv"), col_types = cols())
}else{
  audit_file <- empty_audit_file()

}

# load portfolio overview
if (file.exists(file.path(proc_input_path, portfolio_name_ref_all, "overview_portfolio.rda"))) {
  portfolio_overview <- read_rds(file.path(proc_input_path, portfolio_name_ref_all, "overview_portfolio.rda"))
} else {
  portfolio_overview <- empty_portfolio_overview()
}


if (file.exists(file.path(proc_input_path, portfolio_name_ref_all, "emissions.rda"))){
  emissions <- read_rds(file.path(proc_input_path, portfolio_name_ref_all, "emissions.rda"))
}else{
  emissions <- empty_emissions_results()}

# load equity portfolio data
if (file.exists(file.path(results_path, portfolio_name_ref_all, "Equity_results_portfolio.rda"))) {
  equity_results_portfolio <- read_rds(file.path(results_path, portfolio_name_ref_all, "Equity_results_portfolio.rda"))
} else {
  equity_results_portfolio <- empty_portfolio_results()
}

# load bonds portfolio data
if (file.exists(file.path(results_path, portfolio_name_ref_all, "Bonds_results_portfolio.rda"))) {
  bonds_results_portfolio <- read_rds(file.path(results_path, portfolio_name_ref_all, "Bonds_results_portfolio.rda"))
} else {
  bonds_results_portfolio <- empty_portfolio_results()
}

# load equity company data
if (file.exists(file.path(results_path, portfolio_name_ref_all, "Equity_results_company.rda"))) {
  equity_results_company <- read_rds(file.path(results_path, portfolio_name_ref_all, "Equity_results_company.rda"))
} else {
  equity_results_company <- empty_company_results()
}

# load bonds company data
if (file.exists(file.path(results_path, portfolio_name_ref_all, "Bonds_results_company.rda"))) {
  bonds_results_company <- read_rds(file.path(results_path, portfolio_name_ref_all, "Bonds_results_company.rda"))
} else {
  bonds_results_company <- empty_company_results()
}

# load equity map data
if (file.exists(file.path(results_path, portfolio_name_ref_all, "Equity_results_map.rda"))) {
  equity_results_map <- read_rds(file.path(results_path, portfolio_name_ref_all, "Equity_results_map.rda"))
} else {
  equity_results_map <- empty_map_results()
}

# load bonds map data
if (file.exists(file.path(results_path, portfolio_name_ref_all, "Bonds_results_map.rda"))) {
  bonds_results_map <- read_rds(file.path(results_path, portfolio_name_ref_all, "Bonds_results_map.rda"))
} else {
  bonds_results_map <- empty_map_results()
}

# load equity stress test data
if (file.exists(file.path(results_path, portfolio_name_ref_all, "Equity_results_stress_test.rda"))) {
  equity_results_stress_test <- read_rds(file.path(results_path, portfolio_name_ref_all, "Equity_results_stress_test.rda"))
} else {
  equity_results_stress_test <- empty_st_results()
}

# load bonds stress test data
if (file.exists(file.path(results_path, portfolio_name_ref_all, "Bonds_results_stress_test.rda"))) {
  bonds_results_stress_test <- read_rds(file.path(results_path, portfolio_name_ref_all, "Bonds_results_stress_test.rda"))
} else {
  bonds_results_stress_test <- empty_st_results()
}

# load IPR stress test results
if (file.exists(file.path(results_path, portfolio_name_ref_all, "Stress_test_results_IPR.rds"))) {
  ipr_results_stress_test <- read_rds(file.path(results_path, portfolio_name_ref_all, "Stress_test_results_IPR.rds"))
} else {
  ipr_results_stress_test <- empty_ipr_st_results()
}

# load peers results both individual and aggregate
if (file.exists(file.path(data_location_ext, paste0(project_code, "_peers_equity_results_portfolio.rda")))){
  peers_equity_results_portfolio <- read_rds(file.path(data_location_ext, paste0(project_code, "_peers_equity_results_portfolio.rda")))
}else{
  peers_equity_results_portfolio <- empty_portfolio_results()
}

if(file.exists(file.path(data_location_ext, paste0(project_code, "_peers_bonds_results_portfolio.rda")))){
  peers_bonds_results_portfolio <- read_rds(file.path(data_location_ext, paste0(project_code, "_peers_bonds_results_portfolio.rda")))
}else{
  peers_bonds_results_portfolio <- empty_portfolio_results()
}

if (file.exists(file.path(data_location_ext, paste0(project_code, "_peers_equity_results_portfolio_ind.rda")))){
  peers_equity_results_user <- read_rds(file.path(data_location_ext, paste0(project_code, "_peers_equity_results_portfolio_ind.rda")))
}else{
  peers_equity_results_user <- empty_portfolio_results()
}

if(file.exists(file.path(data_location_ext, paste0(project_code, "_peers_bonds_results_portfolio_ind.rda")))){
  peers_bonds_results_user <- read_rds(file.path(data_location_ext, paste0(project_code, "_peers_bonds_results_portfolio_ind.rda")))
}else{
  peers_bonds_results_user <- empty_portfolio_results()
}

indices_equity_results_portfolio <- read_rds(file.path(data_location_ext, "Indices_equity_portfolio.rda"))

indices_bonds_results_portfolio <- read_rds(file.path(data_location_ext, "Indices_bonds_portfolio.rda"))

dataframe_translations <- readr::read_csv(
  path(template_path, "data/translation/dataframe_labels.csv"),
  col_types = cols()
)

header_dictionary <- readr::read_csv(
  path(template_path, "data/translation/dataframe_headers.csv"),
  col_types = cols()
)

js_translations <- jsonlite::fromJSON(
  txt = path(template_path, "data/translation/js_labels.json")
)


# scenario <- "WEO2019_SDS" # power, oil&gas, coal
# scenario_auto <- "ETP2017_B2DS" # automotive
# scenario_other <- "ETP2017_B2DS" # cement, steel, aviation
# scenario_shipping <- "SBTI_SBTI"
# portfolio_allocation_method <- "portfolio_weight"
# scenario_geography <- "Global"
#
# shock <- 2030 # this should come directly from the stress test.. 2030 based on current discussions in CHPA2020 case
#
# select_scenario = scenario
# select_scenario_auto = scenario_auto
# select_scenario_other = scenario_other
# select_scenario_shipping = scenario_shipping
#
# pacta_sectors_not_analysed <- c("Aviation","Cement","Shipping","Steel")
# twodi_sectors = c("Power", "Automotive", "Shipping", "Oil&Gas", "Coal", "Steel", "Cement", "Aviation")
# green_techs = c("RenewablesCap", "HydroCap", "NuclearCap", "Hybrid", "Electric", "FuelCell", "Hybrid_HDV", "Electric_HDV", "FuelCell_HDV","Dc-Electric Arc Furnace","Ac-Electric Arc Furnace")
# tech_roadmap_sectors = c("Automotive", "Power", "Oil&Gas", "Coal")
# alignment_techs = c("RenewablesCap", "CoalCap", "Coal", "Oil", "Gas", "Electric", "ICE")
#
# repo_path = template_path
# output_dir <- file.path(outputs_path, portfolio_name_ref_all)
#
# display_currency = "CHF"
# currency_exchange_value <- 1.03
# TODO: update this from the currencies file
file_name = "template.Rmd"

create_interactive_report(
  repo_path = template_path,
  template_dir = template_dir,
  output_dir = output_dir,
  survey_dir = survey_dir,
  real_estate_dir = real_estate_dir,
  language_select = language_select,
  report_name = report_name,
  project_name = project_name,
  investor_name = investor_name,
  portfolio_name = portfolio_name,
  peer_group = peer_group,
  start_year = start_year,
  shock = shock_year,
  select_scenario = select_scenario,
  select_scenario_auto = scenario_auto,
  select_scenario_shipping = scenario_shipping,
  select_scenario_other = scenario_other,
  portfolio_allocation_method = portfolio_allocation_method,
  scenario_geography = scenario_geography,
  twodi_sectors = twodi_sectors,
  green_techs = green_techs,
  tech_roadmap_sectors = tech_roadmap_sectors,
  pacta_sectors_not_analysed = pacta_sectors_not_analysed,
  audit_file = audit_file,
  emissions = emissions,
  portfolio_overview = portfolio_overview,
  equity_results_portfolio = equity_results_portfolio,
  bonds_results_portfolio = bonds_results_portfolio,
  equity_results_company = equity_results_company,
  bonds_results_company = bonds_results_company,
  equity_results_map = equity_results_map,
  bonds_results_map = bonds_results_map,
  indices_equity_results_portfolio = indices_equity_results_portfolio,
  indices_bonds_results_portfolio = indices_bonds_results_portfolio,
  peers_equity_results_portfolio = peers_equity_results_portfolio,
  peers_bonds_results_portfolio = peers_bonds_results_portfolio,
  peers_equity_results_user = peers_equity_results_user,
  peers_bonds_results_user = peers_bonds_results_user,
  equity_results_stress_test = equity_results_stress_test,
  bonds_results_stress_test = bonds_results_stress_test,
  dataframe_translations = dataframe_translations,
  js_translations = js_translations,
  ipr_results_stress_test = ipr_results_stress_test,
  display_currency = display_currency,
  currency_exchange_value = currency_exchange_value,
  header_dictionary = header_dictionary
)


create_executive_summary(
  file_name = "template.Rmd",
  exec_summary_dir = exec_summary_dir,
  output_dir = file.path(output_dir, "executive_summary"),
  language_select = language_select,
  project_name = "working_dir",
  investor_name = investor_name,
  portfolio_name = portfolio_name,
  peer_group = peer_group,
  start_year = start_year,
  select_scenario = scenario,
  portfolio_allocation_method = portfolio_allocation_method,
  scenario_geography = scenario_geography,
  twodi_sectors = twodi_sectors,
  green_techs = green_techs,
  tech_roadmap_sectors = tech_roadmap_sectors,
  alignment_techs = alignment_techs,
  equity_results_portfolio = equity_results_portfolio,
  bonds_results_portfolio = bonds_results_portfolio,
  peers_equity_results_portfolio = peers_equity_results_portfolio,
  peers_bonds_results_portfolio = peers_bonds_results_portfolio,
  peers_equity_results_user = peers_equity_results_user,
  peers_bonds_results_user = peers_bonds_results_user

)
