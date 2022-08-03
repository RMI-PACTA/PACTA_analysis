devtools::load_all(quiet = TRUE)
use_r_packages()

cli::cli_h1("web_tool_script_1.R{get_build_version_msg()}")


if (!exists("portfolio_name_ref_all")) { portfolio_name_ref_all <- "TestPortfolio_Input" }
if (!exists("portfolio_root_dir")) { portfolio_root_dir <- "working_dir" }
portfolio_root_dir <- "working_dir"

setup_project()

working_location <- file.path(working_location)

set_webtool_paths(portfolio_root_dir)

set_portfolio_parameters(file_path = fs::path(par_file_path, paste0(portfolio_name_ref_all, "_PortfolioParameters.yml")))

set_project_parameters(file.path(working_location, "parameter_files",paste0("ProjectParameters_", project_code, ".yml")))

# need to define an alternative location for data files
analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

# To save, files need to go in the portfolio specific folder, created here
create_portfolio_subfolders(portfolio_name_ref_all = portfolio_name_ref_all, project_location = project_location)
######################################################################


inc_emission_factors <- FALSE # FIXME: until emissions data is ready



####################
#### DATA FILES ####
####################


# load necessary input data ----------------------------------------------------

file_location <- file.path(analysis_inputs_path)

currencies <- readRDS(file.path(file_location, "currencies.rds"))

fund_data <- readRDS(file.path(file_location, "fund_data.rds"))
fund_data$holding_isin <- as.character(fund_data$holding_isin)
fund_data$fund_isin <- as.character(fund_data$fund_isin)

total_fund_list <- readRDS(file.path(file_location, "total_fund_list.rds"))

fin_data <- readRDS(file.path(file_location, "financial_data.rds"))

abcd_flags_equity <- readRDS(file.path(file_location, "abcd_flags_equity.rds"))
abcd_flags_bonds <- readRDS(file.path(file_location, "abcd_flags_bonds.rds"))

if (inc_emission_factors) {
  average_sector_intensity <- readRDS(file.path(file_location, "average_sector_intensity.rds"))

  company_emissions <- readRDS(file.path(file_location, "company_emissions.rds"))
}



####################
#### PORTFOLIOS ####
####################
abort_if_file_doesnt_exist(
  here::here(
    "working_dir", "20_Raw_Inputs", glue::glue("{portfolio_name_ref_all}.csv")
  )
)
portfolio_raw <- get_input_files(portfolio_name_ref_all)

portfolio <- process_raw_portfolio(
  portfolio_raw = portfolio_raw,
  fin_data = fin_data,
  fund_data = fund_data,
  currencies = currencies,
  grouping_variables = grouping_variables,
  total_fund_list = total_fund_list
)

# information of coverage and coverage loses for all funds in raw_portfolio
fund_coverage <- get_fund_coverage(
  portfolio_raw,
  fin_data,
  fund_data,
  currencies,
  grouping_variables
)

# reduce information on fund coverage to a data frame that can be shared with users
fund_coverage_summary <- summarize_fund_coverage(fund_coverage)

# list ISINs of unknown funds in funds. the list includes value_usd to estimate importance of the isin for o
unknown_funds_in_funds <- list_unknown_funds_in_funds(portfolio)

portfolio <- add_revenue_split(has_revenue, portfolio, revenue_data)

portfolio <- create_ald_flag(portfolio, comp_fin_data = abcd_flags_equity, debt_fin_data = abcd_flags_bonds)

eq_portfolio <- create_portfolio_subset(
  portfolio,
  "Equity"
)

cb_portfolio <- create_portfolio_subset(
  portfolio,
  "Bonds"
)

portfolio_total <- add_portfolio_flags(portfolio)

portfolio_overview <- portfolio_summary(portfolio_total)

audit_file <- create_audit_file(portfolio_total)

if (inc_emission_factors) {
emissions_totals <- calculate_average_portfolio_emissions(
  portfolio_total,
  comp_fin_data,
  average_sector_intensity)
}


port_weights <- pw_calculations(eq_portfolio, cb_portfolio)


################
#### SAVING ####
################

proc_input_path_ <- file.path(proc_input_path, portfolio_name_ref_all)

export_audit_information_data(
  audit_file_ = audit_file %>% filter(portfolio_name == portfolio_name),
  portfolio_total_ = portfolio_total %>% filter(portfolio_name == portfolio_name),
  folder_path = proc_input_path_
)

save_if_exists(portfolio_total, portfolio_name, file.path(proc_input_path_, "total_portfolio.rda"))
save_if_exists(eq_portfolio, portfolio_name, file.path(proc_input_path_, "equity_portfolio.rda"))
save_if_exists(cb_portfolio, portfolio_name, file.path(proc_input_path_, "bonds_portfolio.rda"))
save_if_exists(portfolio_overview, portfolio_name, file.path(proc_input_path_, "overview_portfolio.rda"))
save_if_exists(audit_file, portfolio_name, file.path(proc_input_path_, "audit_file.rda"))
save_if_exists(audit_file, portfolio_name, file.path(proc_input_path_, "audit_file.csv"), csv_or_rds = "csv")
save_if_exists(fund_coverage_summary, portfolio_name, file.path(proc_input_path_, "fund_coverage_summary.rda"))
save_if_exists(unknown_funds_in_funds, portfolio_name, file.path(proc_input_path_, "unknown_funds_in_funds.rda"))

if (inc_emission_factors) {
  save_if_exists(emissions_totals, portfolio_name, file.path(proc_input_path_, "emissions.rda"))
}

if(data_check(port_weights)){
  port_weights <- jsonlite::toJSON(x=port_weights)
  write(x = port_weights, file = file.path(proc_input_path_,"portfolio_weights.json"))
  }

remove_if_exists(portfolio_total)
remove_if_exists(portfolio)
remove_if_exists(audit_file)
remove_if_exists(eq_portfolio)
remove_if_exists(cb_portfolio)
remove_if_exists(fund_coverage_summary)
remove_if_exists(fund_coverage)
remove_if_exists(unknown_funds_in_funds)

