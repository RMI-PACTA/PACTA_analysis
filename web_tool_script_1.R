cli::cli_h1("web_tool_script_1.R")

devtools::load_all(quiet = TRUE)
use_r_packages()

source("0_portfolio_input_check_functions.R")
source("0_global_functions.R")
source("0_web_functions.R")
source("0_json_functions.R")
source("0_portfolio_test.R")

if (!exists("portfolio_name_ref_all")) { portfolio_name_ref_all <- "TestPortfolio_Input" }
if (!exists("portfolio_root_dir")) { portfolio_root_dir <- "working_dir" }

setup_project()

working_location <- file.path(working_location)

set_webtool_paths(portfolio_root_dir)
options(r2dii_config = file.path(par_file_path, "AnalysisParameters.yml"))

set_global_parameters(file.path(par_file_path, "AnalysisParameters.yml"))

# need to define an alternative location for data files
analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

# To save, files need to go in the portfolio specific folder, created here
create_portfolio_subfolders(portfolio_name_ref_all = portfolio_name_ref_all, project_location = project_location)

######################################################################

####################
#### DATA FILES ####
####################

# Files are first cleaned then saved for a faster read in time.
# Set parameter to ensure data is reprocessed in "new_data" == TRUE in the parameter file
file_location <- file.path(analysis_inputs_path, "cleaned_files")

if (new_data == TRUE) {
  prepare_new_data(
    inputs_dir = analysis_inputs_path,
    financial_timestamp = financial_timestamp,
    has_revenue = has_revenue,
    inc_emission_factors = inc_emission_factors,
    processed_data_dir = file_location
  )
} else {
  currencies <- readRDS(file.path(file_location, paste0(default_filenames_sans_ext[["exchange_rates"]], ".rds")))

  fund_data <- readRDS(file.path(file_location, paste0(default_filenames_sans_ext[["funds"]], ".rds")))

  fin_data <- readRDS(file.path(file_location, paste0(default_filenames_sans_ext[["security_financial"]], ".rds")))

  comp_fin_data <- readRDS(file.path(file_location, paste0(default_filenames_sans_ext[["consolidated_financial"]], ".rds")))

  debt_fin_data <- readRDS(file.path(file_location, paste0(default_filenames_sans_ext[["debt_financial"]], ".rds")))

  if (has_revenue) {
    revenue_data <- readRDS(file.path(file_location, paste0(default_filenames_sans_ext[["revenue"]], ".rds")))
  } else {
    revenue_data <- data.frame()
  }

  if (inc_emission_factors) {
    average_sector_intensity <- readRDS(file.path(file_location, paste0(default_filenames_sans_ext[["average_sector_intensity"]], ".rds")))
    company_emissions <- readRDS(file.path(file_location, paste0(default_filenames_sans_ext[["company_emissions"]], ".rds")))
  } else {
    average_sector_intensity <- data.frame()
    company_emissions <- data.frame()
  }
}


####################
#### PORTFOLIOS ####
####################
portfolio_raw <- get_input_files(portfolio_name_ref_all)

portfolio <- process_raw_portfolio(
  portfolio_raw,
  fin_data,
  fund_data,
  currencies,
  grouping_variables
)

portfolio <- add_revenue_split(has_revenue, portfolio, revenue_data)

portfolio <- create_ald_flag(portfolio, comp_fin_data, debt_fin_data)

portfolio <- add_bics_sector(portfolio)

eq_portfolio <- create_portfolio_subset(
  portfolio,
  "Equity",
  comp_fin_data
)

cb_portfolio <- create_portfolio_subset(
  portfolio,
  "Bonds",
  debt_fin_data
)

portfolio_total <- add_portfolio_flags(portfolio)

portfolio_overview <- portfolio_summary(portfolio_total)

identify_missing_data(portfolio_total)

audit_file <- create_audit_file(portfolio_total)

emissions_totals <- calculate_average_portfolio_emissions(
  portfolio_total,
  comp_fin_data,
  average_sector_intensity)

port_weights <- pw_calculations(eq_portfolio, cb_portfolio)


################
#### SAVING ####
################

# Identify the portfolios to save;
# Subset and Save these files

file_names <- identify_portfolios(portfolio_total)

portfolio_name <- file_names$portfolio_name

proc_input_path_ <- file.path(proc_input_path, portfolio_name_ref_all)

# write_csv(file_names, file.path(proc_input_path_, "file_names.csv"))

# create_audit_chart(audit_file, proc_input_path = proc_input_path_)

# website_text(audit_file, proc_input_path = proc_input_path_)

export_audit_information_jsons(
  audit_file_ = audit_file %>% filter(portfolio_name == portfolio_name),
  portfolio_total_ = portfolio_total %>% filter(portfolio_name == portfolio_name),
  folder_path = proc_input_path_
)

save_if_exists(audit_file, portfolio_name, file.path(proc_input_path_, "audit_file.csv"), csv_or_rds = "csv")

save_if_exists(portfolio_total, portfolio_name, file.path(proc_input_path_, "total_portfolio.rda"))
save_if_exists(eq_portfolio, portfolio_name, file.path(proc_input_path_, "equity_portfolio.rda"))
save_if_exists(cb_portfolio, portfolio_name, file.path(proc_input_path_, "bonds_portfolio.rda"))
save_if_exists(portfolio_overview, portfolio_name, file.path(proc_input_path_, "overview_portfolio.rda"))
save_if_exists(audit_file, portfolio_name, file.path(proc_input_path_, "audit_file.rda"))
save_if_exists(emissions_totals, portfolio_name, file.path(proc_input_path_, "emissions.rda"))

if(data_check(port_weights)){
  port_weights <- jsonlite::toJSON(x=port_weights)
  write(x = port_weights, file = file.path(proc_input_path_,"portfolio_weights.json"))
  }
