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
  currencies <- get_currency_data_for_timestamp(financial_timestamp)

  fund_data <- PACTA.analysis::get_and_clean_fund_data(analysis_inputs_path, "funds_2019Q4_reduced_for_meta.rds")

  fin_data <- get_and_clean_fin_data(fund_data)

  comp_fin_data <- PACTA.analysis::get_and_clean_company_fin_data(analysis_inputs_path)

  debt_fin_data <- get_debt_financial_data(analysis_inputs_path)

  if (has_revenue) {
    revenue_data <- get_revenue_data(analysis_inputs_path, filename = "revenue_data_member_ticker.rds")
  } else {
    revenue_data <- data.frame()
  }

  if (inc_emission_factors) {
    average_sector_intensity <- get_average_sector_intensity_data(analysis_inputs_path)
    company_emissions <- get_company_emissions_data(analysis_inputs_path)
  } else {
    average_sector_intensity <- data.frame()
    company_emissions <- data.frame()
  }

  save_files_to(
    file_location,
    currencies,
    fund_data,
    fin_data,
    comp_fin_data,
    debt_fin_data,
    average_sector_intensity,
    company_emissions
  )
} else {
  currencies <- fst::read_fst(file.path(file_location, "currencies.fst"))

  read_fst_or_return_null <- function(fst_file) {
    if (!file.exists(fst_file)) {
      return(NULL)
    }

    fst::read_fst(fst_file)
  }

  fund_data_path <- file.path(file_location, "fund_data.fst")
  fund_data <- read_fst_or_return_null(fund_data_path)

  fin_data <- fst::read_fst(file.path(file_location, "fin_data.fst"))

  comp_fin_data <- fst::read_fst(file.path(file_location, "comp_fin_data.fst"))


  debt_fin_data <- fst::read_fst(file.path(file_location, "debt_fin_data.fst"))

  if (inc_emission_factors) {
    average_sector_intensity <- fst::read_fst(file.path(file_location, "average_sector_intensity.fst"))

    company_emissions <- fst::read_fst(file.path(file_location, "company_emissions.fst"))
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

# portfolio <- add_bics_sector(portfolio)

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
