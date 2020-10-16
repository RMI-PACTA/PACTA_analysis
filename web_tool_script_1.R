devtools::load_all()
use_r_packages()

source("0_portfolio_input_check_functions.R")
source("0_global_functions.R")
source("0_web_functions.R")
source("0_json_functions.R")

setup_project()

working_location <- file.path(working_location)

set_webtool_paths()
options(r2dii_config = file.path(par_file_path, "AnalysisParameters.yml"))

set_global_parameters(file.path(par_file_path, "AnalysisParameters.yml"))

# need to define an alternative location for data files
analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

######################################################################

####################
#### DATA FILES ####
####################

# Files are first cleaned then saved for a faster read in time.
# Set parameter to ensure data is reprocessed in "new_data" == TRUE in the parameter file
file_location <- file.path(analysis_inputs_path, "cleaned_files")

if (new_data == TRUE) {
  currencies <- get_and_clean_currency_data()

  # fund_data <- get_and_clean_fund_data()
  fund_data <- data.frame()

  fin_data <- get_and_clean_fin_data(fund_data)

  comp_fin_data <- get_and_clean_company_fin_data()

  debt_fin_data <- get_and_clean_debt_fin_data()

  # revenue_data <- get_and_clean_revenue_data()

  average_sector_intensity <- get_average_emission_data(inc_emission_factors)

  company_emissions <- get_company_emission_data(inc_emission_factors)

  save_cleaned_files(
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

create_audit_chart(audit_file, proc_input_path)

website_text(audit_file)

emissions_totals <- calculate_portfolio_emissions(
  inc_emission_factors,
  audit_file,
  fin_data,
  comp_fin_data,
  average_sector_intensity,
  company_emissions
)

################
#### SAVING ####
################

# To save, files need to go in the portfolio specific folder.
# Identify the portfolios to save;
# Subset and Save these files

create_portfolio_subfolders(portfolio_name_ref_all)

file_names <- identify_portfolios(portfolio_total)

portfolio_name <- file_names$portfolio_name

proc_input_path_ <- file.path(proc_input_path, portfolio_name_ref_all)

# write_csv(file_names, file.path(proc_input_path_, "file_names.csv"))


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
