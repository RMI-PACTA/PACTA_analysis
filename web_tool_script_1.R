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

####################
#### DATA FILES ####
####################

# Files are first cleaned then saved for a faster read in time.
# Set parameter to ensure data is reprocessed in "new_data" == TRUE in the parameter file
file_location <- file.path(analysis_inputs_path, "cleaned_files")




if (new_data == TRUE) {
  currencies <- get_and_clean_currency_data()


  total_fund_list <- get_and_clean_total_fund_list_data()

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
    company_emissions,
    total_fund_list=total_fund_list
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

  fund_data$holding_isin <- as.character(fund_data$holding_isin)
  fund_data$fund_isin <- as.character(fund_data$fund_isin)


  fin_data <- fst::read_fst(file.path(file_location, "fin_data.fst"))

  comp_fin_data <- fst::read_fst(file.path(file_location, "comp_fin_data.fst"))

  debt_fin_data <- fst::read_fst(file.path(file_location, "debt_fin_data.fst"))

  total_fund_list <- fst::read_fst(file.path(file_location, "total_fund_list.fst"))

  if (inc_emission_factors) {
    average_sector_intensity <- fst::read_fst(file.path(file_location, "average_sector_intensity.fst"))

    company_emissions <- fst::read_fst(file.path(file_location, "company_emissions.fst"))
  }
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
  portfolio_raw,
  fin_data,
  fund_data,
  currencies,
  grouping_variables,
  total_fund_list=total_fund_list
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

portfolio <- create_ald_flag(portfolio, comp_fin_data, debt_fin_data)

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
save_if_exists(emissions_totals, portfolio_name, file.path(proc_input_path_, "emissions.rda"))
save_if_exists(fund_coverage_summary, portfolio_name, file.path(proc_input_path_, "fund_coverage_summary.rda"))
save_if_exists(unknown_funds_in_funds, portfolio_name, file.path(proc_input_path_, "unknown_funds_in_funds.rda"))

if(data_check(port_weights)){
  port_weights <- jsonlite::toJSON(x=port_weights)
  write(x = port_weights, file = file.path(proc_input_path_,"portfolio_weights.json"))
  }

rm(portfolio_total)
rm(portfolio)
rm(audit_file)
rm(eq_portfolio)
rm(cb_portfolio)
rm(fund_coverage_summary)
rm(fund_coverage)
rm(unknown_funds_in_funds)
