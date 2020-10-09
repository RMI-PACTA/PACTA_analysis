## Project start
# File List Check for External Data Requirements:
# security_financial_data.rda
# consolidated_financial_data.rda
# debt_financial_data.rda
# bonds_ald_scenario.rda
# equity_ald_scenario.rda
# masterdata_ownership_datastore.rda
# masterdata_debt_datastore.rda

# optional:
# fund_data_2019Q4.rda (or relevant time stamp)
# revenue_data_member_ticker.rda (if not available, set has_revenue = FALSE in parameter file)

# Obtains data, processes the portfolio and saves the files

####################
#### DATA FILES ####
####################
currencies <- get_and_clean_currency_data()

fund_data <- get_and_clean_fund_data()

fin_data <- get_and_clean_fin_data(fund_data)

comp_fin_data <- get_and_clean_company_fin_data()

debt_fin_data <- get_and_clean_debt_fin_data()

revenue_data <- get_and_clean_revenue_data()

average_sector_intensity <- get_average_emission_data(inc_emission_factors)

company_emissions <- get_company_emission_data(inc_emission_factors)

####################
#### PORTFOLIOS ####
####################
portfolio_raw <- read_raw_portfolio_file(project_name)

portfolio <- process_raw_portfolio(
  portfolio_raw,
  fin_data,
  fund_data,
  currencies,
  grouping_variables
)

portfolio <- add_revenue_split(has_revenue, portfolio, revenue_data)

portfolio <- create_ald_flag(portfolio, comp_fin_data, debt_fin_data)

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
if (length(file_format_list) == 0) {
  stop("Saving results: No file formats defined")
}

if ("csv" %in% file_format_list) {
  if (data_check(portfolio_total)) {
    write_csv(portfolio_total, paste0(proc_input_path, "/", project_name, "_total_portfolio.csv"))
  }
  if (data_check(eq_portfolio)) {
    write_csv(eq_portfolio, paste0(proc_input_path, "/", project_name, "_equity_portfolio.csv"))
  }
  if (data_check(cb_portfolio)) {
    write_csv(cb_portfolio, paste0(proc_input_path, "/", project_name, "_bonds_portfolio.csv"))
  }
  if (data_check(portfolio_overview)) {
    write_csv(portfolio_overview, paste0(proc_input_path, "/", project_name, "_overview_portfolio.csv"))
  }
  if (data_check(audit_file)) {
    write_csv(audit_file, paste0(proc_input_path, "/", project_name, "_audit_file.csv"))
  }
  if (data_check(emissions_totals)) {
    write_csv(emissions_totals, paste0(proc_input_path, "/", project_name, "_emissions.csv"))
  }
}

if ("rds" %in% file_format_list | "rda" %in% file_format_list) {
  if (data_check(portfolio_total)) {
    write_rds(portfolio_total, paste0(proc_input_path, "/", project_name, "_total_portfolio.rda"))
  }
  if (data_check(eq_portfolio)) {
    write_rds(eq_portfolio, paste0(proc_input_path, "/", project_name, "_equity_portfolio.rda"))
  }
  if (data_check(cb_portfolio)) {
    write_rds(cb_portfolio, paste0(proc_input_path, "/", project_name, "_bonds_portfolio.rda"))
  }
  if (data_check(portfolio_overview)) {
    write_rds(portfolio_overview, paste0(proc_input_path, "/", project_name, "_overview_portfolio.rda"))
  }
  if (data_check(audit_file)) {
    write_rds(audit_file, paste0(proc_input_path, "/", project_name, "_audit_file.rda"))
  }
  if (data_check(emissions_totals)) {
    write_rds(emissions_totals, paste0(proc_input_path, "/", project_name, "_emissions.rda"))
  }
}
