# web_tool_script.R

options(encoding = "UTF-8") 
# install.packages("devtools")
# devtools::install_github("2DegreesInvesting/r2dii")
# install.packages("fst")

library(tidyr)
library(dplyr)
library(scales) 
library(reshape2) 
library(tidyverse) 
library(readxl) 
library(tidyselect) 
library(r2dii.utils)
library(fs) 
library(jsonlite)
library(fst)


# source("0_portfolio_test.R")
# source("0_graphing_functions.R")
# source("0_reporting_functions.R")
source("0_portfolio_input_check_functions.R")
source("0_global_functions.R")
source("0_web_functions.R")
source("0_json_functions.R")

if (rstudioapi::isAvailable()) {
  portfolio_name_ref_all <- c("TestPortfolio_Input")
  working_location <- dirname(rstudioapi::getActiveDocumentContext()$path)
  set_web_parameters(file_path = paste0(working_location,"/parameter_files/WebParameters_2dii.yml"))
} else {
  portfolio_name_ref_all = get_portfolio_name()
  working_location <- getwd()
  set_web_parameters(file_path = paste0(working_location,"/parameter_files/WebParameters_docker.yml"))
  }

working_location <- paste0(working_location, "/")

# create_project_folder(project_name, twodii_internal, project_location_ext)

# replaced with web version
# set_project_paths(project_name, twodii_internal, project_location_ext)
# set_web_parameters(file_path = paste0(working_location,"/parameter_files/WebParameters.yml"))

# just done once
# create_project_folder(project_name, twodii_internal, project_location_ext)

set_webtool_paths()

# just done once
# copy_files(project_name)
options(r2dii_config = paste0(par_file_path,"/AnalysisParameters.yml"))

set_global_parameters(paste0(par_file_path,"/AnalysisParameters.yml"))

# need to define an alternative location for data files
analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

######################################################################

####################
#### DATA FILES ####
####################

# Files are first cleaned then saved for a faster read in time.
# Set parameter to ensure data is reprocessed in "new_data" == TRUE in the parameter file
file_location <- paste0(analysis_inputs_path, "cleaned_files")

if(new_data == TRUE){
  currencies <- get_and_clean_currency_data()
  
  # fund_data <- get_and_clean_fund_data()
  fund_data <- data.frame()
  
  fin_data <- get_and_clean_fin_data(fund_data)
  
  comp_fin_data <- get_and_clean_company_fin_data()
  
  debt_fin_data <- get_and_clean_debt_fin_data()
  
  # revenue_data <- get_and_clean_revenue_data()
  
  average_sector_intensity <- get_average_emission_data(inc_emission_factors)
  
  company_emissions <- get_company_emission_data(inc_emission_factors)
  
  save_cleaned_files(file_location,
                     currencies,
                     fund_data,
                     fin_data,
                     comp_fin_data,
                     debt_fin_data,
                     average_sector_intensity,
                     company_emissions)
  
  
  
  
}else{
  
  currencies <- read_file(paste0(file_location, "/currencies.fst"))
  
  fund_data <- read_file(paste0(file_location, "/fund_data.fst"))
  
  fin_data <- read_file(paste0(file_location, "/fin_data.fst"))
  
  comp_fin_data <- read_file(paste0(file_location, "/comp_fin_data.fst"))
  
  debt_fin_data <- read_file(paste0(file_location,"/debt_fin_data.fst"))
  
  # revenue_data <- read_file(paste0(file_location, "revenue_data.fst"))
  
  if (inc_emission_factors){
    average_sector_intensity <- read_file(paste0(file_location, "/average_sector_intensity.fst"))
    
    company_emissions <- read_file(paste0(file_location, "/company_emissions.fst"))
  }
}
####################
#### PORTFOLIOS ####
####################
portfolio_raw <- get_input_files(portfolio_name_ref_all)

portfolio <- process_raw_portfolio(portfolio_raw,
                                   fin_data,
                                   fund_data,
                                   currencies,
                                   grouping_variables)

portfolio <- portfolio %>% 
  mutate(portfolio_name = ifelse(portfolio_name %>% unique() %>% length() > 1,
                                 portfolio_name_ref_all,
                                 portfolio_name)
         )

portfolio <- add_revenue_split(has_revenue, portfolio, revenue_data)

portfolio <- create_ald_flag(portfolio, comp_fin_data, debt_fin_data)

eq_portfolio <- create_portfolio_subset(portfolio, 
                                        "Equity", 
                                        comp_fin_data)

cb_portfolio <- create_portfolio_subset(portfolio, 
                                        "Bonds", 
                                        debt_fin_data)

portfolio_total <- add_portfolio_flags(portfolio)

portfolio_overview <- portfolio_summary(portfolio_total)

identify_missing_data(portfolio_total)


audit_file <- create_audit_file(portfolio_total)

# create_audit_chart(audit_file, proc_input_path)

# website_text(audit_file)

emissions_totals <- calculate_portfolio_emissions(inc_emission_factors,
                                                  audit_file,
                                                  fin_data,
                                                  comp_fin_data,
                                                  average_sector_intensity,
                                                  company_emissions)

################
#### SAVING ####
################

# To save, files need to go in the portfolio specific folder. 
# Identify the portfolios to save; 
# Subset and Save these files


file_names <- identify_portfolios(portfolio_total)
write_csv(file_names, paste0(proc_input_path, "/file_names.csv"))

create_portfolio_subfolders(file_names, portfolio_name_ref_all)

for (p in 1:nrow(file_names)){
  
  portfolio_name_ <- file_names$portfolio_name[p]
  # proc_input_path_ <- paste0(proc_input_path, "/", file_names$portfolio_name[p])
  proc_input_path_ <- paste0(proc_input_path, "/", portfolio_name_ref_all)
  print(p)
  export_audit_information_jsons(audit_file_ = audit_file %>% filter(portfolio_name == portfolio_name_), 
                                 portfolio_total_ = portfolio_total %>% filter(portfolio_name == portfolio_name_),  
                                 folder_path = proc_input_path_
  )
  
  save_if_exists(audit_file, portfolio_name_, paste0(proc_input_path_, "/",portfolio_name_,"_audit_file.csv"), csv_or_rds = "csv")
  
  save_if_exists(portfolio_total, portfolio_name_, paste0(proc_input_path_, "/",portfolio_name_,"_total_portfolio.rda"))
  save_if_exists(eq_portfolio, portfolio_name_, paste0(proc_input_path_, "/",portfolio_name_,"_equity_portfolio.rda"))
  save_if_exists(cb_portfolio, portfolio_name_, paste0(proc_input_path_, "/",portfolio_name_,"_bonds_portfolio.rda"))
  save_if_exists(portfolio_overview, portfolio_name_, paste0(proc_input_path_, "/",portfolio_name_,"_overview_portfolio.rda"))
  save_if_exists(audit_file, portfolio_name_, paste0(proc_input_path_, "/",portfolio_name_,"_audit_file.rda"))
  save_if_exists(emissions_totals, portfolio_name_, paste0(proc_input_path_, "/",portfolio_name_,"_emissions.rda"))
  
}
