
options(encoding = "UTF-8") 


source("0_global_functions.R")
source("0_web_functions.R")

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

if (rstudioapi::isAvailable()) {
  portfolio_name_ref_all <- c("Portfolio2")
  working_location <- dirname(rstudioapi::getActiveDocumentContext()$path)
  set_web_parameters(file_path = paste0(working_location,"/parameter_files/WebParameters_2dii.yml"))
} else {
  portfolio_name_ref_all = get_portfolio_name()
  working_location <- getwd()
  set_web_parameters(file_path = paste0(working_location,"/parameter_files/WebParameters_docker.yml"))
}

working_location <- paste0(working_location, "/")

set_webtool_paths()

options(r2dii_config = paste0(par_file_path,"/AnalysisParameters.yml"))

set_global_parameters(paste0(par_file_path,"/AnalysisParameters.yml"))

set_portfolio_parameters(file_path = fs::path(par_file_path, paste0(portfolio_name_ref_all, "_PortfolioParameters.yml")))

# need to define an alternative location for data files
analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

source(paste0(template_path, "create_interactive_report.R"))



file_names <- read_csv(paste0(proc_input_path, "/file_names.csv"))

# TODO: Remove different start years

repo_path <- template_path
template_dir <- fs::path(template_path, "template_beta")
company_charts_dir <- fs::path(template_path, "company_charts", "Mixed_Portfolio")
output_dir <- outputs_path
project_name <- "working_dir"
investor_name <- investor_name_in
portfolio_name <- portfolio_name_in
start_year <- 2020
scenario <- "SDS"
portfolio_allocation_method <- "portfolio_weight"
scenario_geography <- "Global"
audit_file <- read_csv(fs::path(proc_input_path, file_names$loc_name[1],"/working_dir_audit_file.csv"))
emissions <- read_rds(fs::path(proc_input_path, file_names$loc_name[1],"/working_dir_emissions.rda"))
equity_results_portfolio <- read_rds(fs::path(results_path,"/", investor_name_in,"/Equity_results_portfolio.rda"))
bonds_results_portfolio <- read_rds(fs::path(results_path,"/", investor_name_in,"/Bonds_results_portfolio.rda"))
# bonds_results_portfolio <- equity_results_portfolio %>% filter(portfolio_name == "")
equity_results_company <- read_rds(fs::path(results_path,"/", investor_name_in,"/Equity_results_company.rda"))
bonds_results_company <- read_rds(fs::path(results_path,"/", investor_name_in,"/Bonds_results_company.rda"))

# bonds_results_company <- equity_results_company %>% filter(portfolio_name == "")
equity_results_map <-read_rds(fs::path(results_path,"/", investor_name_in,"/Equity_results_map.rda"))
bonds_results_map <-read_rds(fs::path(results_path,"/", investor_name_in,"/Bonds_results_map.rda"))

# bonds_results_map <- equity_results_map %>% filter(portfolio_name == "")
indicies_equity_results_portfolio <- read_rds(fs::path(data_location_ext,"/0_Indices_equity_portfolio.rda"))
indicies_bonds_results_portfolio <- read_rds(fs::path(data_location_ext,"/0_Indices_bonds_portfolio.rda"))
peers_equity_results_portfolio <- read_rds(fs::path(data_location_ext,"/0_Indices_equity_portfolio.rda"))
peers_bonds_results_portfolio <- read_rds(fs::path(data_location_ext,"/0_Indices_bonds_portfolio.rda"))



create_interactive_report(
  repo_path,
  template_dir,
  company_charts_dir,
  output_dir,
  project_name,
  investor_name,
  portfolio_name,
  start_year,
  scenario,
  portfolio_allocation_method,
  scenario_geography,
  twodi_sectors = c('Power', 'Automotive', 'Shipping', 'Oil&Gas', 'Coal', 'Steel', 'Cement', 'Aviation'),
  gbtech_sectors = c('Power', 'Automotive', 'Oil&Gas', 'Coal'),
  green_techs = c('RenewablesCap', 'HydroCap', 'NuclearCap', 'Hybrid', 'Electric'),
  tech_roadmap_sectors = c('Automotive', 'Power', 'Oil&Gas', 'Coal'),
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
  peers_bonds_results_portfolio
)
