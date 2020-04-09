# web_tool_script.R

options(encoding = "UTF-8") 

library(tidyr)
library(dplyr)
library(scales)
library(reshape2)
library(tidyverse)
library(readxl)
library(tidyselect)

# NEW
library(r2dii.utils)

source("0_portfolio_test.R")
source("0_graphing_functions.R")
source("0_reporting_functions.R")
source("0_portfolio_input_check_functions.R")
source("0_global_functions.R")
# source("0_sda_approach.R")
source("0_web_functions.R")

if (rstudioapi::isAvailable()) {
  portfolio_name_ref <- "Portfolio3"
} else {
  portfolio_name_ref = GetPortfolioName()
  working_location <- getwd()
}


project_name <- "web_tool"
twodii_internal <- FALSE

project_location_ext <- "C:/Users/clare/Desktop/ExternalTest"
data_location_ext <- "C:/Users/clare/Desktop/ExternalTest/r2dii_data/"

#####################################################################

# just done once
# create_project_folder(project_name, twodii_internal, project_location_ext)

# replaced with web version
# set_project_paths(project_name, twodii_internal, project_location_ext)
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
currencies <- get_and_clean_currency_data()

fund_data <- get_and_clean_fund_data()

fin_data <- get_and_clean_fin_data()

comp_fin_data <- get_and_clean_company_fin_data()

# revenue_data <- get_and_clean_revenue_data()

average_sector_intensity <- get_average_emission_data(inc_emission_factors)

company_emissions <- get_company_emission_data(inc_emission_factors)

####################
#### PORTFOLIOS ####
####################

set_web_parameters()

portfolio_raw <- read_raw_portfolio_file(project_name)

portfolio_raw <- add_naming_to_portfolio(portfolio_raw)

portfolio <- process_raw_portfolio(portfolio_raw,
                                        fin_data,
                                        fund_data,
                                        currencies, 
                                        grouping_variables)


portfolio <- add_revenue_split(has_revenue, portfolio, revenue_data)

eq_portfolio <- create_portfolio_subset(portfolio, 
                                        "Equity", 
                                        comp_fin_data)

cb_portfolio <- create_portfolio_subset(portfolio, 
                                        "Bonds", 
                                        comp_fin_data)

portfolio_total <- add_portfolio_flags(portfolio)

portfolio_overview <- portfolio_summary(portfolio_total)

identify_missing_data(portfolio_total)

audit_file <- create_audit_file(portfolio_total, comp_fin_data)

create_audit_chart(audit_file, proc_input_path)

website_text(audit_file)

emissions_totals <- calculate_portfolio_emissions(inc_emission_factors,
                                                  audit_file,
                                                  fin_data, 
                                                  comp_fin_data,  
                                                  average_sector_intensity,
                                                  company_emissions)

################
#### SAVING ####
################


if(data_check(audit_file)){write_csv(audit_file, paste0(proc_input_path, "/", project_name,"_audit_file.csv"))}

if(data_check(portfolio_total)){write_rds(portfolio_total, paste0(proc_input_path, "/", project_name, "_total_portfolio.rda"))}
if(data_check(eq_portfolio)){write_rds(eq_portfolio, paste0(proc_input_path, "/", project_name, "_equity_portfolio.rda"))}
if(data_check(cb_portfolio)){write_rds(cb_portfolio, paste0(proc_input_path, "/", project_name, "_bonds_portfolio.rda"))}
if(data_check(portfolio_overview)){write_rds(portfolio_overview, paste0(proc_input_path, "/", project_name, "_overview_portfolio.rda"))}
if(data_check(audit_file)){write_rds(audit_file, paste0(proc_input_path, "/", project_name,"_audit_file.rda"))}
if(data_check(emissions_totals)){write_rds(emissions_totals, paste0(proc_input_path, "/", project_name, "_emissions.rda"))}
#########################################################################


# delete all results files
unlink(paste0(results_path,"/*"), force = TRUE, recursive = TRUE)

port_col_types <- set_col_types(grouping_variables, "ddddccccddcl")
##################
##### EQUITY #####
##################

equity_input_file <- paste0(proc_input_path, "/",project_name, "_equity_portfolio.rda")

if(file.exists(equity_input_file)){
  port_raw_all_eq <- read_rds(equity_input_file) %>% 
    mutate(id = as.character(id))
  
  if(length(colnames(port_raw_all_eq)) != nchar(port_col_types)){stop("Check port_col_types: difference in length")}
  
  ald_scen_eq <- get_ald_scen("Equity")
  
  ald_raw_eq <- get_ald_raw("Equity")
  
  list_investors_eq <- unique(port_raw_all_eq$investor_name)
  
  for (e in 1:length(list_investors_eq) ){
    map_eq <- NA
    company_all_eq <- NA
    port_all_eq <- NA
    
    investor_name_select <- list_investors_eq[e]
    print(paste0(e, ": ", investor_name_select))
    
    port_raw_eq <- port_raw_all_eq %>% filter(investor_name == investor_name_select)
    
    port_eq <- calculate_weights(port_raw_eq, "Equity", grouping_variables)
    
    port_eq <- merge_in_ald(port_eq, ald_scen_eq)
    
    # Portfolio weight methodology
    port_pw_eq <- port_weight_allocation(port_eq, "Equity")
    
    company_pw_eq <- aggregate_company(port_pw_eq)
    
    port_pw_eq <- aggregate_portfolio(company_pw_eq) 
    
    # Ownership weight methodology
    port_own_eq <- ownership_allocation(port_eq)
    
    company_own_eq <- aggregate_company(port_own_eq)
    
    port_own_eq <- aggregate_portfolio(company_own_eq) 
    
    # Create combined outputs
    company_all_eq <- bind_rows(company_pw_eq, company_own_eq)
    
    port_all_eq <- bind_rows(port_pw_eq, port_own_eq) 
    
    if(has_map){
      
      map_eq <- merge_in_geography(company_all_eq, ald_raw_eq, sectors_for_maps)
      
      map_eq <- aggregate_map_data(map_eq)
      
    } 
    
    # Technology Share Calculation	
    port_all_eq <- calculate_technology_share(port_all_eq)	
    
    company_all_eq <- calculate_technology_share(company_all_eq)	
    
    # Scenario alignment calculations	
    port_all_eq <- calculate_scenario_alignment(port_all_eq)	
    
    company_all_eq <- calculate_scenario_alignment(company_all_eq)
    
    investor_results_path <- paste0(results_path,"/", investor_name_select, "/") 
    if(!dir.exists(investor_results_path)){dir.create(investor_results_path)}
    
    if(data_check(company_all_eq)){write_rds(company_all_eq, paste0(investor_results_path, "Equity_results_company.rda"))}	
    if(data_check(port_all_eq)){write_rds(port_all_eq, paste0(investor_results_path, "Equity_results_portfolio.rda"))}	
    if(has_map){if(data_check(map_eq)){write_rds(map_eq, paste0(investor_results_path, "Equity_results_map.rda"))}}
    
  }
}

#################
##### BONDS #####
#################

bonds_inputs_file <- paste0(proc_input_path, "/",project_name, "_bonds_portfolio.rda")

if (file.exists(bonds_inputs_file)){
  
  port_raw_all_cb <- read_rds(bonds_inputs_file) %>% 
    mutate(id = as.character(id))
  
  if(length(colnames(port_raw_all_cb)) != nchar(port_col_types)){stop("Check port_col_types: difference in length")}
  
  ald_scen_cb <- get_ald_scen("Bonds")
  
  ald_raw_cb <- get_ald_raw("Bonds")
  
  list_investors_cb <- unique(port_raw_all_cb$investor_name)
  
  for (b in 1:length(list_investors_cb) ){
    map_cb <- NA
    company_all_cb <- NA
    port_all_cb <- NA
    
    investor_name_select <- list_investors_cb[b]
    
    print(paste0(b, ": ", investor_name_select))
    
    port_raw_cb <- port_raw_all_cb %>% filter(investor_name == investor_name_select)
    
    port_cb <- calculate_weights(port_raw_cb, "Bonds", grouping_variables)
    
    port_cb <- merge_in_ald(port_cb, ald_scen_cb)
    
    # Portfolio weight methodology
    port_pw_cb <- port_weight_allocation(port_cb, "Bonds")
    
    company_pw_cb <- aggregate_company(port_pw_cb)
    
    port_pw_cb <- aggregate_portfolio(company_pw_cb) 
    
    # Create combined outputs
    company_all_cb <- company_pw_cb
    
    port_all_cb <- port_pw_cb
    
    if(has_map){
      
      if(data_check(company_all_cb)){
        map_cb <- merge_in_geography(company_all_cb, ald_raw_cb, sectors_for_maps)
        
        map_cb <- aggregate_map_data(map_cb)
      }  
    } 
    
    # Technology Share Calculation	
    if(nrow(port_all_cb)>0){port_all_cb <- calculate_technology_share(port_all_cb)}	
    
    if(nrow(company_all_cb)>0){company_all_cb <- calculate_technology_share(company_all_cb)}	
    
    # Scenario alignment calculations	
    port_all_cb <- calculate_scenario_alignment(port_all_cb)	
    
    company_all_cb <- calculate_scenario_alignment(company_all_cb)
    
    investor_results_path <- paste0(results_path,"/", investor_name_select, "/") 
    if(!dir.exists(investor_results_path)){dir.create(investor_results_path)}
    
    if(data_check(company_all_cb)){ write_rds(company_all_cb, paste0(investor_results_path, "Bonds_results_company.rda"))}	
    if(data_check(port_all_cb)){write_rds(port_all_cb, paste0(investor_results_path, "Bonds_results_portfolio.rda"))}	
    if(has_map){if(data_check(map_cb)){write_rds(map_cb, paste0(investor_results_path, "Bonds_results_map.rda"))}}
    
  }
}

##################################################################



