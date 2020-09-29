
#########################################################################
# START RUN ANALYIS
#########################################################################

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

source("0_portfolio_test.R")
source("0_global_functions.R")
source("0_web_functions.R")

portfolio_name_ref_all <- c("TestPortfolio_Input")
working_location <- here::here() 
set_web_parameters(file_path = paste0(working_location,"/parameter_files/WebParameters_2dii.yml"))

working_location <- paste0(working_location, "/")

set_webtool_paths()

# just done once
options(r2dii_config = paste0(par_file_path,"/AnalysisParameters.yml"))

set_global_parameters(paste0(par_file_path,"/AnalysisParameters.yml"))

# need to define an alternative location for data files
analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

# delete all results files within the current portfolio folder
unlink(paste0(results_path,"/",portfolio_name_ref_all,"/*"), force = TRUE, recursive = TRUE)

# run again so output folders are available after deleting past results
file_names <- read_csv(paste0(proc_input_path, "/file_names.csv"))
create_portfolio_subfolders(file_names, portfolio_name_ref_all)

port_col_types <- set_col_types(grouping_variables, "ddddccccddclc")

##################
##### EQUITY #####
##################

ald_scen_eq <- get_ald_scen("Equity")
ald_raw_eq <- get_ald_raw("Equity")

equity_input_file <- paste0(proc_input_path,"/", portfolio_name_ref_all, "/",file_names$portfolio_name,"_equity_portfolio.rda")
portfolio_name <- file_names$portfolio_name
  
if(file.exists(equity_input_file)){
  port_raw_all_eq <- read_rds(equity_input_file) %>% 
    mutate(id = as.character(id))
    
  if(length(colnames(port_raw_all_eq)) != nchar(port_col_types)){stop("Check port_col_types: difference in length")}
    
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
    port_pw_eq <- port_weight_allocation(port_eq)
    
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
      
      map_eq <- merge_in_geography(company_all_eq, ald_raw_eq)
    
      map_eq <- aggregate_map_data(map_eq)
        
    } 
      
    # Technology Share Calculation	
    port_all_eq <- calculate_technology_share(port_all_eq)	
    
    company_all_eq <- calculate_technology_share(company_all_eq)	
    
    # Scenario alignment calculations	
    port_all_eq <- calculate_scenario_alignment(port_all_eq)	
    
    company_all_eq <- calculate_scenario_alignment(company_all_eq)
    
    pf_file_results_path <- paste0(results_path,"/", portfolio_name_ref_all, "/") 
    if(!dir.exists(pf_file_results_path)){dir.create(pf_file_results_path)}
    
    if(data_check(company_all_eq)){write_rds(company_all_eq, paste0(pf_file_results_path, portfolio_name,"_Equity_results_company.rda"))}	
    if(data_check(port_all_eq)){write_rds(port_all_eq, paste0(pf_file_results_path, portfolio_name,"_Equity_results_portfolio.rda"))}	
    if(has_map){if(data_check(map_eq)){write_rds(map_eq, paste0(pf_file_results_path, portfolio_name,"_Equity_results_map.rda"))}}
    
    # investor_results_path <- paste0(results_path,"/", investor_name_select, "/") 
    # if(!dir.exists(investor_results_path)){dir.create(investor_results_path)}
    # 
    # if(data_check(company_all_eq)){write_rds(company_all_eq, paste0(investor_results_path, "Equity_results_company.rda"))}	
    # if(data_check(port_all_eq)){write_rds(port_all_eq, paste0(investor_results_path, "Equity_results_portfolio.rda"))}	
    # if(has_map){if(data_check(map_eq)){write_rds(map_eq, paste0(investor_results_path, "Equity_results_map.rda"))}}
    
  }
}


#################
##### BONDS #####
#################

ald_scen_cb <- get_ald_scen("Bonds")
ald_raw_cb <- get_ald_raw("Bonds")

bonds_inputs_file <- paste0(proc_input_path,"/", portfolio_name_ref_all,"/",file_names$portfolio_name,"_bonds_portfolio.rda")
portfolio_name <- file_names$portfolio_name
  
if (file.exists(bonds_inputs_file)){
    
  port_raw_all_cb <- read_rds(bonds_inputs_file) %>% 
    mutate(id = as.character(id))
    
  if(length(colnames(port_raw_all_cb)) != nchar(port_col_types)){stop("Check port_col_types: difference in length")}
    
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
    port_pw_cb <- port_weight_allocation(port_cb)
      
    company_pw_cb <- aggregate_company(port_pw_cb)
      
    port_pw_cb <- aggregate_portfolio(company_pw_cb) 
      
    # Create combined outputs
    company_all_cb <- company_pw_cb
      
    port_all_cb <- port_pw_cb
      
    if(has_map){
        
      if(data_check(company_all_cb)){
        map_cb <- merge_in_geography(company_all_cb, ald_raw_cb)
          
        map_cb <- aggregate_map_data(map_cb)
      }  
    } 
      
    # Technology Share Calculation	
    if(nrow(port_all_cb)>0){port_all_cb <- calculate_technology_share(port_all_cb)}	
      
    if(nrow(company_all_cb)>0){company_all_cb <- calculate_technology_share(company_all_cb)}	
      
    # Scenario alignment calculations	
    port_all_cb <- calculate_scenario_alignment(port_all_cb)	
      
    company_all_cb <- calculate_scenario_alignment(company_all_cb)
      
    pf_file_results_path <- paste0(results_path,"/", portfolio_name_ref_all, "/") 
    if(!dir.exists(pf_file_results_path)){dir.create(pf_file_results_path)}
      
    if(data_check(company_all_cb)){ write_rds(company_all_cb, paste0(pf_file_results_path, portfolio_name,"_Bonds_results_company.rda"))}	
    if(data_check(port_all_cb)){write_rds(port_all_cb, paste0(pf_file_results_path, portfolio_name,"_Bonds_results_portfolio.rda"))}	
    if(has_map){if(data_check(map_cb)){write_rds(map_cb, paste0(pf_file_results_path, portfolio_name,"_Bonds_results_map.rda"))}}
      
    # investor_results_path <- paste0(results_path,"/", investor_name_select, "/") 
    # if(!dir.exists(investor_results_path)){dir.create(investor_results_path)}
    # 
    # if(data_check(company_all_cb)){ write_rds(company_all_cb, paste0(investor_results_path, "Bonds_results_company.rda"))}	
    # if(data_check(port_all_cb)){write_rds(port_all_cb, paste0(investor_results_path, "Bonds_results_portfolio.rda"))}	
    # if(has_map){if(data_check(map_cb)){write_rds(map_cb, paste0(investor_results_path, "Bonds_results_map.rda"))}}
      
  }
}

