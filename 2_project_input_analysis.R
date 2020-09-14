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

portfolio <- process_raw_portfolio(portfolio_raw,
                                   fin_data,
                                   fund_data,
                                   currencies, 
                                   grouping_variables)

portfolio <- add_revenue_split(has_revenue, portfolio, revenue_data)

portfolio <- create_ald_flag(portfolio, comp_fin_data, debt_fin_data)

portfolio <- add_bics_sector(portfolio, comp_fin_data, debt_fin_data)

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

create_audit_chart(audit_file, proc_input_path)

emissions_totals <- calculate_portfolio_emissions(inc_emission_factors,
                                                  audit_file,
                                                  fin_data, 
                                                  comp_fin_data,  
                                                  average_sector_intensity,
                                                  company_emissions)

################
#### SAVING ####
################
if (length(file_format_list) == 0){stop("Saving results: No file formats defined")}

if("csv" %in% file_format_list){
  if(data_check(portfolio_total)){write_csv(portfolio_total, paste0(proc_input_path, "/", project_name, "_total_portfolio.csv"))}
  if(data_check(eq_portfolio)){write_csv(eq_portfolio, paste0(proc_input_path, "/", project_name, "_equity_portfolio.csv"))}
  if(data_check(cb_portfolio)){write_csv(cb_portfolio, paste0(proc_input_path, "/", project_name, "_bonds_portfolio.csv"))}
  if(data_check(portfolio_overview)){write_csv(portfolio_overview, paste0(proc_input_path, "/", project_name, "_overview_portfolio.csv"))}
  if(data_check(audit_file)){write_csv(audit_file, paste0(proc_input_path, "/", project_name,"_audit_file.csv"))}
  if(data_check(emissions_totals)){write_csv(emissions_totals, paste0(proc_input_path, "/", project_name, "_emissions.csv"))}
}

if("rds" %in% file_format_list | "rda" %in% file_format_list){
  if(data_check(portfolio_total)){write_rds(portfolio_total, paste0(proc_input_path, "/", project_name, "_total_portfolio.rda"))}
  if(data_check(eq_portfolio)){write_rds(eq_portfolio, paste0(proc_input_path, "/", project_name, "_equity_portfolio.rda"))}
  if(data_check(cb_portfolio)){write_rds(cb_portfolio, paste0(proc_input_path, "/", project_name, "_bonds_portfolio.rda"))}
  if(data_check(portfolio_overview)){write_rds(portfolio_overview, paste0(proc_input_path, "/", project_name, "_overview_portfolio.rda"))}
  if(data_check(audit_file)){write_rds(audit_file, paste0(proc_input_path, "/", project_name,"_audit_file.rda"))}
  if(data_check(emissions_totals)){write_rds(emissions_totals, paste0(proc_input_path, "/", project_name, "_emissions.rda"))}
}


# Calculate PF and ownership weights--------------------------------------------


port_col_types <- set_col_types(grouping_variables, "ddddccccddclc")


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
    
    investor_proc_inputs_path <- paste0(proc_input_path,"/", investor_name_select, "/") 
    if(!dir.exists(investor_proc_inputs_path)){dir.create(investor_proc_inputs_path)}
    
    if(data_check(company_all_eq)){write_rds(company_all_eq, paste0(investor_proc_inputs_path, "/", investor_name_select, "_equity_company_weights.rda"))}	
    if(data_check(port_all_eq)){write_rds(port_all_eq, paste0(investor_proc_inputs_path, "/", investor_name_select, "_equity_portfolio_weights.rda"))}	
    if(has_map){if(data_check(map_eq)){write_rds(map_eq, paste0(investor_proc_inputs_path, "/", investor_name_select, "_equity_map_weights.rda"))}}
    
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
    port_pw_cb <- port_weight_allocation(port_cb)
    
    company_pw_cb <- aggregate_company(port_pw_cb)
    
    port_pw_cb <- aggregate_portfolio(company_pw_cb) 
    
    # Create combined outputs
    company_all_cb <- company_pw_cb
    
    port_all_cb <- port_pw_cb
    
    if(has_map){
      
      if(data_check(company_all_cb)){
        map_cb <- merge_in_geography(portfolio = company_all_cb, 
                                     ald_raw = ald_raw_cb)
        
        map_cb <- aggregate_map_data(map_cb)
      }  
    } 
    
    investor_proc_inputs_path <- paste0(proc_input_path,"/", investor_name_select, "/") 
    if(!dir.exists(investor_proc_inputs_path)){dir.create(investor_proc_inputs_path)}
    
    if(data_check(company_all_cb)){write_rds(company_all_cb, paste0(investor_proc_inputs_path, "/", investor_name_select, "_bonds_company_weights.rda"))}	
    if(data_check(port_all_cb)){write_rds(port_all_cb, paste0(investor_proc_inputs_path, "/", investor_name_select, "_bonds_portfolio_weights.rda"))}	
    if(has_map){if(data_check(map_cb)){write_rds(map_cb, paste0(investor_proc_inputs_path, "/", investor_name_select, "_bonds_map_weights.rda"))}}
    
  }
}



