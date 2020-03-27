# TODO: 
# Emissions factors aggregations for each portfolio
# Clean up sectors options
# Functionalise saving
# Tech share? if yes, then complete function
# 


port_col_types <- set_col_types(grouping_variables, "ddddccccddcl")
##################
##### EQUITY #####
##################

equity_input_file <- paste0(proc_input_path, "/",project_name, "_equity_portfolio.csv")

if(file.exists(equity_input_file)){
  port_raw_all_eq <- read_csv(equity_input_file, col_types = port_col_types) %>% 
    mutate(id = as.character(id))
  
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
    
    # Missing tech share calculations
    # map needs the "complete" function built in if this is necessary
    
    
    investor_results_path <- paste0(results_path,"/", investor_name_select, "/") 
    if(!dir.exists(investor_results_path)){dir.create(investor_results_path)}
    
    write_rds(company_all_eq, paste0(investor_results_path, "Equity_results_company.rda"))
    write_rds(port_all_eq, paste0(investor_results_path, "Equity_results_portfolio.rda"))
    write_rds(map_eq, paste0(investor_results_path, "Equity_results_map.rda"))
    
  }
}

#################
##### BONDS #####
#################

bonds_inputs_file <- paste0(proc_input_path, "/",project_name, "_bonds_portfolio.csv")

if (file.exists(bonds_inputs_file)){
  
  port_raw_all_cb <- read_csv(bonds_inputs_file, col_types = port_col_types) %>% 
    mutate(id = as.character(id))
  
  
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
    
    # Missing tech share calculations
    # map needs the "complete" function built in if this is necessary
    
    
    investor_results_path <- paste0(results_path,"/", investor_name_select, "/") 
    if(!dir.exists(investor_results_path)){dir.create(investor_results_path)}
    
    write_rds(company_all_cb, paste0(investor_results_path, "Bonds_results_company.rda"))
    write_rds(port_all_cb, paste0(investor_results_path, "Bonds_results_portfolio.rda"))
    write_rds(map_cb, paste0(investor_results_path, "Bonds_results_map.rda"))
    
    
  }
  
}


#####################
##### AGGREGATE #####
#####################


### TODO: FUNCTIONALISE

all_investors <- list.dirs(results_path)
all_investors <- basename(all_investors)[-1]

all_results_cb <- "NA"
all_results_eq <- "NA"



for (i in 1:length(all_investors)){
  investor_name_select <- all_investors[i]
  print(investor_name_select)
  
  results_path_investor <- paste0(results_path,"/",investor_name_select,"/")
  if(file.exists(paste0(results_path_investor,"/Equity_results_portfolio.rda"))){
    
    results_eq <- as.data.frame(read_rds(paste0(results_path_investor,"/Equity_results_portfolio.rda")))
    
    if (is.data.frame(all_results_eq)){
      all_results_eq <- rbind(all_results_eq, results_eq)
    }else{
      all_results_eq <- results_eq
    }
    
  }
  
  if(file.exists(paste0(results_path_investor,"Bonds_results_portfolio.rda"))){
    
    results_cb <- read_rds(paste0(results_path_investor,"Bonds_results_portfolio.rda"))
    
    if (is.data.frame(all_results_cb)){
      all_results_cb <- rbind(all_results_cb, results_cb)
    }else{
      all_results_cb <- results_cb
    }
  }
}

saveRDS(all_results_cb,paste0(results_path,"/Bonds_results_portfolio.rda"))
write.csv(all_results_cb,paste0(results_path,"/Bonds_results_portfolio.csv"))

saveRDS(all_results_eq,paste0(results_path,"/Equity_results_portfolio.rda"))
write.csv(all_results_eq,paste0(results_path,"/Equity_results_portfolio.csv"))


