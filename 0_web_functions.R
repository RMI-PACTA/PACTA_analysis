# 0_web_functions
library(fs, quietly = TRUE)
suppressPackageStartupMessages(library(dplyr, quietly = TRUE))
library(tidyr, quietly = TRUE)
library(jsonlite, quietly = TRUE)




get_portfolio_name <- function(){
  PortfolioNameRef = commandArgs(trailingOnly=TRUE)
  
  if (length(PortfolioNameRef)==0) {
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
  } 
  
  return(PortfolioNameRef)
  
}

set_webtool_paths <- function(){
  
  project_location <<-  paste0(working_location,"web_folders/working_dir")
  
  log_path <<- paste0(project_location,"/00_Log_Files")
  par_file_path <<- paste0(project_location,"/10_Parameter_File")
  raw_input_path <<-  paste0(project_location,"/20_Raw_Inputs")
  proc_input_path <<- paste0(project_location,"/30_Processed_Inputs")
  results_path <<- paste0(project_location,"/40_Results")
  outputs_path <<- paste0(project_location,"/50_Outputs")
  
  
}

set_web_parameters <- function(file_path){
  
  cfg <- config::get(file = file_path)
  
  project_location_ext <<- cfg$paths$project_location_ext
  data_location_ext <<- cfg$paths$data_location_ext
  
  project_name <<- cfg$parameters$project_name
  twodii_internal <<- cfg$parameters$twodii_internal
  new_data <<- cfg$parameters$new_data
  portfolio_name_ref <<- cfg$parameters$portfolio_name_ref
  financial_timestamp <<- cfg$parameters$financial_timestamp
  portfolio_name_in <<- cfg$parameters$portfolio_name_in
  investor_name_in <<- cfg$parameters$investor_name_in
  
}

add_naming_to_portfolio <- function(portfolio_raw){
  
  portfolio_raw$portfolio_name <- portfolio_name_in
  portfolio_raw$investor_name <- investor_name_in
  
  return(portfolio_raw)
  
}

get_input_files <- function(){
  
  portfolio <- NA
  
  input_path <- paste0(project_location, "/20_Raw_Inputs/")
  
  input_files = list.files(path = input_path,full.names = T)
  
  for (i in 1:length(input_files)){
    
    input_file_path <- input_files[i]
    
    portfolio_ <- read_web_input_file(input_file_path)
    
    portfolio_ <- portfolio_ %>%  select(-contains("X"))
    head(portfolio_)
    # clean and check column names
    portfolio_ <- check_input_file_contents(portfolio_, portfolio_name_in, investor_name_in)
    
    head(portfolio_)
    portfolio_$count = i
    
    portfolio <- rbind(portfolio, portfolio_)
    # create warning here for user. 
  }
  portfolio <- clean_portfolio_col_types(portfolio)
  portfolio <- clear_portfolio_input_blanks(portfolio)

  return(portfolio)
  }

read_web_input_file <- function(input_file_path){
  
  file_ext = tools::file_ext(input_file_path)

  if (file_ext == "csv"){
    input_file <- read_csv(input_file_path)
    
  }
  if (file_ext == "xlsx"){
    input_file <- read_xlsx(input_file_path)
  }
  
  if (file_ext == "txt"){
    enc <- guess_encoding(input_file_path)$encoding[1]
    input_file <- read.table(input_file_path,sep = ",", header = T, fileEncoding = enc)   
    
    if (ncol(input_file) == 1){
      input_file <- read.table(input_file_path,sep = "\t", header = T, fileEncoding = enc)
    }
    
    if (ncol(input_file) == 1){
      input_file <- read.table(input_file_path,sep = ";", header = T, fileEncoding = enc)
    }
    
    
  }
  
  if(data_check(input_file) == FALSE){
    warning("Input file not readable")
  }
  
  return(input_file)

}

check_input_file_contents <- function(portfolio_, portfolio_name_in, investor_name_in){
  
  portfolio_clean <- clean_colnames_portfolio_input_file(portfolio_)
  
  necessary_columns <- c(grouping_variables, "market_value", "currency", "isin")

  if (!"portfolio_name" %in% colnames(portfolio_clean)){portfolio_clean$portfolio_name = portfolio_name_in}
  if (!"investor_name" %in% colnames(portfolio_clean)){portfolio_clean$investor_name = investor_name_in}
  
  if (length(setdiff(necessary_columns, colnames(portfolio_clean))) > 0){
    missing_cols = setdiff(necessary_columns, colnames(portfolio_clean))
    
    warning(paste0("Missing inputs for this portfolio: ", missing_cols)) # Add LOG
  }
  
  return(portfolio_clean)  
}

website_text <- function(audit_file){
  
  PortValues <- audit_file %>% 
    ungroup() %>%
    filter(valid_input == TRUE) %>%
    summarize(PortTot = sum(value_usd))
  
  Asset.Values <- audit_file %>%
    ungroup() %>%
    group_by(asset_type) %>%
    filter(valid_input == TRUE) %>%
    summarise(Tot = sum(value_usd))
  
  asset_types <- c("Equity","Bonds")
  
  if (length(setdiff(asset_types,Asset.Values$asset_type))>0){
    newrow <- data.frame(asset_type = setdiff(asset_types,Asset.Values$asset_type),Tot=0)
    Asset.Values <- rbind(Asset.Values,newrow)
  }
  
  Bonds.Value <- prettyNum(round(Asset.Values$Tot[Asset.Values$asset_type == "Bonds"],0),big.mark=",",scientific=FALSE)
  
  Equity.Value <- prettyNum(round(Asset.Values$Tot[Asset.Values$asset_type == "Equity"],0),big.mark=",",scientific=FALSE)
  
  text <- paste0("The portfolio you have uploaded has $", prettyNum(round(PortValues[[1]],0),big.mark=",",scientific=FALSE)," USD in holdings. 
                 Of which $",Bonds.Value," USD is in bonds, 
                and $",Equity.Value," USD is in equity. 

                The remainder of the holdings are in asset classes outside the scope of this analysis. 
                For more information as to how each holding is classified, review the chart and audit file below.")
  
  write(text,paste0(proc_input_path,"/Websitetext.txt"))
}

save_cleaned_files <- function(save_loc, 
                               currencies, 
                               fund_data, 
                               fin_data,
                               comp_fin_data,
                               average_sector_intensity,
                               company_emissions){
  
  if(!dir.exists(save_loc)){dir.create(save_loc)}
  
  write_rds(currencies,paste0(save_loc,"/currencies.rda"))
  write_rds(fund_data,paste0(save_loc,"/fund_data.rda"))
  write_rds(fin_data,paste0(save_loc,"/fin_data.rda"))
  write_rds(comp_fin_data,paste0(save_loc,"/comp_fin_data.rda"))
  write_rds(average_sector_intensity,paste0(save_loc,"/average_sector_intensity.rda" ))
  write_rds(company_emissions,paste0(save_loc,"/company_emissions.rda"))
  
  if(check_file_size(save_loc)) warning("File size exceeds what can be pushed to GitHub. Check before Committing")
  
  
  
}

check_file_size <- function(folder_to_check){
  
  files_to_check <- list.files(folder_to_check,full.names = T)
  any(file.size(files_to_check) > 100e6) 
  
  
}

export_audit_information_jsons <- function(audit_file_ = audit_file, portfolio_total_ = portfolio_total,  folder_path = paste0(proc_input_path), project_name_=NA){
  
  # Check format
  assertthat::is.string(folder_path)
  assertthat::are_equal(is.data.frame(audit_file_),TRUE)
  
  if ("Meta Investor" %in% audit_file_$investor_name){
    audit_file_ <- subset(audit_file_, investor_name != "Meta Investor")
  }
  if ("Meta Investor" %in% portfolio_total_$investor_name){
    portfolio_total_ <- subset(portfolio_total_, investor_name != "Meta Investor")
  }
  
  
  
  
  
  folder_path <- paste0(folder_path, "/")
  if(!is.na(project_name_)){folder_path <- paste0(folder_path, project_name_, "_")}
  # function 
  export_audit_graph_json(audit_file_, paste0(folder_path, "coveragegraph"))
  export_audit_invalid_json(portfolio_total_, paste0(folder_path,"invalidsecurities"))
  export_audit_textvar_json(portfolio_total_, paste0(folder_path,"coveragetextvar"))
  
  
}







this_project_dir <- path_expand('~/Dropbox (2° Investing)/PortCheck_v2/10_Projects/PACTA_2020_TESTS_CJ')
this_root_output_dir <- getwd()
start_year <- 2019


to_jsonp <-
  function(x, obj_name, pretty = FALSE, auto_unbox = TRUE, na = 'null', digits = NA, ...) {
    json <- jsonlite::toJSON(x, pretty = pretty, auto_unbox = auto_unbox,
                             na = na, digits = digits, ...)
    paste0('var ', obj_name, ' = ', json, ';')
  }


export_report_content_variables_json <-function(audit_file__ = audit_file, 
                                results_port_eq__ = port_all_eq, 
                                results_port_cb__ = port_all_cb,
                                results_company_eq__ = company_all_eq, 
                                results_company_cb__ = company_all_cb, 
                            
                                investor_name_ = 'Meta Investor', 
                                portfolio_name_ = 'Meta Portfolio',
                                
                                
                                scenario_ = "SDS",
                                scenario_geography_ = "Global",
                                allocation_ = "portfolio_weight",
                                export_path_full = paste0(outputs_path,"/", investor_name_, "_", portfolio_name_,"_",allocation_,"_gitbook_variables.json")
){
  
  this_investor_name <- investor_name_
  this_portfolio_name <- portfolio_name_
  
  start_year <- min( min(unique(results_port_cb__$year)),  min(unique(results_port_eq__$year)),
                     min(unique(results_company_eq__$year)),  min(unique(results_company_cb__$year)), na.rm = T )
  
  
  
  
  assertthat::are_equal(this_investor_name %in% audit_file__$investor_name, T)
  
  audit_file__ <- audit_file__  %>% 
    filter(investor_name == this_investor_name,
           portfolio_name == this_portfolio_name)
  
  results_port_eq__ <- results_port_eq__  %>% 
    filter(investor_name == this_investor_name,
           portfolio_name == this_portfolio_name,
           year == start_year,
           scenario == scenario_,
           scenario_geography == scenario_geography_,
           allocation == allocation_)
  
  results_port_cb__ <- results_port_cb__  %>% 
    filter(investor_name == this_investor_name,
           portfolio_name == this_portfolio_name,
           year == start_year,
           scenario == scenario_,
           scenario_geography == scenario_geography_,
           allocation == allocation_)
  
  
  results_company_eq__ <- results_company_eq__  %>% 
    filter(investor_name == this_investor_name,
           portfolio_name == this_portfolio_name,
           year == start_year,
           scenario == scenario_,
           scenario_geography == scenario_geography_,
           allocation == allocation_)
  
  
  results_company_cb__ <- results_company_cb__  %>% 
    filter(investor_name == this_investor_name,
           portfolio_name == this_portfolio_name,
           year == start_year,
           scenario == scenario_,
           scenario_geography == scenario_geography_,
           allocation == allocation_)
  
  technologies <- results_port_eq__$technology
  green_brown_ <- c()
  
  for (i in 1:length(technologies)){green_brown_[i] <- (green_brown(technologies[i]))}
  results_port_eq__["green_brown_"] <- green_brown_
  
  technologies <- results_port_cb__$technology
  green_brown_ <- c()
  
  for (i in 1:length(technologies)){green_brown_[i] <- (green_brown(technologies[i]))}
  results_port_cb__["green_brown_"] <- green_brown_
  
  
  
   unique_isins <-
     audit_file__ %>% 
    filter(investor_name == this_investor_name,
           portfolio_name == this_portfolio_name) %>% 
    pull(isin) %>% 
    unique() %>% 
    length()
  
   
   total_portfolio_value_usd <- audit_file__%>% 
     filter(direct_holding == T) %>% pull(value_usd) %>% sum(na.rm =T)
   
   
   total_portfolio_percentage_equity <- (audit_file__%>% 
     filter(asset_type == 'Equity') %>% pull(value_usd) %>% sum(na.rm =T))/total_portfolio_value_usd
   
   
   total_portfolio_percentage_bonds <- (audit_file__%>% 
                                          filter(asset_type == 'Bonds') %>% pull(value_usd) %>% sum(na.rm =T))/total_portfolio_value_usd
   
   
   total_portfolio_percentage_other_asset_classes <-(audit_file__%>% 
                                                       filter(asset_type == 'Others') %>% pull(value_usd) %>% sum(na.rm =T))/total_portfolio_value_usd
   
   
   
   total_portfolio_percentage_coverage <- (total_portfolio_percentage_equity+total_portfolio_percentage_bonds)
   
   
   
  
   
   total_portfolio_percentage_climate_rel_emission <- "XXX_1"
   
   
   results_absolute_value_equity <- audit_file__ %>% filter(asset_type == "Equity", valid_input == T) %>% pull(value_usd) %>% sum(na.rm = T)
     
    
   results_absolute_value_bonds <- audit_file__ %>% filter(asset_type == "Bonds", valid_input == T) %>% pull(value_usd) %>% sum(na.rm = T)
     
   results_absolute_value_valid_input <- results_absolute_value_equity +  results_absolute_value_bonds
   
   results_percentage_climate_rel_value_bonds <- results_port_cb__ %>% filter(ald_sector != "Other") %>% pull("plan_carsten") %>% sum( na.rm=T)
   
   
   results_percentage_climate_rel_value_equity <- results_port_eq__ %>% filter(ald_sector != "Other") %>% pull("plan_carsten") %>% sum( na.rm=T)
   
   
   results_percentage_climate_rel_value <-  ((results_percentage_climate_rel_value_bonds*results_absolute_value_bonds)+ 
                                               (results_percentage_climate_rel_value_equity*results_absolute_value_equity))/results_absolute_value_valid_input
     
     
   results_percentage_lowcarb_value_equity <- results_port_eq__ %>% filter(green_brown_== "green") %>% pull("plan_carsten") %>% sum( na.rm=T)
   
   results_percentage_lowcarb_value_bonds  <- results_port_cb__ %>% filter(green_brown_== "green") %>% pull("plan_carsten") %>% sum( na.rm=T)
   
   results_percentage_highcarb_value_equity <- results_port_eq__ %>% filter(green_brown_== "brown") %>% pull("plan_carsten") %>% sum( na.rm=T)
   
   results_percentage_highcarb_value_bonds <-  results_port_cb__ %>% filter(green_brown_== "brown") %>% pull("plan_carsten") %>% sum( na.rm=T)
   
   results_absolute_lowcarb_value_equity <-  results_percentage_lowcarb_value_equity*results_absolute_value_equity
   results_absolute_highcarb_value_equity  <-  results_percentage_highcarb_value_equity*results_absolute_value_equity
   
   results_absolute_lowcarb_value_bonds <- results_percentage_lowcarb_value_bonds*results_absolute_value_bonds
   results_absolute_highcarb_value_bonds <- results_percentage_highcarb_value_bonds*results_absolute_value_bonds
   
   results_percentage_highcarb_value <- (results_absolute_highcarb_value_bonds + results_absolute_highcarb_value_equity)/(results_absolute_value_bonds+results_absolute_value_equity)
   results_percentage_lowcarb_value <- (results_absolute_lowcarb_value_bonds + results_absolute_lowcarb_value_equity)/(results_absolute_value_bonds+results_absolute_value_equity)
   
   results_company_nr_relevent_companies <- "5"
     
   
   
   parameters_list <- 
    list(InvestorName = this_investor_name,
         PortfolioName = this_portfolio_name,
         unique_isins = unique_isins,
         
         total_portfolio_value_usd = total_portfolio_value_usd,
         total_portfolio_percentage_equity = total_portfolio_percentage_equity,
         total_portfolio_percentage_bonds = total_portfolio_percentage_bonds,
         total_portfolio_percentage_other_asset_classes = total_portfolio_percentage_other_asset_classes,
         total_portfolio_percentage_coverage = total_portfolio_percentage_coverage,
         
         results_percentage_climate_rel_value = results_percentage_climate_rel_value,
         
         total_portfolio_percentage_climate_rel_emission = total_portfolio_percentage_climate_rel_emission,
         
         results_absolute_value_equity = results_absolute_value_equity,
         results_absolute_value_bonds = results_absolute_value_bonds,
         
         results_percentage_climate_rel_value_bonds = results_percentage_climate_rel_value_bonds,
         results_percentage_climate_rel_value_equity = results_percentage_climate_rel_value_equity,
         
         results_absolute_highcarb_value_bonds = results_absolute_highcarb_value_bonds,
         results_absolute_highcarb_value_equity = results_absolute_highcarb_value_equity,
         
         results_percentage_highcarb_value_bonds = results_percentage_highcarb_value_bonds,
         results_percentage_highcarb_value_equity = results_percentage_highcarb_value_equity,
         
         results_percentage_lowcarb_value_bonds = results_percentage_lowcarb_value_bonds,
         results_percentage_lowcarb_value_equity = results_percentage_lowcarb_value_equity,
         
         results_percentage_lowcarb_value = results_percentage_lowcarb_value,
         results_percentage_highcarb_value = results_percentage_highcarb_value,
         
         results_company_nr_relevent_companies = results_company_nr_relevent_companies
         
    )
  
  
  parameters_list %>% 
    to_jsonp('data_parameters') %>%
    writeLines(path(export_path_full))
  
}





export_audit_graph_json <-function(audit_file__, export_path_full){
  
  
  audit_file__ <-audit_file__ %>% select("isin", "holding_id","flag")
  
  all_flags<- c("Missing currency information","Negative or missing input value","Invalid or missing ISIN","Holding not in Bloomberg database","Included in analysis")
  flags_in_auditfile <- unique(audit_file__$flag)
  
  
  indicator <- TRUE
  for (i in 1:length(flags_in_auditfile)){indicator = indicator & (flags_in_auditfile[i] %in% all_flags)} 
  assertthat::are_equal(indicator, TRUE)
  
  
  number_of_isin  <- length(audit_file__$holding_id)
  
  missing_currency <- audit_file__ %>% subset(flag == "Missing currency information")
  number_missing_currency <- length(missing_currency$holding_id)
  
  negative_missing_input <- audit_file__ %>% subset(flag == "Negative or missing input value")
  number_negative_missing_input <- length(negative_missing_input$holding_id)
  
  invalid_input   <- audit_file__ %>% subset(flag == "Invalid or missing ISIN")
  number_invalid_input  <- length(invalid_input$holding_id)
  
  not_in_bloomberg <-  audit_file__ %>% subset(flag == "Holding not in Bloomberg database") 
  number_not_in_bloomberg <- length(not_in_bloomberg$holding_id)
  
  included_in_analysis <-  audit_file__ %>% subset(flag == "Included in analysis") 
  number_included_in_analysis <- length(included_in_analysis$holding_id)
  
  all_flags_numbers <- c(number_missing_currency,  number_negative_missing_input,  number_invalid_input,  number_not_in_bloomberg,  number_included_in_analysis)
  assertthat::are_equal(number_of_isin, sum(all_flags_numbers))
  
  number_all_invalid_input <- sum(c(number_missing_currency, number_negative_missing_input,number_invalid_input))
  
  legend <- c( "\"Invalid input\"", "\"No data coverage\"", "\"Included in analysis\"")
  keys <- c("\"key_1\"", "\"key_2\"", "\"key_3\"")
  values <- c(number_all_invalid_input, number_not_in_bloomberg, number_included_in_analysis)
  
  assertthat::are_equal(length(legend), length(keys))
  assertthat::are_equal(length(values), length(keys))
  
  json_head = "{"
  json_tail = "}"
  
  chart_information <- json_head
  chart_legend <- json_head
  for (i in 1:(length(keys)-1)){
    chart_information <- paste0(chart_information," " , keys[i],": ", values[i],",")
    chart_legend <- paste0(chart_legend," " , keys[i],": ", legend[i],",")
  } 
  i = length(keys)
  chart_information <- paste0(chart_information," " , keys[i],": ", values[i])
  chart_legend <- paste0(chart_legend," " , keys[i],": ", legend[i])
  
  chart_information <- paste0(chart_information, json_tail)
  chart_legend <- paste0(chart_legend, json_tail)
  
  write(chart_legend, file=paste0(export_path_full,"legend.json"))
  write(chart_information, file=paste0(export_path_full,".json"))
  
}

export_audit_invalid_json <-function(portfolio_total_,export_path_full){
  
  portfolio_total_ <- portfolio_total_ %>% subset(flag %in% c("Missing currency information", "Negative or missing input value","Invalid or missing ISIN"))
  portfolio_total_ <- portfolio_total_ %>% select("isin", "market_value", "currency", "flag")
  portfolio_total_ <- portfolio_total_[order(-portfolio_total_$market_value),]
  
  colnames(portfolio_total_) <- c("isin","marketValues","currency","flag")
  invalidsecurties <- toJSON(portfolio_total_, dataframe = c('columns'))
  
  write(invalidsecurties, file=paste0(export_path_full,".json"))
  
} 

export_audit_textvar_json <-function(portfolio_total_, export_path_full){
  
  portfolio_total_ <- portfolio_total_ %>% select("value_usd", "asset_type", "has_valid_input")
  total_v <- sum(portfolio_total_$value_usd)
  included <- portfolio_total_ %>% subset(has_valid_input == TRUE)
  included_v <- sum(included$value_usd)
  bonds <- portfolio_total_ %>% subset(asset_type == "Bonds")
  bonds_v <- sum(bonds$value_usd)
  equity <- portfolio_total_ %>% subset(asset_type == "Equity")
  equity_v <- sum(equity$value_usd)
  
  assertthat::are_equal(included_v<=total_v, TRUE)
  assertthat::are_equal(bonds_v+equity_v-included_v<1, TRUE)
  
  total_v <- round(total_v)
  total_v <- as.character(total_v)
  included_v <- round(included_v)
  bonds_v <- round(bonds_v)
  equity_v <- round(equity_v)
  
  keys <- c("\"total\"", "\"included\"", "\"bonds\"", "\"equity\"")
  values <- c(total_v,included_v,bonds_v,equity_v)
  
  
  json_head = "{"
  json_tail = "}"
  
  text_varibles <- json_head
  
  for (i in 1:(length(keys)-1)){
    text_varibles <- paste0(text_varibles," " , keys[i],": ", values[i],",")
  } 
  i = length(keys)
  
  text_varibles <- paste0(text_varibles," " , keys[i],": ", values[i])
  
  text_varibles <- paste0(text_varibles, json_tail)
  
  write(text_varibles, file=paste0(export_path_full,".json"))}
