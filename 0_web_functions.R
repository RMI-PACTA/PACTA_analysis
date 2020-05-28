# 0_web_functions

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



