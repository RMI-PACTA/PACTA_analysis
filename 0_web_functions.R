# 0_web_functions

set_webtool_paths <- function(){
  
  project_location <<-  paste0(working_location,"web_folders/web_tool")
  
  log_path <<- paste0(project_location,"/00_Log_Files")
  par_file_path <<- paste0(project_location,"/10_Parameter_File")
  raw_input_path <<-  paste0(project_location,"/20_Raw_Inputs")
  proc_input_path <<- paste0(project_location,"/30_Processed_Inputs")
  results_path <<- paste0(project_location,"/40_Results")
  outputs_path <<- paste0(project_location,"/50_Outputs")
  
  
}


set_web_parameters <- function(){
  
  NameInput <- read.csv(paste0("web_folders/ParameterFiles/",portfolio_name_ref,"_Naming.csv"), strip.white = TRUE, stringsAsFactors = FALSE)
  PortfolioName <<- NameInput$PortfolioName
  InvestorName <<- NameInput$InvestorName
  
  TimeInput <- read.csv(paste0("web_folders/ParameterFiles/",portfolio_name_ref,"_TimeStamp.csv"), strip.white = TRUE, stringsAsFactors = FALSE)
  FinDataTimeStamp <<- TimeInput$FinDataTimeStamp

}

add_naming_to_portfolio <- function(portfolio_raw){
  
  portfolio_raw$portfolio_name <- PortfolioName
  portfolio_raw$investor_name <- InvestorName
  
  return(portfolio_raw)
  
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

