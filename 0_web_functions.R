# 0_web_functions

get_portfolio_name <- function(){
  PortfolioNameRef = commandArgs(trailingOnly=TRUE)
  
  if (length(PortfolioNameRef)==0) {
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
  } 
  
  return(PortfolioNameRef)
  
}

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


export_audit_information_jsons <-function(audit_file_ = audit_file, portfolio_total_ = portfolio_total,  folder_path = paste0(proc_input_path), project_name_=NA){
  
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
 
  write(text_varibles, file=paste0(export_path_full,".json"))
  }
