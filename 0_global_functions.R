# Checks whether a variable is a dataframe. Considers also logicals and null values.
data_check <- function(df) {
  if (is.data.frame(df)) {
    if (nrow(df) > 0) {
      check <- TRUE
    } else {
      check <- FALSE
    }
  } else {
    check <- FALSE
  }

  return(check)
}

# Checks whether a value is null or blank
is_blank_na <- function(x) {
  if (is.na(x) | x == "") {
    flag <- TRUE
  } else {
    flag <- FALSE
  }
  flag
}

set_location <- function() {
  if (rstudioapi::isAvailable()) {
    working_location <- dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    working_location <- getwd()
  }

  working_location <- paste0(working_location, "/")

  return(working_location)
}

set_col_types <- function(grouping_variables, fixed_col_types) {

  # defines the column types based on the number of grouping_variables
  port_col_types <- paste0(paste0(rep("c", length(grouping_variables)), collapse = ""), fixed_col_types)

  return(port_col_types)
}

set_project_parameters <- function(file_path){
  cfg <- config::get(file = file_path)

  project_report_name <<- cfg$reporting$project_report_name
  display_currency <<- cfg$reporting$display_currency
  currency_exchange_value <<- as.numeric(cfg$reporting$currency_exchange_value)

  financial_timestamp <<- cfg$parameters$timestamp
  dataprep_timestamp <<- cfg$parameters$dataprep_timestamp

  start_year <<- as.numeric(cfg$parameters$start_year)
  time_horizon <<- as.numeric(cfg$parameters$horizon_year)

  select_scenario <<- cfg$parameters$select_scenario
  scenario_auto <<- cfg$parameters$scenario_auto
  scenario_other <<- cfg$parameters$scenario_other
  scenario_shipping <<- cfg$parameters$scenario_shipping
  portfolio_allocation_method <<- cfg$parameters$portfolio_allocation_method
  scenario_geography <<- cfg$parameters$scenario_geography

  tech_roadmap_sectors <<- cfg$sectors$tech_roadmap_sectors
  pacta_sectors_not_analysed <<- cfg$sectors$pacta_sectors_not_analysed
  sector_list <<- c(tech_roadmap_sectors, pacta_sectors_not_analysed)

  scenario_sources_list <<- cfg$scenario_sources_list
  scenario_geographies_list <<- cfg$scenario_geography_list
  asset_types <<- cfg$asset_types
  equity_market_list <<- cfg$equity_market_list

  grouping_variables <<- cfg$grouping_variables

  green_techs <<- cfg$sectors$green_techs
  alignment_techs <<- cfg$sectors$alignment_techs

  shock_year <<- cfg$stress_test$shock_year
  price_data_version <<- cfg$stress_test$price_data_version


  # meta_investor_name <<- cfg$ComparisonBenchmarks$MetaInvestorName
  # meta_portfolio_name <<- cfg$ComparisonBenchmarks$MetaPortfolioName

  inc_meta_portfolio <<- cfg$ComparisonBenchmarks$CreateMetaPortfolio
  if (is.null(inc_meta_portfolio)) {
    inc_meta_portfolio <<- FALSE
  }

  has_map <<- cfg$methodology$has_map
  if (is.null(has_map)) {
    has_map <<- TRUE
    warning("Warning: has_map set to standard value (TRUE) as it is not defined in the parameter file")
  }

  has_sb <<- cfg$methodology$has_sb
  if (is.null(has_sb)) {
    has_sb <<- FALSE
    warning("Warning: has_sb set to standard value (FALSE) as it is not defined in the parameter file")
  }

  has_credit <<- cfg$methodology$has_credit
  if (is.null(has_credit)) {
    has_credit <<- FALSE
    warning("Warning: has_credit set to standard value (FALSE) as it is not defined in the parameter file")
  }

  has_revenue <<- cfg$methodology$has_revenue
  if (is.null(has_revenue)) {
    has_revenue <<- FALSE
    warning("Warning: has_revenue set to standard value (FALSE) as it is not defined in the parameter file")
  }

  inc_emission_factors <<- cfg$methodology$inc_emissionfactors
  if (is.null(inc_emission_factors)) {
    inc_emission_factors <<- FALSE
    warning("Warning: inc_emission_factors set to standard value (FALSE) as it is not defined in the parameter file")
  }

  inc_stresstest <<- cfg$methodology$inc_stresstest
  if (is.null(inc_stresstest)) {
    inc_stresstest <<- FALSE
    warning("Warning: inc_stresstest set to standard value (FALSE) as it is not defined in the parameter file")
  }

}

set_global_parameters <- function(file_path) {
  cfg <- config::get(file = file_path)

  grouping_variables <<- cfg$GroupingVariables

  financial_timestamp <<- cfg$TimeStamps$FinancialData.Timestamp
  if (is.null(financial_timestamp)) {
    stop("Error: No Financial Timestamp is defined in the parameter file. Please add a FinancialData.Timestamp!")
  }

  ald_timestamp <<- cfg$TimeStamps$ALD.Timestamp
  if (is.null(ald_timestamp)) {
    stop("Error: No Asset level Data Timestamp is defined in the parameter file. Please add a ALD.Timestamp!")
  }

  datastore_timestamp <<- cfg$TimeStamps$DataStore.Export

  dataprep_timestamp <<- cfg$TimeStamps$DataPrep.Timestamp
  if (is.null(dataprep_timestamp)) {
    stop("Error: No Analysis Inputs Timestamp is defined in the parameter file. Please add a dataprep_timestamp in the parameter file!")
  }

  start_year <<- cfg$AnalysisPeriod$Years.Startyear
  time_horizon <<- cfg$AnalysisPeriod$Years.Horizon
  risk_year <<- cfg$AnalysisPeriod$Years.Riskyear
  additional_year <<- cfg$AnalysisPeriod$Years.Additional

  tech_list <<- cfg$Lists$Technology.List
  tech_exclude <<- cfg$Lists$Technology.Exclusion.List
  sector_list <<- cfg$Lists$TechnologyRoadmap.Sector.List
  other_sector_list <<- cfg$Lists$CO2Intensity.Sector.List

  scenario_sources_list <<- cfg$Lists$Scenario.Sources.List
  iea_scenario_list <<- cfg$Lists$IEA.Scenarios.List
  web_region_list <<- cfg$Lists$WebToolRegions
  scenario_geographies_list <<- cfg$Lists$Scenario.Geography.List

  equity_market_list <<- cfg$Lists$Equity.Market.List

  allowable_asset_list <<- cfg$Lists$AssetTypes
  if (is.null(allowable_asset_list)) {
    allowable_asset_list <<- c("Funds", "Equity", "Bonds", "Others")
  }
  # allowable_asset_list <<- allowable_asset_list

  global_aggregate_sector_list <<- cfg$Lists$Global.Aggregate.Sector.List
  global_aggregate_scenario_sources_list <<- cfg$Lists$Global.Aggregate.Scenario.Sources.List


  meta_investor_name <<- cfg$ComparisonBenchmarks$MetaInvestorName
  meta_portfolio_name <<- cfg$ComparisonBenchmarks$MetaPortfolioName

  inc_meta_portfolio <<- cfg$ComparisonBenchmarks$CreateMetaPortfolio
  if (is.null(inc_meta_portfolio)) {
    inc_meta_portfolio <<- FALSE
  }

  has_map <<- cfg$Methodology$HasMAP
  if (is.null(has_map)) {
    has_map <<- TRUE
    warning("Warning: has_map set to standard value (TRUE) as not defined in the parameter file")
  }

  has_sb <<- cfg$Methodology$HasSB
  if (is.null(has_sb)) {
    has_sb <<- FALSE
    warning("Warning: has_sb set to standard value (FALSE) as not defined in the parameter file")
  }

  has_credit <<- cfg$Methodology$HasCC
  if (is.null(has_credit)) {
    has_credit <<- FALSE
    warning("Warning: has_credit set to standard value (FALSE) as not defined in the parameter file")
  }

  has_revenue <<- cfg$Methodology$HasRevenue
  if (is.null(has_revenue)) {
    has_revenue <<- TRUE
    warning("Warning: has_revenue set to standard value (TRUE) as not defined in the parameter file")
  }

  inc_emission_factors <<- cfg$Methodology$IncEmissionFactors
  if (is.null(inc_emission_factors)) {
    inc_emission_factors <<- FALSE
    warning("Warning: inc_emission_factors set to standard value (inc_emission_factors) as not defined in the parameter file")
  }

  file_format_list <<- tolower(cfg$data_output$file_type)
  if (is.null(file_format_list) | length(file_format_list) == 0) {
    file_format_list <<- c("rda")
    warning("Warning: file_format_list set to standard value ('rda') as not defined in the parameter file")
  }
}

set_project_paths <- function(project_name, twodii_internal, project_location_ext) {

  # portcheck_v2_path <<- path_dropbox_2dii("PortCheck_v2")
  project_location <<- ifelse(twodii_internal,
    r2dii.utils::path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name),
    paste0(project_location_ext, "/", project_name)
  )

  log_path <<- paste0(project_location, "/00_Log_Files")
  par_file_path <<- paste0(project_location, "/10_Parameter_File")
  raw_input_path <<- paste0(project_location, "/20_Raw_Inputs")
  proc_input_path <<- paste0(project_location, "/30_Processed_Inputs")
  results_path <<- paste0(project_location, "/40_Results")
  outputs_path <<- paste0(project_location, "/50_Outputs")
}

set_git_path <- function() {
  if (rstudioapi::isAvailable()) {
    git_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
  } else {
    git_path <- getwd()
  }

  git_path <- gsub("?", "", git_path)
  git_path <- paste0(git_path, "/")

  git_path
}

set_analysis_inputs_path <- function(twodii_internal, data_location_ext, dataprep_ref = datastore_timestamp) {

  if (twodii_internal) {
    analysis_inputs_path <- r2dii.utils::path_dropbox_2dii("PortCheck", "00_Data", "07_AnalysisInputs", dataprep_ref)
    analysis_inputs_path <- file.path(analysis_inputs_path)
  } else {
    analysis_inputs_path <- data_location_ext
  }

  return(analysis_inputs_path)
}

set_data_paths <- function(financial_timestamp = financial_timestamp, dataprep_timestamp = dataprep_timestamp, ald_timestamp = ald_timestamp) {
  data_path <<- r2dii.utils::path_dropbox_2dii("PortCheck", "00_Data")
  data_store_path <<- r2dii.utils::path_dropbox_2dii("PortCheck", "00_Data", "06_DataStore", datastore_timestamp, ald_timestamp)
  scenario_data_path <<- r2dii.utils::path_dropbox_2dii("PortCheck", "00_Data", "01_ProcessedData", "03_ScenarioData")
  master_data_path <<- r2dii.utils::path_dropbox_2dii("PortCheck", "00_Data", "01_ProcessedData", "01_SectorMasters", ald_timestamp)
  general_fin_path <<- r2dii.utils::path_dropbox_2dii("PortCheck", "00_Data", "02_FinancialData")
  sb_data_path <<- r2dii.utils::path_dropbox_2dii("PortCheck", "00_Data", "04_Other", "1_SovereignBonds")
}

copy_files <- function(project_name) {
  folder_location <- paste0(set_git_path(), "parameter_files")

  input_file <- paste0(raw_input_path, "/", project_name, "_Input.csv")
  parameter_file <- paste0(par_file_path, "/ReportParameters.yml")
  yml_file <- paste0(par_file_path, "/ProjectParameters.yml")

  if (!file.exists(input_file)) {
    file.copy(paste0(folder_location, "/ProjectName_Input.csv"), input_file, overwrite = F)
  }

  if (!file.exists(parameter_file)) {
    file.copy(paste0(folder_location, "/ReportParameters.yml"), parameter_file, overwrite = F)
  }

  if (!file.exists(yml_file)) {
    file.copy(paste0(folder_location, "/ProjectParameters.yml"), yml_file, overwrite = F)
  }
}

create_project_folder <- function(project_name, twodii_internal, project_location_ext, working_location = working_location) {
  project_location <- ifelse(twodii_internal,
    r2dii.utils::path_dropbox_2dii("PortCheck_v2", "10_Projects", project_name),
    paste0(project_location_ext, "/", project_name)
  )

  # Create the new project folder
  if (dir.exists(project_location)) {
    print("Project Folder Already Exists")
  } else {
    project_folders <- c(
      "00_Log_Files",
      "10_Parameter_File",
      "20_Raw_Inputs",
      "30_Processed_Inputs",
      "40_Results",
      "50_Outputs"
    )

    project_folders <- paste0(project_location, "/", project_folders)

    dir.create(project_location)
    lapply(project_folders, function(x) dir.create(x))

    # Copy in Parameter File
    file.copy(
      paste0(working_location, "parameter_files/AnalysisParameters.yml"),
      paste0(project_location, "/10_Parameter_File/AnalysisParameters.yml")
    )
  }
}

create_results_folder <- function(project_name, investor_name_select, portfolio_name_select, report_handle) {
  investor_folder <- paste0(outputs_path, "/", investor_name_select, "/")
  portfolio_folder <- paste0(investor_folder, "/", portfolio_name_select, "/")
  report_folder <- paste0(portfolio_folder, report_handle, "/")

  folder_structure <- paste0(getwd(), "/", "sample_files/10_folder_structures/results_folders")

  folder_structure <- paste0(folder_structure, "/")
  # Create the new project folder
  if (!dir.exists(investor_folder)) {
    dir.create(investor_folder)
  }

  if (!dir.exists(portfolio_folder)) {
    dir.create(portfolio_folder)
    # a <- list.dirs(folder_structure)
    # b <- basename(a)[-1]
    # c <- paste0(portfolio_folder,b)
    # lapply(c, function(x) dir.create(x))
  }

  if (!dir.exists(report_folder)) {
    dir.create(report_folder)
  }

  report_path <<- report_folder
}

first_char_up <- function(x) {
  x <- paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  x
}


# write error log for input portfolio - msg should be a string containing the error message
write_log <- function(msg, file_path = log_path, ...) {
  composed <- paste(
    as.character(Sys.time()),
    as.character(msg),
    ...
  )
  write(composed, file = file.path(file_path,"error_messages.txt"), append = TRUE)
}

