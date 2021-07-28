# 0_web_functions

get_portfolio_name <- function() {
  portfolio_name_ref <- commandArgs(trailingOnly = TRUE)

  if (length(portfolio_name_ref) == 0) {
    stop("At least one argument must be supplied (input file).n", call. = FALSE)
  }

  return(portfolio_name_ref)
}

identify_portfolios <- function(portfolio_total) {
  port_names <- portfolio_total %>%
    select(investor_name, portfolio_name) %>%
    distinct()

  port_names <- port_names %>%
    mutate(
      file_name = gsub(" ", "", portfolio_name),
      loc_name = paste0(portfolio_name_ref_all, "-", file_name)
    )


  if (length(port_names %>% select(investor_name) %>% distinct()) > 1) {
    write_log(
      msg = "There is more than one investor name within a portfolio. Please correct the input data and retry.",
      file_path = log_path
    )
    stop("More than one investor in portfolio")
  }

  return(port_names)
}

create_portfolio_subfolders <- function(portfolio_name_ref_all = NULL, project_location = NULL) {
  folders <- c("00_Log_Files", "30_Processed_Inputs", "40_Results", "50_Outputs")

  locs_to_create <- folders %>%
    purrr::map(~ file.path(project_location, .x, portfolio_name_ref_all)) %>%
    purrr::flatten_chr()

  locs_to_create %>%
    purrr::map(~ dir.create(.x, showWarnings = FALSE))

  invisible(portfolio_name_ref_all)
}

save_if_exists <- function(df, portfolio_name_, save_name, csv_or_rds = "rds") {
  if (data_check(df)) {
    df <- df %>% filter(portfolio_name == all_of(portfolio_name_))
  }

  if (data_check(df)) {
    if (csv_or_rds == "rds") {
      saveRDS(df, save_name)
    } else if (csv_or_rds == "csv") {
      write_csv_file(df, file = save_name)
    }
  }
}

set_webtool_paths <- function(project_root_dir = "working_dir") {
  project_location <<- file.path(working_location, project_root_dir)

  log_path <<- file.path(project_location, "00_Log_Files", portfolio_name_ref_all)
  par_file_path <<- file.path(project_location, "10_Parameter_File")
  raw_input_path <<- file.path(project_location, "20_Raw_Inputs")
  proc_input_path <<- file.path(project_location, "30_Processed_Inputs")
  results_path <<- file.path(project_location, "40_Results")
  outputs_path <<- file.path(project_location, "50_Outputs")
}

set_web_parameters <- function(file_path) {
  cfg <- config::get(file = file_path)

  project_location_ext <<- cfg$paths$project_location_ext
  data_location_ext <<- cfg$paths$data_location_ext
  template_path <<- cfg$paths$template_location
  stress_test_path <<- cfg$paths$stress_test_location
  stress_test_data_location <<- cfg$paths$stress_test_data_location
  user_results_path <<- cfg$paths$user_data_location

  project_name <<- cfg$parameters$project_name
  twodii_internal <<- cfg$parameters$twodii_internal
  new_data <<- cfg$parameters$new_data

  financial_timestamp <<- cfg$parameters$financial_timestamp
}

set_portfolio_parameters <- function(file_path) {
  cfg <- config::get(file = file_path)

  portfolio_name <<- cfg$parameters$portfolio_name
  investor_name <<- cfg$parameters$investor_name
  peer_group <<- cfg$parameters$peer_group
  language_select <<- cfg$parameters$language
  user_id <<- cfg$parameters$user_id
  project_code <<- cfg$parameters$project_code
}

add_naming_to_portfolio <- function(portfolio_raw) {
  portfolio_raw$portfolio_name <- portfolio_name
  portfolio_raw$investor_name <- investor_name

  return(portfolio_raw)
}

get_input_files <- function(portfolio_name_ref_all) {

  portfolio <- tibble()

  input_path <- file.path(project_location, "20_Raw_Inputs")

  # set_portfolio_parameters(file_path = file.path(par_file_path, "PortfolioParameters.yml"))
  # check that the provided reference names match to the input files

  input_files <- list.files(path = input_path, full.names = FALSE)
  input_names <- tools:::file_path_sans_ext(input_files)
  portfolio_files <- grep(portfolio_name_ref_all, input_files,
                          value = TRUE, fixed = TRUE)
  input_file_type <- unique(tools::file_ext(portfolio_files))

  if (!input_file_type %in% c("csv", "xlsx", "txt")) {
    write_log(
      msg = "Input file format not supported. Must be .csv, .xlsx or .txt",
      file_path = log_path
    )
    stop("Input file format not supported. Must be .csv, .xlsx or .txt")
  }

  if (!all(portfolio_name_ref_all %in% input_names)) {
    write_log(
      msg = "Difference in input files and input argument portfolio names.",
      file_path = log_path
    )
    stop("Difference in input files and input argument portfolio names.")
  }

  if (any(!portfolio_name_ref_all %in% input_names)) {
    write_log(
      msg = "Missing input argument",
      file_path = log_path
    )
    stop("Missing input argument")
  }

  portfolio_file_names <- list.files(file.path(project_location, "10_Parameter_File"))
  portfolio_file_names <- portfolio_file_names[grepl("_PortfolioParameters.yml", portfolio_file_names)]
  portfolio_file_names <- gsub("_PortfolioParameters.yml", "", portfolio_file_names)

  if (!all(portfolio_name_ref_all %in% portfolio_file_names)) {
    write_log(
      msg = "Difference in parameter files and input argument portfolio names.",
      file_path = log_path
    )
    stop("Difference in parameter files and input argument portfolio names.")
  }
  if (any(!portfolio_name_ref_all %in% portfolio_file_names)) {
    write_log(
      msg = "Missing portfolio parameter file",
      file_path = log_path
    )
    stop("Missing portfolio parameter file")
  }

  input_file_path <- path(input_path, paste0(portfolio_name_ref_all, ".csv"))

  portfolio <- read_web_input_file(input_file_path)

  portfolio <- portfolio %>% select(-contains("X"))

  set_portfolio_parameters(file_path = file.path(par_file_path, paste0(portfolio_name_ref_all, "_PortfolioParameters.yml")))

  # this writes the portfolio and ivestor names that are provided from the parameter file to the pf
  # as agreed with Constructiva. They ensure grouped portfolios will get one name only.
  portfolio <- portfolio %>%
    mutate(
      Portfolio.Name = portfolio_name,
      Investor.Name = investor_name
    )

  # clean and check column names
  portfolio <- check_input_file_contents(portfolio, portfolio_name, investor_name)

  portfolio <- clean_portfolio_col_types(portfolio)
  portfolio <- clear_portfolio_input_blanks(portfolio)

  if (portfolio %>% pull(investor_name) %>% unique() %>% length() > 1) {
    write_log(
      msg = "Multiple investors detected. Only one investor at a time can be anaylsed",
      file_path = log_path
    )
    stop("Multiple investors detected. Only one investor at a time can be anaylsed")
  }

  return(portfolio)
}

read_web_input_file <- function(input_file_path) {
  file_ext <- tools::file_ext(input_file_path)

  if (file_ext == "csv") {
    input_file <- read_csv(input_file_path, col_types = cols())
  }
  if (file_ext == "xlsx") {
    input_file <- read_xlsx(input_file_path)
  }
  if (file_ext == "txt") {
    enc <- guess_encoding(input_file_path)$encoding[1]
    input_file <- read.table(input_file_path, sep = ",", header = T, fileEncoding = enc)

    if (ncol(input_file) == 1) {
      input_file <- read.table(input_file_path, sep = "\t", header = T, fileEncoding = enc)
    }

    if (ncol(input_file) == 1) {
      input_file <- read.table(input_file_path, sep = ";", header = T, fileEncoding = enc)
    }
  }

  if (data_check(input_file) == FALSE) {
    warning("Input file not readable")
    ifelse(nrow(input_file) == 0,
           write_log(
             msg = "Input file has 0 rows. Please ensure the uploaded file is not empty.",
             file_path = log_path
           ),
           write_log(
             msg = "Input file could not be transformed into a data.frame. Please check the uploaded file has the correct format.",
             file_path = log_path
           )
    )
  }

  return(input_file)
}

check_input_file_contents <- function(portfolio_, portfolio_name, investor_name) {
  portfolio_clean <- clean_colnames_portfolio_input_file(portfolio_)

  necessary_columns <- c(grouping_variables, "market_value", "currency", "isin")

  # if portfolio_name and investor_name are not among the columns of the input file, they are created using the values from the parameter file
  if (!"portfolio_name" %in% colnames(portfolio_clean)) {
    portfolio_clean$portfolio_name <- portfolio_name
  }
  if (!"investor_name" %in% colnames(portfolio_clean)) {
    portfolio_clean$investor_name <- investor_name
  }

  if (length(setdiff(necessary_columns, colnames(portfolio_clean))) > 0) {
    missing_cols <- setdiff(necessary_columns, colnames(portfolio_clean))

    write_log(
      msg = paste(
        "The uploaded portfolio file contains the following missing variables:", str_c(missing_cols, collapse = ", "),
        "\n For correct analysis, please ensure the following required variables are included in your uploaded portfolio file:",
        str_c(necessary_columns, collapse = ", ")
      ),
      file_path = log_path
    )
    stop(paste0("Missing inputs for this portfolio: ", str_c(missing_cols, collapse = ", ")))
  }

  return(portfolio_clean)
}

website_text <- function(audit_file, proc_input_path) {
  PortValues <- audit_file %>%
    ungroup() %>%
    filter(valid_input == TRUE) %>%
    summarize(PortTot = sum(value_usd))

  Asset.Values <- audit_file %>%
    ungroup() %>%
    group_by(asset_type) %>%
    filter(valid_input == TRUE) %>%
    summarise(Tot = sum(value_usd), .groups = "drop_last")

  asset_types <- c("Equity", "Bonds")

  if (length(setdiff(asset_types, Asset.Values$asset_type)) > 0) {
    newrow <- data.frame(asset_type = setdiff(asset_types, Asset.Values$asset_type), Tot = 0)
    Asset.Values <- rbind(Asset.Values, newrow)
  }

  Bonds.Value <- prettyNum(round(Asset.Values$Tot[Asset.Values$asset_type == "Bonds"], 0), big.mark = ",", scientific = FALSE)

  Equity.Value <- prettyNum(round(Asset.Values$Tot[Asset.Values$asset_type == "Equity"], 0), big.mark = ",", scientific = FALSE)

  text <- paste0("The portfolio you have uploaded has $", prettyNum(round(PortValues[[1]], 0), big.mark = ",", scientific = FALSE), " USD in holdings.
                 Of which $", Bonds.Value, " USD is in bonds,
                and $", Equity.Value, " USD is in equity.

                The remainder of the holdings are in asset classes outside the scope of this analysis.
                For more information as to how each holding is classified, review the chart and audit file below.")

  write(text, file = file.path(proc_input_path, "Websitetext.txt"))
}

save_cleaned_files <- function(save_loc,
                               currencies,
                               fund_data,
                               fin_data,
                               comp_fin_data,
                               debt_fin_data,
                               average_sector_intensity,
                               company_emissions,
                               total_fund_list = NULL) {
  if (!dir.exists(save_loc)) {
    dir.create(save_loc)
  }

  fst::write_fst(as.data.frame(currencies), file.path(save_loc, "currencies.fst"))
  fst::write_fst(fund_data, file.path(save_loc, "fund_data.fst"))
  fst::write_fst(fin_data, file.path(save_loc, "fin_data.fst"))
  fst::write_fst(comp_fin_data, file.path(save_loc, "comp_fin_data.fst"))
  fst::write_fst(debt_fin_data, file.path(save_loc, "debt_fin_data.fst"))
  fst::write_fst(average_sector_intensity, file.path(save_loc, "average_sector_intensity.fst"))
  fst::write_fst(company_emissions, file.path(save_loc, "company_emissions.fst"))
  if (!is.null(total_fund_list)) {
    fst::write_fst(total_fund_list, file.path(save_loc, "total_fund_list.fst"))
  }

  if (check_file_size(save_loc)) warning("File size exceeds what can be pushed to GitHub. Check before Committing")
}

check_file_size <- function(folder_to_check) {
  files_to_check <- list.files(folder_to_check, full.names = T)
  any(file.size(files_to_check) > 100e6)
}

empty_portfolio_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "scenario" = NA_character_, "allocation" = NA_character_,
    "equity_market" = NA_character_, "scenario_geography" = NA_character_,
    "year" = NA_integer_, "ald_sector" = NA_character_, "technology" = NA_character_,
    "plan_tech_prod" = NA_integer_, "plan_alloc_wt_tech_prod" = NA_integer_,
    "plan_carsten" = NA_integer_, "plan_emission_factor" = NA_integer_,
    "scen_tech_prod" = NA_integer_, "scen_alloc_wt_tech_prod" = NA_integer_,
    "scen_carsten" = NA_integer_, "scen_emission_factor" = NA_integer_,
    "plan_sec_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "plan_sec_carsten" = NA_integer_, "plan_sec_emissions_factor" = NA_integer_,
    "scen_sec_prod" = NA_integer_, "scen_alloc_wt_sec_prod" = NA_integer_,
    "scen_sec_carsten" = NA_integer_, "scen_sec_emissions_factor" = NA_integer_,
    "plan_tech_share" = NA_integer_, "scen_tech_share" = NA_integer_,
    "trajectory_deviation" = NA_integer_, "trajectory_alignment" = NA_integer_
  )
}

empty_company_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "scenario" = NA_character_, "allocation" = NA_character_,
    "id" = NA_character_, "company_name" = NA_character_,
    "financial_sector" = NA_character_, "port_weight" = NA_integer_,
    "allocation_weight" = NA_integer_, "plan_br_dist_alloc_wt" = NA_integer_,
    "scen_br_dist_alloc_wt" = NA_integer_,
    "equity_market" = NA_character_, "scenario_geography" = NA_character_,
    "year" = NA_integer_, "ald_sector" = NA_character_, "technology" = NA_character_,
    "plan_tech_prod" = NA_integer_, "plan_alloc_wt_tech_prod" = NA_integer_,
    "plan_carsten" = NA_integer_, "plan_emission_factor" = NA_integer_,
    "scen_tech_prod" = NA_integer_, "scen_alloc_wt_tech_prod" = NA_integer_,
    "scen_carsten" = NA_integer_, "scen_emission_factor" = NA_integer_,
    "plan_sec_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "plan_sec_carsten" = NA_integer_, "plan_sec_emissions_factor" = NA_integer_,
    "scen_sec_prod" = NA_integer_, "scen_alloc_wt_sec_prod" = NA_integer_,
    "scen_sec_carsten" = NA_integer_, "scen_sec_emissions_factor" = NA_integer_,
    "plan_tech_share" = NA_integer_, "scen_tech_share" = NA_integer_,
    "trajectory_deviation" = NA_integer_, "trajectory_alignment" = NA_integer_
  )
}

empty_emissions_results <- function(){
  tibble("investor_name" = NA_character_, "portfolio_name" = NA_character_,
         "asset_type" = NA_character_, "sector" = NA_character_,
         "weighted_sector_emissions" = NA_real_)
}

empty_audit_file <- function(){
  tibble("investor_name" = NA_character_, "portfolio_name" = NA_character_,
         "asset_type" = NA_character_, "valid_input" = NA, "isin" = NA_character_,
         "direct_holding" = NA, "value_usd" = NA_real_)
}

empty_map_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "ald_location" = NA_character_, "year" = NA_integer_,
    "ald_sector" = NA_character_, "technology" = NA_character_,
    "financial_sector" = NA_character_, "allocation" = NA_character_,
    "allocation_weight" = NA_integer_, "ald_production_unit" = NA_character_,
    "plan_alloc_wt_tech_prod" = NA_integer_, "plan_alloc_wt_sec_prod" = NA_integer_,
    "equity_market" = NA_character_, "scenario" = NA_character_,
    "scenario_geography" = NA_character_
  )
}

empty_st_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "ald_sector" = NA_character_, "technology" = NA_character_,
    "scenario_geography" = NA_character_, "VaR_technology" = NA_real_,
    "asset_portfolio_value" = NA_real_, "VaR_Sector" = NA_real_,
    "scenario_name" = NA_character_, "technology_exposure" = NA_real_,
    "ector_exposure" = NA_real_, "sector_loss" = NA_real_,
    "climate_relevant_var" = NA_real_, "portfolio_aum" = NA_real_,
    "portfolio_loss_percentage" = NA_real_, "year_of_shock" = NA_integer_,
    "duration_of_shock" = NA_integer_, "production_shock_percentage" = NA_real_
  )
}

empty_ipr_st_results <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "sector" = NA_character_, "subsector" = NA_character_,
    "exposure" = NA_real_, "description" = NA_character_,
    "scenario" = NA_character_, "shock" = NA_real_,
    "loss" = NA_real_
  )
}

empty_portfolio_overview <- function(){
  tibble(
    "investor_name" = NA_character_, "portfolio_name" = NA_character_,
    "asset_type" = NA_character_, "financial_sector" = NA_character_,
    "valid_input" = NA, "valid_value_usd" = NA_real_,
    "asset_value_usd" = NA_real_, "portfolio_value_usd" = NA_real_
  )
}

