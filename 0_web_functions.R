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
    write_log("There is more than one investor name within a portfolio. Please correct the input data and retry.")
    stop("More than one investor in portfolio")
  }

  return(port_names)
}

create_portfolio_subfolders <- function(portfolio_name_ref_all) {
  folders <- c("30_Processed_Inputs", "40_Results", "50_Outputs")

  locs_to_create <- folders %>%
    purrr::map(~ paste0(project_location, "/", .x, "/", portfolio_name_ref_all)) %>%
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
      write_rds(df, save_name)
    } else if (csv_or_rds == "csv") {
      write_csv(df, save_name)
    }
  }
}

set_webtool_paths <- function() {
  project_location <<- file.path(working_location, "working_dir")

  log_path <<- file.path(project_location, "00_Log_Files")
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

  input_files <- list.files(path = input_path, full.names = F)
  input_names <- gsub(".csv", "", input_files)
  input_names <- gsub(".txt", "", input_names)
  input_names <- gsub(".xlsx", "", input_names)

  input_file_type <- unique(tools::file_ext(grep(portfolio_name_ref_all, list.files(path = input_path, full.names = F), value = T)))
  if (!input_file_type %in% c("csv", "xlsx", "txt")) {
    write_log(msg = "Input file format not supported. Must be .csv, .xlsx or .txt")
    stop("Input file format not supported. Must be .csv, .xlsx or .txt")
  }


  if (!all(portfolio_name_ref_all %in% input_names)) {
    write_log(msg = "Difference in input files and input argument portfolio names.")
    stop("Difference in input files and input argument portfolio names.")
  }

  if (any(!portfolio_name_ref_all %in% input_names)) {
    write_log(msg = "Missing input argument")
    stop("Missing input argument")
  }

  portfolio_file_names <- list.files(file.path(project_location, "10_Parameter_File"))
  portfolio_file_names <- portfolio_file_names[grepl("_PortfolioParameters.yml", portfolio_file_names)]
  portfolio_file_names <- gsub("_PortfolioParameters.yml", "", portfolio_file_names)

  if (!all(portfolio_name_ref_all %in% portfolio_file_names)) {
    write_log(msg = "Difference in parameter files and input argument portfolio names.")
    stop("Difference in parameter files and input argument portfolio names.")
  }
  if (any(!portfolio_name_ref_all %in% portfolio_file_names)) {
    write_log(msg = "Missing portfolio parameter file")
    stop("Missing portfolio parameter file")
  }
  # this is already tested above -- duplicate
  # if(any(!portfolio_name_ref_all %in% input_names)){stop("Missing input argument")}


  for (i in 1:length(portfolio_name_ref_all)) {
    input_file_path <- file.path(input_path, input_files[i])
    portfolio_name_ref <- portfolio_name_ref_all[i]

    portfolio_ <- read_web_input_file(input_file_path)

    portfolio_ <- portfolio_ %>% select(-contains("X"))

    set_portfolio_parameters(file_path = file.path(par_file_path, paste0(portfolio_name_ref, "_PortfolioParameters.yml")))

    # this writes the portfolio and ivestor names that are provided from the parameter file to the pf
    # as agreed with Constructiva. They ensure grouped portfolios will get one name only.
    portfolio_ <- portfolio_ %>%
      mutate(
        Portfolio.Name = portfolio_name,
        Investor.Name = investor_name
      )

    # clean and check column names
    portfolio_ <- check_input_file_contents(portfolio_, portfolio_name, investor_name)

    portfolio_$count <- i

    portfolio <- rbind(portfolio, portfolio_)
    # create warning here for user.
  }
  portfolio <- clean_portfolio_col_types(portfolio)
  portfolio <- clear_portfolio_input_blanks(portfolio)

  if (portfolio %>% pull(investor_name) %>% unique() %>% length() > 1) {
    write_log(msg = "Multiple investors detected. Only one investor at a time can be anaylsed")
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
      write_log(msg = "Input file has 0 rows. Please ensure the uploaded file is not empty."),
      write_log(msg = "Input file could not be transformed into a data.frame.
                     Please check the uploaded file has the correct format.")
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

    write_log(msg = paste(
      "The uploaded portfolio file contains the following missing variables:", str_c(missing_cols, collapse = ", "),
      "\n For correct analysis, please ensure the following required variables are included in your uploaded portfolio file:",
      str_c(necessary_columns, collapse = ", ")
    ))
    stop(paste0("Missing inputs for this portfolio: ", str_c(missing_cols, collapse = ", ")))
  }

  return(portfolio_clean)
}

website_text <- function(audit_file) {
  PortValues <- audit_file %>%
    ungroup() %>%
    filter(valid_input == TRUE) %>%
    summarize(PortTot = sum(value_usd))

  Asset.Values <- audit_file %>%
    ungroup() %>%
    group_by(asset_type) %>%
    filter(valid_input == TRUE) %>%
    summarise(Tot = sum(value_usd))

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

  write(text, file.path(proc_input_path, "Websitetext.txt"))
}

save_cleaned_files <- function(save_loc,
                               currencies,
                               fund_data,
                               fin_data,
                               comp_fin_data,
                               debt_fin_data,
                               average_sector_intensity,
                               company_emissions) {
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

  if (check_file_size(save_loc)) warning("File size exceeds what can be pushed to GitHub. Check before Committing")
}

check_file_size <- function(folder_to_check) {
  files_to_check <- list.files(folder_to_check, full.names = T)
  any(file.size(files_to_check) > 100e6)
}
