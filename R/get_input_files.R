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
