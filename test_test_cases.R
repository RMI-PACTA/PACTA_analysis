library(readr)
library(yaml)

test_cases_dir <- "~/Dropbox (2° Investing)/2° Investing Team/People/Jacob/p2020_test_cases"

test_cases_csvs <- list.files(test_cases_dir, pattern = "[.]csv", full.names = TRUE)

test_cases_output_dir <- "test_cases"

unlink(test_cases_output_dir, recursive = TRUE)

for (i in seq_along(test_cases_csvs)) {
  filepath <- test_cases_csvs[[i]]

  test_case <- read_csv(filepath, col_types = cols())
  filename <- tools::file_path_sans_ext(basename(filepath))
  portfolio_name <- unique(test_case$Portfolio.Name)
  investor_name <- unique(test_case$Investor.Name)

  # if no unique, valid portfolio_name or investor_name, use the filename
  if (length(portfolio_name) != 1) { portfolio_name <- filename }
  if (length(investor_name) != 1) { investor_name <- filename }

  yaml_data <-
    list(default = list(
      parameters =
        list(
          portfolio_name_in = portfolio_name,
          investor_name_in = investor_name,
          peer_group = "pensionfund",
          language = "EN",
          project_code = "CHPA2020"
        )
    ))


  out_dir <- file.path(test_cases_output_dir, portfolio_name)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  sub_directories_needed <-
    c("00_Log_Files", "10_Parameter_File", "20_Raw_Inputs",
      "30_Processed_Inputs", "40_Results", "50_Outputs")

  lapply(sub_directories_needed, function(sub_dir) {
    fs::dir_create(fs::path(out_dir, sub_dir), recurse = TRUE)
  })

  write_csv(test_case, file.path(out_dir, "20_Raw_Inputs", paste0(portfolio_name, ".csv")))
  write_yaml(yaml_data, file.path(out_dir, "10_Parameter_File", paste0(portfolio_name, "_PortfolioParameters.yml")), indent = 4)
  file.copy("working_dir/10_Parameter_File/AnalysisParameters.yml",
            file.path(out_dir, "10_Parameter_File", "AnalysisParameters.yml"))
}


for (csv_num in seq_along(test_cases_csvs)) {
  filepath <- test_cases_csvs[[csv_num]]
  test_case <- read_csv(filepath, col_types = cols())
  filename <- tools::file_path_sans_ext(basename(filepath))
  portfolio_name <- unique(test_case$Portfolio.Name)
  sub_directory <- portfolio_name

  # if no unique, valid portfolio_name, use an empty string
  if (length(portfolio_name) != 1) { portfolio_name <- "" }

  # if no unique, valid portfolio_name, use the filename
  if (length(sub_directory) != 1) { sub_directory <- filename }

  out_dir <- file.path(test_cases_output_dir, sub_directory)

  portfolio_name_ref_all <- portfolio_name
  portfolio_root_dir <- out_dir

  cli::cli_h1(paste0("running for: ", portfolio_name_ref_all, " (test #", csv_num, ")"))
  cli::cli_h1(paste0("in: ", portfolio_root_dir))

  try(source("web_tool_script_1.R"))
  try(source("web_tool_script_2.R"))
  try(source("web_tool_script_3.R"))
}
