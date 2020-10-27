library(readr)
library(yaml)

test_cases_dir <- "~/Dropbox (2° Investing)/2° Investing Team/People/Jacob/p2020_test_cases"

test_cases_csvs <- list.files(test_cases_dir, pattern = "\\.csv", full.names = TRUE)

test_cases_output_dir <- "test_cases"

unlink(test_cases_output_dir, recursive = TRUE)

for (i in seq_along(test_cases_csvs)) {
  filepath <- test_cases_csvs[[i]]

  test_case <- read_csv(filepath, col_types = cols())
  filename <- tools::file_path_sans_ext(basename(filepath))
  portfolio_name <- unique(test_case$Portfolio.Name)[[1]]
  investor_name <- unique(test_case$Investor.Name)[[1]]

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
  dir.create(file.path(out_dir, "00_Log_Files"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(out_dir, "10_Parameter_File"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(out_dir, "20_Raw_Inputs"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(out_dir, "30_Processed_Inputs"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(out_dir, "40_Results"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(out_dir, "50_Outputs"), showWarnings = FALSE, recursive = TRUE)


  write_csv(test_case, file.path(out_dir, "20_Raw_Inputs", paste0(portfolio_name, ".csv")))
  write_yaml(yaml_data, file.path(out_dir, "10_Parameter_File", paste0(portfolio_name, "_PortfolioParameters.yml")), indent = 4)
  file.copy("working_dir/10_Parameter_File/AnalysisParameters.yml",
            file.path(out_dir, "10_Parameter_File", "AnalysisParameters.yml"))
}


for (i in seq_along(test_cases_csvs)) {
  filepath <- test_cases_csvs[[i]]
  test_case <- read_csv(filepath, col_types = cols())
  portfolio_name <- unique(test_case$Portfolio.Name)[[1]]
  out_dir <- file.path(test_cases_output_dir, portfolio_name)

  portfolio_name_ref_all <- portfolio_name
  portfolio_root_dir <- out_dir
  
  cli::cli_h1(paste0("running for: ", portfolio_name_ref_all))
  cli::cli_h1(paste0("in: ", portfolio_root_dir))

  source("web_tool_script_1.R")
  source("web_tool_script_2.R")
  source("web_tool_script_3.R")
}
