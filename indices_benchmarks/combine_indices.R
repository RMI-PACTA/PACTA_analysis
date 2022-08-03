# -------------------------------------------------------------------------

portfolios_dir <- "~/Dropbox (2° Investing)/PortCheck_v2/10_Projects/0_Indices/20_Raw_Inputs/2020Q4"

pacta_directories <- c("00_Log_Files", "10_Parameter_File", "20_Raw_Inputs", "30_Processed_Inputs", "40_Results", "50_Outputs")
working_dir <- "working_dir"

output_dir <- "~/Desktop"

holdings_date <- "2020Q4"


# -------------------------------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(fs)
library(cli)
library(yaml)


# -------------------------------------------------------------------------

portfolios <- list.files(portfolios_dir, pattern = "20[0-9]{2}Q[1-4]_.*_Index.*[.]csv", full.names = TRUE)
tmp_dir <- tempdir()
investor_name <- "Indices2020Q4"

for (portfolio in portfolios) {
  fs::dir_delete(working_dir)
  fs::dir_create(file.path(working_dir, pacta_directories))

  file.copy(portfolio, file.path(working_dir, "20_Raw_Inputs", "Indices2020Q4.csv"))

  portfolio_name <- readr::read_csv(portfolio, col_types = "cddccc") %>% pull(portfolio_name) %>% unique()

  config_list <-
    list(
      default = list(
        parameters = list(
          investor_name = investor_name,
          portfolio_name = portfolio_name,
          language = "EN",
          project_code = investor_name,
          holdings_date = holdings_date
        )
      )
    )
  yaml::write_yaml(config_list, file = file.path(working_dir, "10_Parameter_File", paste0("Indices2020Q4", "_PortfolioParameters.yml")))

  portfolio_name_ref_all <- investor_name


  cli::cli_alert_info("running PACTA on: {.emph {portfolio_name}}")
  source("web_tool_script_1.R", local = TRUE)
  source("web_tool_script_2.R", local = TRUE)

  eq_result <- file.path(working_dir, "40_Results", investor_name, "Equity_results_portfolio.rds")
  bond_result <- file.path(working_dir, "40_Results", investor_name, "Bonds_results_portfolio.rds")

  eq_out <- file.path(tmp_dir, paste0(portfolio_name, "_", basename(eq_result)))
  bond_out <- file.path(tmp_dir, paste0(portfolio_name, "_", basename(bond_result)))

  if (file.exists(eq_result)) { file.copy(eq_result, eq_out) }
  if (file.exists(bond_result)) { file.copy(bond_result, bond_out) }
}


# -------------------------------------------------------------------------

output_files <- list.files(tmp_dir, pattern = "[.]rda$", full.names = TRUE)

combined <-
  output_files %>%
  map_dfr(readRDS)

unlink(output_files)

combined <-
  combined %>%
  mutate(portfolio_name = case_when(
    grepl("S&P", portfolio_name) ~ "iShares Core S&P 500 ETF",
    grepl("MSCI World", portfolio_name) ~ "iShares MSCI World ETF",
    grepl("MSCI EM", portfolio_name) ~ "iShares MSCI Emerging Markets ETF",
    grepl("Global Corp Bond", portfolio_name) ~ "iShares Global Corp Bond UCITS ETF",
    grepl("MSCI ACWI", portfolio_name) ~ "iShares MSCI ACWI ETF",
    TRUE ~ portfolio_name
  ))

combined %>%
  filter(!grepl("Global Corp Bond", portfolio_name)) %>%
  saveRDS(file.path(output_dir, "Indices_equity_portfolio.rds"))

combined %>%
  filter(grepl("Global Corp Bond", portfolio_name)) %>%
  saveRDS(file.path(output_dir, "Indices_bonds_portfolio.rds"))
