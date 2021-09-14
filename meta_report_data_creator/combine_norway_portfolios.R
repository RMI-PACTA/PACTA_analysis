
# manually set certain values and paths -----------------------------------

output_dir <- normalizePath("~/Desktop/NORWAY", mustWork = FALSE)

portfolios_path <- "~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/05_PACTACOP_NO/04_Input_Cleaning/portfolios"
portfolios_meta_csv <- "~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/05_PACTACOP_NO/04_Input_Cleaning/portfolios.csv"
users_meta_csv <- "~/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/05_PACTACOP_NO/04_Input_Cleaning/users.csv"

project_code <- "PA2021NO"
default_language <- "EN"

project_prefix <- "norway"

local_PACTA_git_path <- "~/Documents/git/PACTA_analysis"


# check paths and directories ---------------------------------------------

dir.create(output_dir, showWarnings = FALSE)
stopifnot(dir.exists(portfolios_path))
stopifnot(file.exists(portfolios_meta_csv))
stopifnot(file.exists(users_meta_csv))
stopifnot(dir.exists(local_PACTA_git_path))


# load required packages --------------------------------------------------

library(dplyr)
library(purrr)
library(stringr)
library(fs)
library(readr)
library(yaml)

source("meta_report_data_creator/read_portfolio_csv.R")


# prepare alist of all the CSVs to import ---------------------------------

portfolio_csvs <- list.files(portfolios_path, pattern = "[.]csv$", full.names = TRUE)

# remove bogus csvs
portfolio_csvs <- portfolio_csvs[!grepl("20303[.]csv$", portfolio_csvs)]
portfolio_csvs <- portfolio_csvs[!grepl("26102[.]csv$", portfolio_csvs)]


# read in all the CSVs ----------------------------------------------------

data <- map_dfr(set_names(portfolio_csvs, portfolio_csvs), read_portfolio_csv, .id = "csv_name")


# read in meta data CSVs --------------------------------------------------

portfolios_meta <- read_csv(portfolios_meta_csv, col_types = "nnnccnc")
users_meta <- read_csv(users_meta_csv, col_types = "ncccccn")


# add meta data to full data and save it ----------------------------------

data <-
  data %>%
  mutate(port_id = as.numeric(tools::file_path_sans_ext(basename(csv_name)))) %>%
  left_join(portfolios_meta[, c("id", "user_id")], by = c(port_id = "id")) %>%
  left_join(users_meta[, c("id", "organization_type")], by = c(user_id = "id"))

write.csv(
  data,
  file = file.path(output_dir, paste0(project_prefix, "_full.csv")),
  row.names = FALSE, fileEncoding = "UTF-8"
  )
saveRDS(data, file.path(output_dir, paste0(project_prefix, "_full.rds")))


# prepare meta PACTA project ----------------------------------------------

meta_output_dir <- file.path(output_dir, "meta")
dir.create(meta_output_dir, showWarnings = FALSE)

dir.create(file.path(meta_output_dir, "00_Log_Files"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(meta_output_dir, "10_Parameter_File"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(meta_output_dir, "20_Raw_Inputs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(meta_output_dir, "30_Processed_Inputs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(meta_output_dir, "40_Results"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(meta_output_dir, "50_Outputs"), recursive = TRUE, showWarnings = FALSE)

data %>%
  mutate(portfolio_name = "Meta Portfolio") %>%
  mutate(investor_name = "Meta Investor") %>%
  write.csv(
    file = file.path(meta_output_dir, "20_Raw_Inputs", paste0(project_prefix, "_meta.csv")),
    row.names = FALSE, fileEncoding = "UTF-8"
  )

config_list <-
  list(
    default = list(
      parameters = list(
        portfolio_name_in = "Meta Portfolio",
        investor_name_in = "Meta Investor",
        peer_group = paste0(project_prefix, "_meta"),
        language = default_language,
        project_code = project_code
      )
    )
  )
write_yaml(config_list, file = file.path(meta_output_dir, "10_Parameter_File", paste0(project_prefix, "_meta", "_PortfolioParameters.yml")))


# slices for per port_id --------------------------------------------------

ports_output_dir <- file.path(output_dir, "port_id")
dir.create(ports_output_dir, showWarnings = FALSE)

all_port_ids <- unique(data$port_id)

for (port_id in all_port_ids) {
  port_data <- data %>% dplyr::filter(port_id == .env$port_id)

  portfolio_name <- encodeString(as.character(unique(port_data$portfolio_name)))
  if (length(portfolio_name) > 1) { portfolio_name <- port_id }

  investor_name <- encodeString(as.character(unique(port_data$investor_name)))
  if (length(investor_name) > 1) { investor_name <- investor_name[[1]] }

  peer_group <- unique(port_data$organization_type)
  if (length(peer_group) > 1) { peer_group <- peer_group[[1]] }

  config_list <-
    list(
      default = list(
        parameters = list(
          portfolio_name_in = portfolio_name,
          investor_name_in = investor_name,
          peer_group = peer_group,
          language = default_language,
          project_code = project_code
        )
      )
    )

  port_id_output_dir <- file.path(ports_output_dir, paste0(project_prefix, "_port_", port_id))
  dir.create(file.path(port_id_output_dir, "00_Log_Files"), recursive = TRUE)
  dir.create(file.path(port_id_output_dir, "10_Parameter_File"), recursive = TRUE)
  dir.create(file.path(port_id_output_dir, "20_Raw_Inputs"), recursive = TRUE)
  dir.create(file.path(port_id_output_dir, "30_Processed_Inputs"), recursive = TRUE)
  dir.create(file.path(port_id_output_dir, "40_Results"), recursive = TRUE)
  dir.create(file.path(port_id_output_dir, "50_Outputs"), recursive = TRUE)

  write_yaml(config_list, file = file.path(port_id_output_dir, "10_Parameter_File", paste0(project_prefix, "_port_", port_id, "_PortfolioParameters.yml")))

  port_data %>%
    select(investor_name, portfolio_name, isin, market_value, currency) %>%
    write_csv(file.path(port_id_output_dir, "20_Raw_Inputs", paste0(project_prefix, "_port_", port_id, ".csv")))
}


# slices for per user_id --------------------------------------------------

users_output_dir <- file.path(output_dir, "user_id")
dir.create(users_output_dir, showWarnings = FALSE)

all_user_ids <- unique(data$user_id)

for (user_id in all_user_ids) {
  user_data <- data %>% dplyr::filter(user_id == .env$user_id)

  investor_name <- encodeString(as.character(unique(user_data$investor_name)))
  if (length(investor_name) > 1) {
    investor_name <- investor_name[[1]]
    user_data <- user_data %>% mutate(investor_name = .env$investor_name)
  }

  user_data <- user_data %>% mutate(portfolio_name = .env$investor_name)

  peer_group <- unique(user_data$organization_type)
  if (length(peer_group) > 1) { peer_group <- peer_group[[1]] }

  config_list <-
    list(
      default = list(
        parameters = list(
          portfolio_name_in = investor_name,
          investor_name_in = investor_name,
          peer_group = peer_group,
          language = default_language,
          project_code = project_code
        )
      )
    )

  user_id_output_dir <- file.path(users_output_dir, paste0(project_prefix, "_user_", user_id))
  dir.create(file.path(user_id_output_dir, "00_Log_Files"), recursive = TRUE)
  dir.create(file.path(user_id_output_dir, "10_Parameter_File"), recursive = TRUE)
  dir.create(file.path(user_id_output_dir, "20_Raw_Inputs"), recursive = TRUE)
  dir.create(file.path(user_id_output_dir, "30_Processed_Inputs"), recursive = TRUE)
  dir.create(file.path(user_id_output_dir, "40_Results"), recursive = TRUE)
  dir.create(file.path(user_id_output_dir, "50_Outputs"), recursive = TRUE)

  write_yaml(config_list, file = file.path(user_id_output_dir, "10_Parameter_File", paste0(project_prefix, "_user_", user_id, "_PortfolioParameters.yml")))

  user_data %>%
    select(investor_name, portfolio_name, isin, market_value, currency) %>%
    write_csv(file.path(user_id_output_dir, "20_Raw_Inputs", paste0(project_prefix, "_user_", user_id, ".csv")))
}


# slices for per organization_type --------------------------------------------------

orgs_output_dir <- file.path(output_dir, "organization_type")
dir.create(orgs_output_dir, showWarnings = FALSE)

all_org_types <- unique(data$organization_type)

for (org_type in all_org_types) {
  org_data <-
    data %>%
    dplyr::filter(organization_type == .env$org_type) %>%
    mutate(investor_name = .env$org_type) %>%
    mutate(portfolio_name = .env$org_type)

  config_list <-
    list(
      default = list(
        parameters = list(
          portfolio_name_in = org_type,
          investor_name_in = org_type,
          peer_group = org_type,
          language = default_language,
          project_code = project_code
        )
      )
    )

  org_type_output_dir <- file.path(orgs_output_dir, paste0(project_prefix, "_org_", org_type))
  dir.create(file.path(org_type_output_dir, "00_Log_Files"), recursive = TRUE)
  dir.create(file.path(org_type_output_dir, "10_Parameter_File"), recursive = TRUE)
  dir.create(file.path(org_type_output_dir, "20_Raw_Inputs"), recursive = TRUE)
  dir.create(file.path(org_type_output_dir, "30_Processed_Inputs"), recursive = TRUE)
  dir.create(file.path(org_type_output_dir, "40_Results"), recursive = TRUE)
  dir.create(file.path(org_type_output_dir, "50_Outputs"), recursive = TRUE)

  write_yaml(config_list, file = file.path(org_type_output_dir, "10_Parameter_File", paste0(project_prefix, "_org_", org_type, "_PortfolioParameters.yml")))

  org_data %>%
    select(investor_name, portfolio_name, isin, market_value, currency) %>%
    write_csv(file.path(org_type_output_dir, "20_Raw_Inputs", paste0(project_prefix, "_org_", org_type, ".csv")))
}


# run meta portfolio through PACTA --------------------------------------------

message("running meta portfolio through PACTA")

dir_delete("working_dir")
dir_copy(meta_output_dir, "working_dir", overwrite = TRUE)

portfolio_name_ref_all <- paste0(project_prefix, "_meta")

source("web_tool_script_1.R", local = TRUE)
source("web_tool_script_2.R", local = TRUE)

dir_delete(meta_output_dir)
dir_copy("working_dir", meta_output_dir, overwrite = TRUE)


# run portfolio files through PACTA --------------------------------------------

for (port_id in all_port_ids) {
  message(paste0("running port_id: ", port_id, " through PACTA"))

  port_id_output_dir <- file.path(ports_output_dir, paste0(project_prefix, "_port_", port_id))

  dir_delete("working_dir")
  dir_copy(port_id_output_dir, "working_dir", overwrite = TRUE)

  portfolio_name_ref_all <- paste0(project_prefix, "_port_", port_id)

  source("web_tool_script_1.R", local = TRUE)
  source("web_tool_script_2.R", local = TRUE)

  dir_delete(port_id_output_dir)
  dir_copy("working_dir", port_id_output_dir, overwrite = TRUE)
}


# run user files through PACTA --------------------------------------------

for (user_id in all_user_ids) {
  message(paste0("running user_id: ", user_id, " through PACTA"))

  user_id_output_dir <- file.path(users_output_dir, paste0(project_prefix, "_user_", user_id))

  dir_delete("working_dir")
  dir_copy(user_id_output_dir, "working_dir", overwrite = TRUE)

  portfolio_name_ref_all <- paste0(project_prefix, "_user_", user_id)

  source("web_tool_script_1.R", local = TRUE)
  source("web_tool_script_2.R", local = TRUE)

  dir_delete(user_id_output_dir)
  dir_copy("working_dir", user_id_output_dir, overwrite = TRUE)
}


# run organization_type files through PACTA --------------------------------------------

for (org_type in all_org_types) {
  message(paste0("running org_type: ", org_type, " through PACTA"))

  org_type_output_dir <- file.path(orgs_output_dir, paste0(project_prefix, "_org_", org_type))

  dir_delete("working_dir")
  dir_copy(org_type_output_dir, "working_dir", overwrite = TRUE)

  portfolio_name_ref_all <- paste0(project_prefix, "_org_", org_type)

  source("web_tool_script_1.R", local = TRUE)
  source("web_tool_script_2.R", local = TRUE)

  dir_delete(org_type_output_dir)
  dir_copy("working_dir", org_type_output_dir, overwrite = TRUE)
}
