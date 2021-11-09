
# manually set certain values and paths ----------------------------------------

output_dir <- normalizePath("~/Desktop/Peru", mustWork = FALSE)
combined_portfolio_results_output_dir <- file.path(output_dir, "combined", "portfolio_level")
combined_user_results_output_dir <- file.path(output_dir, "combined", "user_level")
combined_orgtype_results_output_dir <- file.path(output_dir, "combined", "orgtype_level")

data_path <- r2dii.utils::path_dropbox_2dii("2° Investing Team/1. research/1. Studies (projects)/PACTA . Regulator Monitoring/PACTA COP/03_Initiative_Level/06_PACTACOP_PE/04_Input_Cleaning/04. Data")
portfolios_path <- file.path(data_path, "portfolios")
portfolios_meta_csv <- file.path(data_path, "portfolios.csv")
users_meta_csv <- file.path(data_path, "users.csv")

project_code <- "PA2021PE"
default_language <- "ES"

project_prefix <- "peru"

local_PACTA_git_path <- "~/Documents/git/PACTA_analysis"

bogus_csvs_to_be_ignored <- c()


# check paths and directories --------------------------------------------------

dir.create(output_dir, showWarnings = FALSE)
dir.create(combined_portfolio_results_output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(combined_portfolio_results_output_dir, "30_Processed_inputs"), showWarnings = FALSE)
dir.create(file.path(combined_portfolio_results_output_dir, "40_Results"), showWarnings = FALSE)
dir.create(combined_user_results_output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(combined_user_results_output_dir, "30_Processed_inputs"), showWarnings = FALSE)
dir.create(file.path(combined_user_results_output_dir, "40_Results"), showWarnings = FALSE)
dir.create(combined_orgtype_results_output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(combined_orgtype_results_output_dir, "30_Processed_inputs"), showWarnings = FALSE)
dir.create(file.path(combined_orgtype_results_output_dir, "40_Results"), showWarnings = FALSE)

stopifnot(dir.exists(output_dir))
stopifnot(dir.exists(portfolios_path))
stopifnot(file.exists(portfolios_meta_csv))
stopifnot(file.exists(users_meta_csv))
stopifnot(dir.exists(local_PACTA_git_path))
stopifnot(dir.exists(combined_portfolio_results_output_dir))
stopifnot(dir.exists(file.path(combined_portfolio_results_output_dir, "30_Processed_inputs")))
stopifnot(dir.exists(file.path(combined_portfolio_results_output_dir, "40_Results")))
stopifnot(dir.exists(combined_user_results_output_dir))
stopifnot(dir.exists(file.path(combined_user_results_output_dir, "30_Processed_inputs")))
stopifnot(dir.exists(file.path(combined_user_results_output_dir, "40_Results")))
stopifnot(dir.exists(combined_orgtype_results_output_dir))
stopifnot(dir.exists(file.path(combined_orgtype_results_output_dir, "30_Processed_inputs")))
stopifnot(dir.exists(file.path(combined_orgtype_results_output_dir, "40_Results")))


# load required packages -------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(stringr)
library(fs)
library(readr)
library(yaml)

source(file.path(local_PACTA_git_path, "meta_report_data_creator/read_portfolio_csv.R"))

pacta_directories <- c("00_Log_Files", "10_Parameter_File", "20_Raw_Inputs", "30_Processed_Inputs", "40_Results", "50_Outputs")


# prepare a list of all the CSVs to import -------------------------------------

portfolio_csvs <- list.files(portfolios_path, pattern = "[.]csv$", full.names = TRUE)

# remove bogus CSVs
portfolio_csvs <- portfolio_csvs[! tools::file_path_sans_ext(basename(portfolio_csvs)) %in% bogus_csvs_to_be_ignored]


# read in all the CSVs ---------------------------------------------------------

data <- map_dfr(set_names(portfolio_csvs, portfolio_csvs), read_portfolio_csv, .id = "csv_name")


# read in meta data CSVs -------------------------------------------------------

portfolios_meta <- read_csv(portfolios_meta_csv, col_types = "nnnccnc")
users_meta <- read_csv(users_meta_csv, col_types = "ncccccn")


# add meta data to full data and save it ---------------------------------------

data <-
  data %>%
  mutate(port_id = suppressWarnings(as.numeric(tools::file_path_sans_ext(basename(csv_name))))) %>%
  left_join(portfolios_meta[, c("id", "user_id")], by = c(port_id = "id")) %>%
  left_join(users_meta[, c("id", "organization_type")], by = c(user_id = "id"))

data <-
  data %>%
  filter(!is.na(port_id)) %>%
  filter(!is.na(user_id))

write_csv(data, file = file.path(output_dir, paste0(project_prefix, "_full.csv")))
saveRDS(data, file.path(output_dir, paste0(project_prefix, "_full.rds")))


# prepare meta PACTA project ---------------------------------------------------

meta_output_dir <- file.path(output_dir, "meta")
dir.create(meta_output_dir, showWarnings = FALSE)

dir_create(file.path(meta_output_dir, pacta_directories))

data %>%
  mutate(portfolio_name = "Meta Portfolio") %>%
  mutate(investor_name = "Meta Investor") %>%
  select(investor_name, portfolio_name, isin, market_value, currency) %>%
  write_csv(file = file.path(meta_output_dir, "20_Raw_Inputs", paste0(project_prefix, "_meta.csv")))

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


# slices for per port_id -------------------------------------------------------

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
  dir_create(file.path(port_id_output_dir, pacta_directories))

  write_yaml(config_list, file = file.path(port_id_output_dir, "10_Parameter_File", paste0(project_prefix, "_port_", port_id, "_PortfolioParameters.yml")))

  port_data %>%
    select(investor_name, portfolio_name, isin, market_value, currency) %>%
    write_csv(file.path(port_id_output_dir, "20_Raw_Inputs", paste0(project_prefix, "_port_", port_id, ".csv")))
}


# slices for per user_id -------------------------------------------------------

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
  dir_create(file.path(user_id_output_dir, pacta_directories))

  write_yaml(config_list, file = file.path(user_id_output_dir, "10_Parameter_File", paste0(project_prefix, "_user_", user_id, "_PortfolioParameters.yml")))

  user_data %>%
    select(investor_name, portfolio_name, isin, market_value, currency) %>%
    write_csv(file.path(user_id_output_dir, "20_Raw_Inputs", paste0(project_prefix, "_user_", user_id, ".csv")))
}


# slices for per organization_type ---------------------------------------------

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
  dir_create(file.path(org_type_output_dir, pacta_directories))

  write_yaml(config_list, file = file.path(org_type_output_dir, "10_Parameter_File", paste0(project_prefix, "_org_", org_type, "_PortfolioParameters.yml")))

  org_data %>%
    select(investor_name, portfolio_name, isin, market_value, currency) %>%
    write_csv(file.path(org_type_output_dir, "20_Raw_Inputs", paste0(project_prefix, "_org_", org_type, ".csv")))
}


# run meta portfolio through PACTA ---------------------------------------------

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


# run user files through PACTA -------------------------------------------------

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


# run organization_type files through PACTA ------------------------------------

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


# combine all portfolio level results ------------------------------------------


mutate(portfolio_name = portfolio_id) %>%
mutate(portfolio_id = as.numeric(portfolio_id)) %>%
left_join(select(portfolios_meta, portfolio_id = id, user_id)) %>%
mutate(investor_name = user_id) %>%
select(-c(portfolio_id, user_id))




data_filenames <-
  c(
    "Bonds_results_portfolio.rda",
    "Bonds_results_company.rda",
    "Bonds_results_map.rda",
    "Equity_results_portfolio.rda",
    "Equity_results_company.rda",
    "Equity_results_map.rda"
  )

lapply(data_filenames, function(data_filename) {
  portfolio_result_filepaths <- list.files(ports_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  meta_result_filepaths <- list.files(meta_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  all_result_filepaths <- c(portfolio_result_filepaths, meta_result_filepaths)
  all_result_filepaths <- setNames(all_result_filepaths, sub("^norway_port_", "", basename(dirname(all_result_filepaths))))

  all_results <-
    map_df(all_result_filepaths, readRDS, .id = "portfolio_id") %>%
    mutate(portfolio_name = portfolio_id) %>%
    mutate(portfolio_id = as.numeric(portfolio_id)) %>%
    left_join(select(portfolios_meta, portfolio_id = id, user_id)) %>%
    mutate(investor_name = user_id) %>%
    select(-portfolio_id, -user_id)

  saveRDS(all_results, file.path(combined_portfolio_results_output_dir, "40_Results", data_filename))
})

data_filenames <-
  c(
    "Overview_portfolio.rda",
    "total_portfolio.rda",
    "emissions.rda"
  )

lapply(data_filenames, function(data_filename) {
  portfolio_result_filepaths <- list.files(ports_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  meta_result_filepaths <- list.files(meta_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  all_result_filepaths <- c(portfolio_result_filepaths, meta_result_filepaths)

  all_results <- map_df(all_result_filepaths, readRDS)
  saveRDS(all_results, file.path(combined_portfolio_results_output_dir, "30_Processed_inputs", data_filename))
})


# combine all user level results -----------------------------------------------

data_filenames <-
  c(
    "Bonds_results_portfolio.rda",
    "Bonds_results_company.rda",
    "Bonds_results_map.rda",
    "Equity_results_portfolio.rda",
    "Equity_results_company.rda",
    "Equity_results_map.rda"
  )

lapply(data_filenames, function(data_filename) {
  portfolio_result_filepaths <- list.files(users_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  meta_result_filepaths <- list.files(meta_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  all_result_filepaths <- c(portfolio_result_filepaths, meta_result_filepaths)
  all_result_filepaths <- setNames(all_result_filepaths, sub("^norway_user_", "", basename(dirname(all_result_filepaths))))

  all_results <-
    map_df(all_result_filepaths, readRDS, .id = "user_id") %>%
    mutate(portfolio_name = user_id) %>%
    mutate(user_id = as.numeric(user_id)) %>%
    left_join(select(users_meta, user_id = id, organization_type)) %>%
    rename(peergroup = organization_type) %>%
    mutate(investor_name = peergroup) %>%
    select(-peergroup, -user_id)

  saveRDS(all_results, file.path(combined_user_results_output_dir, "40_Results", data_filename))
})

data_filenames <-
  c(
    "Overview_portfolio.rda",
    "total_portfolio.rda",
    "emissions.rda"
  )

lapply(data_filenames, function(data_filename) {
  portfolio_result_filepaths <- list.files(users_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  meta_result_filepaths <- list.files(meta_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  all_result_filepaths <- c(portfolio_result_filepaths, meta_result_filepaths)
  all_result_filepaths <- setNames(all_result_filepaths, sub("^norway_user_", "", basename(dirname(all_result_filepaths))))

  all_results <-
    map_df(all_result_filepaths, readRDS, .id = "user_id") %>%
    mutate(portfolio_name = user_id) %>%
    mutate(user_id = as.numeric(user_id)) %>%
    left_join(users_meta[, c("id", "organization_type")], by = c(user_id = "id")) %>%
    rename(peergroup = organization_type) %>%
    mutate(investor_name = peergroup) %>%
    select(-c(peergroup, user_id))

  saveRDS(all_results, file.path(combined_user_results_output_dir, "30_Processed_inputs", data_filename))
})


# combine all organization_type level results ----------------------------------

data_filenames <-
  c(
    "Bonds_results_portfolio.rda",
    "Bonds_results_company.rda",
    "Bonds_results_map.rda",
    "Equity_results_portfolio.rda",
    "Equity_results_company.rda",
    "Equity_results_map.rda"
  )

lapply(data_filenames, function(data_filename) {
  portfolio_result_filepaths <- list.files(org_type_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  meta_result_filepaths <- list.files(meta_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  all_result_filepaths <- c(portfolio_result_filepaths, meta_result_filepaths)

  all_results <- map_df(all_result_filepaths, readRDS)
  saveRDS(all_results, file.path(combined_orgtype_results_output_dir, "40_Results", data_filename))
})

data_filenames <-
  c(
    "Overview_portfolio.rda",
    "total_portfolio.rda",
    "emissions.rda"
  )

lapply(data_filenames, function(data_filename) {
  portfolio_result_filepaths <- list.files(org_type_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  meta_result_filepaths <- list.files(meta_output_dir, pattern = data_filename, recursive = TRUE, full.names = TRUE)
  all_result_filepaths <- c(portfolio_result_filepaths, meta_result_filepaths)

  all_results <- map_df(all_result_filepaths, readRDS)
  saveRDS(all_results, file.path(combined_orgtype_results_output_dir, "30_Processed_inputs", data_filename))
})
