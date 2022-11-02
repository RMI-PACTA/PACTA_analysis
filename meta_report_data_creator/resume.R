library("dplyr")
library("tibble")
library("purrr")
library("tidyr")
library("fs")

output_dir <- "/datadrive/PA2022CH_out2"  # this will likely not work on Windows, so change it!
project_prefix <- "PA2022CH"

pacta_directories <- c("00_Log_Files", "10_Parameter_File", "20_Raw_Inputs", "30_Processed_Inputs", "40_Results", "50_Outputs")

group_types <- c("meta", "organization_type", "user_id", "port_id")

all_paths <- tibble(type = setdiff(group_types, "meta")) %>% #so the meta 10/20/30 dirs don't get expanded
  mutate(path = purrr::map(type, ~ list.dirs(file.path(output_dir, .x), recursive = FALSE))) %>%
  tidyr::unnest(path) %>%
  bind_rows(tibble(type = "meta", path = file.path(output_dir, "meta"))) %>%
  mutate(portfolio_name_ref_all = case_when(
    type == "meta" ~ "meta",
    TRUE ~ basename(path)
  )) %>%
  arrange(type, portfolio_name_ref_all)
  
for ( i in seq(1, nrow(all_paths)) ){
  this_row <- all_paths[i, ]
  message(paste(Sys.time(), this_row$type, this_row$portfolio_name_ref_all, "--", i, "/", nrow(all_paths)))
  message(paste("  ", this_row$path))
  these_dirs <- file.path(this_row$path, pacta_directories)
  stopifnot(all(dir.exists(these_dirs)))
  has_results <- (length(list.files(file.path(this_row$path, "40_Results"))) > 0)
  if (has_results){
    message("  Results already exist, skipping")
  } else {
    message("  Running PACTA")

    dir_delete("working_dir")
    dir_copy(this_row$path, "working_dir", overwrite = TRUE)

    portfolio_name_ref_all <- this_row$portfolio_name_ref_all

    source("web_tool_script_1.R", local = TRUE)
    source("web_tool_script_2.R", local = TRUE)

    dir_delete(this_row$path)
    dir_copy("working_dir", this_row$path, overwrite = TRUE)
  }
}
