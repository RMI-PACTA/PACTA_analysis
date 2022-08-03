create_portfolio_subfolders <- function(portfolio_name_ref_all = NULL, project_location = NULL) {
  folders <- c("00_Log_Files", "30_Processed_Inputs", "40_Results", "50_Outputs")

  locs_to_create <- folders %>%
    purrr::map(~ file.path(project_location, .x, portfolio_name_ref_all)) %>%
    purrr::flatten_chr()

  locs_to_create %>%
    purrr::map(~ dir.create(.x, showWarnings = FALSE))

  invisible(portfolio_name_ref_all)
}
