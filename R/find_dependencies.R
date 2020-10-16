find_dependencies <- function(paths = pacta_projects()) {
  deps <- purrr::map_df(paths, ~renv::dependencies(.x, progress = FALSE))
  deps <- rlang::set_names(deps, tolower)

  tibble::as_tibble(deps)
}

pacta_projects <- function() {
  path_parent(c(
    "PACTA_analysis",
    "create_interactive_report",
    "StressTestingModelDev"
  ))
}

path_parent <- function(...) {
  fs::path(fs::path_dir(here::here()), ...)
}
