setup_project <- function() {
  path <- fs::path(
    "deduplicate",
    "set_portfolio-name-ref-all_working-location_and_web-parameters.R"
  )
  source(path)
}

use_r_packages <- function(path = r_packages_path()) {
  suppressPackageStartupMessages(source(path))
  resolve_conflicts()

  invisible(path)
}

resolve_conflicts <- function() {
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("lag", "dplyr")
  conflicted::conflict_prefer("mutate", "dplyr")
  conflicted::conflict_prefer("here", "here")
  conflicted::conflict_prefer("rename", "dplyr")
  conflicted::conflict_prefer("summarise", "dplyr")
  conflicted::conflict_prefer("arrange", "dplyr")
}
