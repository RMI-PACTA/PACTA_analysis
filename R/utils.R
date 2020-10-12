test_integration <- function(input = "integration-test.Rmd") {
  needless <- tempfile(fileext = ".html")
  rmarkdown::render(input, output_file = needless)

  invisible(input)
}

setup_project <- function() {
  path <- fs::path(
    "deduplicate",
    "set_portfolio-name-ref-all_working-location_and_web-parameters.R"
  )
  source(path)
}

use_r_packages <- function(path = "deduplicate/load-and-attach-r-packages.R") {
  source(path)
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


get_param <- function(..., if_null = NULL) {
  dots <- rlang::list2(...)
  function(file = get_config()) {
    pluck_param(dots, file = file, if_null = if_null)
  }
}
# Must be defined after get_param()
START.YEAR <- get_param(
  "AnalysisPeriod", "Years.Startyear",
  if_null = NULL
)

pluck_param <- function(x, file, if_null) {
  out <- purrr::pluck(config::get(file = file), !!!x)
  param <- ui_field(dplyr::last(unlist(x)))

  if (is.null(out) && identical(if_null, stop)) {
    abort(glue(
      "{param} must be not `NULL`.
      Please set {param} on the configuration (.yml) file."
    ))
  }

  if (is.null(out) && !is.null(if_null)) {
    param_ <- ui_field(param)
    warn(glue("On config.yml, {param} is undefined."))

    if_null_ <- ui_field(if_null)
    inform(glue("Setting {param_} to default value: {if_null_}."))
    out <- if_null
  }

  out
}

#' @importFrom rlang abort warn inform
#' @importFrom usethis ui_field
NULL
pluck_param <- function(x, file, if_null) {
  out <- purrr::pluck(config::get(file = file), !!!x)
  param <- ui_field(dplyr::last(unlist(x)))

  if (is.null(out) && identical(if_null, stop)) {
    abort(glue(
      "{param} must be not `NULL`.
      Please set {param} on the configuration (.yml) file."
    ))
  }

  if (is.null(out) && !is.null(if_null)) {
    param_ <- ui_field(param)
    warn(glue("On config.yml, {param} is undefined."))

    if_null_ <- ui_field(if_null)
    inform(glue("Setting {param_} to default value: {if_null_}."))
    out <- if_null
  }

  out
}
