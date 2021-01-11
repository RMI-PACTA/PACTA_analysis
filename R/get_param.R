# Copied and adapted from r2dii.utils, to avoid depending on it

get_config <- function() {
  getOption("r2dii_config")
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
  param <- dplyr::last(unlist(x))

  if (is.null(out) && identical(if_null, stop)) {
    abort(glue(
      "{param} must be not `NULL`.
      Please set {param} on the configuration (.yml) file."
    ))
  }

  if (is.null(out) && !is.null(if_null)) {
    warn(glue("On config.yml, {param} is undefined."))

    inform(glue("Setting {param} to default value: {if_null}."))
    out <- if_null
  }

  out
}
