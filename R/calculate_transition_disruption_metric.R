#' Calculate the transition disruption metric, based on the IPR scenario
#'
#' @param data A dataset like the `*_results_portfolio.rda` outputs of PACTA.
#' @param start_year The start year for which the TDM value should be calculated.
#' @param ... Variables to group by.
#'
#' @return A tibble with the columns specified in the `...` input, as well as:
#'   `tdm_tech`, the technology level transition disruption metric and `tdm_sec`,
#'    the sector level transition disruption metric.
#' @export
#'
#' @examples
calculate_transition_disruption_metric <- function(data, start_year, ...) {

  stopifnot(
    is.data.frame(data),
    is.numeric(start_year)
  )

  crucial <- c(
    "allocation",
    "ald_sector",
    "technology",
    "year",
    "scen_alloc_wt_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten"
  )

  check_crucial_names(data, crucial)

  groups <- c(
    "technology",
    "ald_sector",
    ...
  )

  data <- data %>%
    # TODO: This function will only work if the allocation method is portfolio_weight
    # ownership_weight outputs 0 for all carsten metric values, and thus we would be
    # dividing by zero otherwise...
    dplyr::filter(.data$allocation == "portfolio_weight") %>%
    warn_if_has_zero_rows(
      'Filtering for "portfolio_weight" allocation, outputs 0 rows'
      )

  if (nrow(data) == 0) {
    return(empty_calculate_transition_disruption_metric())
  }

  technology_level_dy <- data %>%
    dplyr::filter(.data$year %in% c(start_year, start_year + 5, start_year + 10)) %>%
    dplyr::mutate(
      time_step = dplyr::case_when(
        .data$year == start_year ~ "start_year",
        .data$year == start_year + 5 ~ "plus_five",
        .data$year == start_year + 10 ~ "plus_ten"
      )
    ) %>%
    dplyr::group_by(!!!rlang::syms(groups)) %>%
    dplyr::select(
      !!!rlang::syms(groups),
      time_step,
      scen_alloc = "scen_alloc_wt_tech_prod",
      plan_alloc = "plan_alloc_wt_tech_prod"
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$time_step,
      values_from = c("scen_alloc", "plan_alloc")
      ) %>%
    dplyr::mutate(
      .numerator = .data$scen_alloc_plus_ten - .data$plan_alloc_plus_five,
      .denominator = .data$scen_alloc_plus_ten - .data$scen_alloc_start_year,
      tdm_tech = max(0, .numerator / .denominator) * 2
    ) %>%
    dplyr::select(tdm_tech, !!!rlang::syms(groups)) %>%
    dplyr::ungroup()

  data %>%
    dplyr::filter(.data$year == start_year) %>%
    dplyr::left_join(technology_level_dy) %>%
    dplyr::group_by(!!!rlang::syms(groups)) %>%
    dplyr::ungroup(technology) %>%
    dplyr::mutate(
      tdm_sec = .data$plan_carsten * sum(.data$tdm_tech) / sum(.data$plan_carsten),
      reference_year = start_year
      ) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!!rlang::syms(groups), tdm_tech, tdm_sec, reference_year)
}

check_crucial_names <- function(data, expected_names) {
  stopifnot(rlang::is_named(data))
  stopifnot(is.character(expected_names))

  ok <- all(unique(expected_names) %in% names(data))
  if (!ok) {
    abort_missing_names(sort(setdiff(expected_names, names(data))))
  }

  invisible(data)
}

abort_missing_names <- function(missing_names) {
  nms <- glue::glue_collapse(missing_names, sep = ", ", last = ", and ")
  abort(glue("Must have missing names:\n{nms}."), class = "missing_names")
}

warn_if_has_zero_rows <- function(data, message) {
  if (nrow(data) == 0L) warn(message = message, class = "has_zero_rows")
  invisible(data)
}

empty_calculate_transition_disruption_metric <- function() {
  tibble::tibble(
    technology = character(0),
    ald_sector = character(0),
    tdm_tech = numeric(0),
    tdm_sec = numeric(0),
    reference_year = integer(0),
  )
}
