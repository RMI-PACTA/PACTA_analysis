#' Calculate the transition disruption metric, based on the IPR scenario
#'
#' @param data A dataset like the `*_results_portfolio.rda` outputs of PACTA.
#' @param start_year The start year for which the TDM value should be calculated.
#' @param additional_groups Character vector. The names of columns to group by,
#'   in addition to these ones (which are always used):
#'
#'    `r toString(crucial_tdm_groups())`
#'
#' @return A tibble with the columns specified in the `...` input, as well as:
#'   `tdm_tech`, the technology level transition disruption metric and `tdm_sec`,
#'    the sector level transition disruption metric.
#' @export
#'
#' @examples
#' library(tibble)
#'
#' pacta_results <- tibble(
#'   investor_name = rep("some_investor", 3),
#'   portfolio_name = rep("some_portfolio", 3),
#'   allocation = rep("portfolio_weight", 3),
#'   equity_market = rep("GlobalMarket", 3),
#'   scenario_geography = rep("Global", 3),
#'   ald_sector = rep("Power", 3),
#'   technology = rep("RenewablesCap", 3),
#'   year = c(2020, 2025, 2030),
#'   plan_alloc_wt_tech_prod = rep(1, 3),
#'   scen_alloc_wt_tech_prod = 1:3,
#'   plan_carsten = rep(0.5, 3)
#' )
#' pacta_results
#'
#' calculate_tdm(pacta_results, start_year = 2020)
calculate_tdm <- function(data, start_year, additional_groups = NULL) {
  check_calculate_tdm(data, start_year)

  portfolio_weight <- filter(data, .data$allocation == "portfolio_weight")
  if (nrow(portfolio_weight) == 0) {
    return(warn_zero_rows(tdm_prototype()))
  }

  groups <- unique(c(crucial_tdm_groups(), additional_groups))
  tech_dy <- technology_level_dy(portfolio_weight, start_year, groups)

  # TODO: Try to extract one or more meaningful functions
  portfolio_weight %>%
    filter(.data$year == start_year) %>%
    left_join(tech_dy, by = groups) %>%
    group_by(!!!rlang::syms(groups)) %>%
    ungroup(.data$technology) %>%
    add_tdm_sec(start_year) %>%
    ungroup() %>%
    select(names(tdm_prototype()), all_of(groups))
}

check_calculate_tdm <- function(data, start_year) {
  stopifnot(is.data.frame(data), is.numeric(start_year))

  crucial <- c(
    "allocation",
    crucial_tdm_groups(),
    "year",
    "scen_alloc_wt_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten"
  )
  check_crucial_names(data, crucial)

  invisible(data)
}
warn_zero_rows <- function(data) {
  # TODO: This function will only work if the allocation method is
  # portfolio_weight ownership_weight outputs 0 for all carsten metric values,
  # and thus we would be dividing by zero otherwise...
  message <- 'Filtering for "portfolio_weight" allocation, outputs 0 rows'
  warn(message, class = "has_zero_rows")

  # Pass `data` to inline inside return()
  invisible(data)
}

tdm_prototype <- function() {
  tibble(
    technology = character(0),
    ald_sector = character(0),
    tdm_tech = numeric(0),
    tdm_sec = numeric(0),
    reference_year = integer(0),
  )
}

crucial_tdm_groups <- function() {
  c("technology", "ald_sector")
}

#' Technology-level "dy"
#' TODO: Explain the meaning of "dy".
#' @noRd
technology_level_dy <- function(data, start_year, groups) {
  data %>%
    filter(.data$year %in% c(start_year, start_year + 5, start_year + 10)) %>%
    add_time_step(start_year) %>%
    group_by(!!!rlang::syms(groups)) %>%
    select(
      !!!rlang::syms(groups),
      .data$time_step,
      scen_alloc = "scen_alloc_wt_tech_prod",
      plan_alloc = "plan_alloc_wt_tech_prod"
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$time_step,
      values_from = c("scen_alloc", "plan_alloc")
    ) %>%
    add_tdm_tech() %>%
    select(.data$tdm_tech, all_of(groups)) %>%
    ungroup()
}

add_tdm_sec <- function(data, start_year) {
  data %>%
    mutate(
      tdm_sec = .data$plan_carsten * sum(.data$tdm_tech) / sum(.data$plan_carsten),
      reference_year = start_year
    )
}

add_time_step <- function(data, start_year) {
  data %>%
    mutate(
      time_step = case_when(
        .data$year == start_year ~ "start_year",
        .data$year == start_year + 5 ~ "plus_five",
        .data$year == start_year + 10 ~ "plus_ten"
      )
    )
}

add_tdm_tech <- function(data) {
  data %>%
    mutate(
      .numerator = .data$scen_alloc_plus_ten - .data$plan_alloc_plus_five,
      .denominator = .data$scen_alloc_plus_ten - .data$scen_alloc_start_year,
      tdm_tech = max(0, .data$.numerator / .data$.denominator) * 2
    )
}
