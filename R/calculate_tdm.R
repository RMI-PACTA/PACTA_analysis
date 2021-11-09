#' Calculate the transition disruption metric, based on the IPR scenario
#'
#' @param data A dataset like the `*_results_portfolio.rda` outputs of PACTA.
#' @param t0 The start year for which the TDM value should be calculated.
#' @param t1 The number of years into the future for which there is production
#'   data. Default is 5 years.
#' @param t2 The number of years into the future against which you want to
#'   calculate disruption. Default is 10 years.
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
#' calculate_tdm(pacta_results, t0 = 2020)
calculate_tdm <- function(data, t0, t1 = 5, t2 = 10, additional_groups = NULL) {

  groups <- unique(c(crucial_tdm_groups(), additional_groups))
  check_calculate_tdm(data, t0, t1, t2, groups)

  portfolio_weight <- filter(data, .data$allocation == "portfolio_weight")
  if (nrow(portfolio_weight) == 0) {
    return(warn_zero_rows(tdm_prototype()))
  }

  joint <- left_join(
    filter(portfolio_weight, .data$year == t0),
    technology_level_dy(portfolio_weight, t0, t1, t2, groups),
    by = groups
  )

  # TODO: Try to extract one or more meaningful functions
  joint %>%
    group_by(!!!rlang::syms(groups)) %>%
    ungroup(.data$technology) %>%
    add_sector_level_tdm(t0) %>%
    ungroup() %>%
    select(names(tdm_prototype()), all_of(groups))
}

check_calculate_tdm <- function(data, t0, t1, t2, additional_groups) {
  stopifnot(is.data.frame(data), is.numeric(t0), is.numeric(t1), is.numeric(t2))

  crucial <- c(
    "allocation",
    crucial_tdm_groups(),
    "year",
    "scen_alloc_wt_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten"
  )
  check_crucial_names(data, crucial)

  check_crucial_years(data, t0, t1, t2, additional_groups)

  check_data_is_unique_per_year_and_groups(data, additional_groups)

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
technology_level_dy <- function(data, t0, t1, t2, groups) {
  long <- data %>%
    filter(.data$year %in% c(t0, t0 + t1, t0 + t2)) %>%
    add_time_step(t0, t1, t2) %>%
    group_by(!!!rlang::syms(groups)) %>%
    select(
      !!!rlang::syms(groups),
      .data$time_step,
      scen_alloc = "scen_alloc_wt_tech_prod",
      plan_alloc = "plan_alloc_wt_tech_prod"
    )

  long %>%
    pivot_wider(
      names_from = .data$time_step,
      values_from = all_of(c("scen_alloc", "plan_alloc"))
      ) %>%
    add_technology_level_tdm() %>%
    select(.data$tdm_tech, all_of(groups)) %>%
    distinct() %>%
    ungroup()
}

add_sector_level_tdm <- function(data, t0) {
  data %>%
    mutate(
      tdm_sec = .data$plan_carsten * sum(.data$tdm_tech) / sum(.data$plan_carsten),
      reference_year = t0
    )
}

add_time_step <- function(data, t0, t1, t2) {
  data %>%
    mutate(
      time_step = case_when(
        .data$year == t0 ~ "t0",
        .data$year == t0 + t1 ~ "plus_t1",
        .data$year == t0 + t2 ~ "plus_t2"
      )
    )
}

add_technology_level_tdm <- function(data) {
  data %>%
    mutate(
      .numerator = .data$scen_alloc_plus_t2 - .data$plan_alloc_plus_t1,
      .denominator = .data$scen_alloc_plus_t2 - .data$scen_alloc_t0,
      #TODO: Ensure with @antoine-lacherche that this is the right way to
      #address the issue of 0 denominator
      tdm_tech = ifelse(
        .data$.denominator == 0,
        0,
        max(0, .data$.numerator / .data$.denominator) * 2
        )
    )
}

check_data_is_unique_per_year_and_groups <- function(data, groups) {

  columns_that_must_be_unique <- c(
    "scen_alloc_wt_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten"
    )

  groups <- c("year", groups)

  data <- data %>%
    group_by(!!!rlang::syms(groups)) %>%
    dplyr::summarize(rows_by_year_and_group = dplyr::n()) %>%
    mutate(rows_are_unique = rows_by_year_and_group == 1)

  ok <- all(data$rows_are_unique)
  if (!ok) {
    rlang::abort(
      "multiple_values_per_year",
      message = glue(
        "Data must be unique by year and groups for the following columns:
      {paste0('`', columns_that_must_be_unique, '`', collapse = ', ')} \n
      Are you sure you have included the correct groupings?"
      )
    )
  }

  invisible(data)
}

check_crucial_years <- function(data, t0, t1, t2, groups) {

  crucial_years <- tibble::tibble(crucial_years = c(t0, t0 + t1, t0 + t2))

  missing_crucial_years <- crucial_years %>%
    left_join( data, by = c(crucial_years = "year")) %>%
    group_by(!!!rlang::syms(groups)) %>%
    dplyr::summarize(missing_data = sum(is.na(plan_alloc_wt_tech_prod)))

  ok <- all(missing_crucial_years$missing_data == 0)

  if (!ok) {
    rlang::abort(
      "missing_crucial_years",
      message = glue(
        "Data must contain all crucial years, by group:
      {paste0('`', crucial_years, '`', collapse = ', ')} \n
      Are all crucial years present in input data?"
      )
    )
  }
}
