#' Calculate and add the transition disruption metric to a dataset
#'
#' This function takes a typical PACTA output, and uses it to calculate the
#' transition disruption metric.
#'
#' @param data A data.frame (or tibble), with a format similar to the
#'   `*_results_portfolio.rda` outputs of PACTA.
#' @param t0 The start year, from which the TDM value should be calculated. This
#'   value should coincide with the earliest year in the PACTA results dataset.
#' @param t1 The number of years into the future for which there is production
#'   data. The default value is 5 years since this is often the limit of the
#'   forward-looking data.
#' @param t2 The number of years into the future against which you want to
#'   calculate disruption.
#' @param additional_groups Character vector. The names of columns to group by,
#'   in addition to these ones (which are always used):
#'
#'   `r toString(crucial_tdm_groups())`
#'
#' @return A tibble with the columns specified in the `additional_groups` input
#'   as well as `tdm_tech`: the technology level transition disruption metric
#'   and `tdm_sec`: the sector level transition disruption metric.
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
  stopifnot(
    is.data.frame(data),
    is.numeric(t0),
    is.numeric(t1),
    is.numeric(t2)
  )

  if (!is.null(additional_groups)) stopifnot(is.character(additional_groups))

  groups <- union(crucial_tdm_groups(), additional_groups)

  check_calculate_tdm(data, t0, t1, t2, groups)

  filtered_data <- filter(data, .data$allocation == "portfolio_weight")

  if (nrow(filtered_data) == 0) {
    return(warn_zero_rows(tdm_prototype()))
  }

  # TODO: Filter the data fora  particular scenario. Function should gain the
  # argument `scenario`, which by default specifies IPR - FPS scenario. I am
  # waiting until the data is prepared as I'm not sure exactly what this
  # scenario will be called in the input data.

  data_with_monotonic_factors <- add_monotonic_factor(filtered_data, t0, t1, t2, groups)

  initial_year_data <- filter(data_with_monotonic_factors, .data$year == t0)

  preformatted_data <- pre_format_data(data_with_monotonic_factors, t0, t1, t2, groups)

  data_with_tdm <- add_tdm(preformatted_data, groups)

  formatted_data_with_tdm <- left_join(
    initial_year_data,
    data_with_tdm,
    by = groups
  )

  formatted_data_with_tdm %>%
    add_aggregate_tdm(t0, groups) %>%
    select(names(tdm_prototype()), all_of(groups))
}

warn_zero_rows <- function(data) {
  # NOTE: This function will only work if the allocation method is
  # portfolio_weight. ownership_weight outputs 0 for all carsten metric values,
  # and thus we would be dividing by zero otherwise...
  message <- 'Filtering for "portfolio_weight" allocation, outputs 0 rows'
  warn(message, class = "has_zero_rows")

  invisible(data)
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

add_monotonic_factor <- function(data, t0, t1, t2, groups) {
  data <- data %>%
    group_by(!!!rlang::syms(groups)) %>%
    mutate(
      end_year = last(.data$year),
      end_year_is_t0_t2 = .data$end_year == t0 + t2
    )

  if (all(data$end_year_is_t0_t2)) {
    monotonic_factors <- data %>%
      mutate(
        .is_monotonic = TRUE,
        monotonic_factor = dplyr::if_else(.data$.is_monotonic, 1, -1),
        .is_monotonic = NULL
      ) %>%
      select(
        -c(
          .data$year,
          .data$end_year,
          .data$plan_carsten,
          .data$plan_alloc_wt_tech_prod,
          .data$scen_alloc_wt_tech_prod,
          .data$end_year_is_t0_t2
        )
      ) %>%
      distinct() %>%
      ungroup()
  } else {
    monotonic_factors <- data %>%
      add_time_step(t0, t1, t2) %>%
      mutate(time_step = case_when(
        .data$year == .data$end_year ~ "end_year",
        TRUE ~ .data$time_step
      )) %>%
      select(
        -c(
          .data$year,
          .data$end_year,
          .data$plan_carsten,
          .data$plan_alloc_wt_tech_prod,
          .data$end_year_is_t0_t2
        )
      ) %>%
      tidyr::pivot_wider(
        names_from = .data$time_step,
        values_from = .data$scen_alloc_wt_tech_prod
      ) %>%
      mutate(
        .increasing_overall = .data$end_year > .data$t0,
        .increasing_in_interval = .data$plus_t2 > .data$t0,
        .is_monotonic = .data$.increasing_in_interval & .data$.increasing_overall,
        monotonic_factor = dplyr::if_else(.data$.is_monotonic, 1, -1),
        .increasing_overall = NULL,
        .increasing_in_interval = NULL,
        .is_monotonic = NULL,
        t0 = NULL,
        plus_t1 = NULL,
        plus_t2 = NULL,
        end_year = NULL
      ) %>%
      ungroup()
  }

  left_join(data, monotonic_factors, by = groups)
}

add_tdm <- function(data, groups) {
  data %>%
    group_by(!!!rlang::syms(groups)) %>%
    mutate(
      .numerator = .data$scenario_production_plus_t2 - .data$portfolio_production_plus_t1,
      .denominator = .data$scenario_production_plus_t2 - .data$scenario_production_t0,
      # TODO: This case we
      tdm_tech = ifelse(
        .data$.denominator == 0,
        0,
        max(0, (.data$.numerator / .data$.denominator) * .data$monotonic_factor) * 2
      ),
      .numerator = NULL,
      .denominator = NULL
    ) %>%
    select(.data$tdm_tech, all_of(groups)) %>%
    distinct() %>%
    ungroup()
}

add_aggregate_tdm <- function(data, t0, groups) {
  data %>%
    group_by(!!!rlang::syms(groups)) %>%
    ungroup(.data$technology) %>%
    mutate(
      tdm_sec = .data$plan_carsten * sum(.data$tdm_tech) / sum(.data$plan_carsten),
      reference_year = t0
    ) %>%
    ungroup()
}

pre_format_data <- function(data, t0, t1, t2, groups) {
  data %>%
    filter(.data$year %in% c(t0, t0 + t1, t0 + t2)) %>%
    group_by(!!!rlang::syms(groups)) %>%
    add_time_step(t0, t1, t2) %>%
    select(
      !!!rlang::syms(groups),
      .data$time_step,
      .data$monotonic_factor,
      scenario_production = .data$scen_alloc_wt_tech_prod,
      portfolio_production = .data$plan_alloc_wt_tech_prod,
    ) %>%
    pivot_wider(
      names_from = .data$time_step,
      values_from = all_of(c("scenario_production", "portfolio_production"))
    ) %>%
    ungroup()
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

check_calculate_tdm <- function(data, t0, t1, t2, groups) {
  check_crucial_names(data, crucial_columns())

  check_crucial_years(data, t0, t1, t2, groups)

  check_unique_by_year_and_groups(data, groups)

  invisible(data)
}

check_unique_by_year_and_groups <- function(data, groups) {
  req_unique_columns <- c(
    "scen_alloc_wt_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten"
  )

  data <- data %>%
    group_by(!!!rlang::syms(c("year", groups))) %>%
    dplyr::summarize(rows_by_year_and_group = dplyr::n()) %>%
    mutate(rows_are_unique = .data$rows_by_year_and_group == 1)

  ok <- all(data$rows_are_unique)
  if (!ok) {
    rlang::abort(
      message = c(
        "Data must be unique by year and groups for the following columns",
        x = glue::glue_collapse(
          req_unique_columns,
          sep = ",",
          width = 50,
          last = "and"
          ),
        i = "Are you sure you have included the correct groupings?"
        ),
      class = "multiple_values_per_year",
    )
  }

  invisible(data)
}

check_crucial_years <- function(data, t0, t1, t2, groups) {
  crucial_years <- tibble::tibble(crucial_years = c(t0, t0 + t1, t0 + t2))

  missing_crucial_years <- crucial_years %>%
    left_join(data, by = c(crucial_years = "year")) %>%
    group_by(!!!rlang::syms(groups)) %>%
    dplyr::summarize(missing_data = sum(is.na(.data$plan_alloc_wt_tech_prod)))

  ok <- all(missing_crucial_years$missing_data == 0)

  if (!ok) {
    rlang::abort(
      message = c(
        "Data must contain all crucial years, by group:",
        x = glue::glue_collapse(
          crucial_years,
          sep = ",",
          width = 50,
          last = "and"
        ),
        i = "Are all crucial years present in input data?"
      ),
      class = "missing_crucial_years",
    )
  }
}

crucial_tdm_groups <- function() {
  c("technology", "ald_sector")
}

crucial_columns <- function() {
  c(
    crucial_tdm_groups(),
    "allocation",
    "year",
    "scen_alloc_wt_tech_prod",
    "plan_alloc_wt_tech_prod",
    "plan_carsten"
  )
}
