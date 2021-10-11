test_that("with bad `data` errors with informative message", {
  expect_error(calculate_tdm("bad", 2020), "data.frame.*not.*TRUE")
  expect_error(calculate_tdm(fake_tdm_data(), "bad"), "numeric.*not.*TRUE")
})

test_that("outputs the expected tibble", {
  data <- fake_tdm_data(
    year = c(2020, 2025, 2030), scen_alloc_wt_tech_prod = 1:3
  )
  out <- calculate_tdm(data, 2020)
  expect_snapshot(out)
})

test_that("FIXME: outputs `NaN` in columns `tdm_tech` and `tdm_sec`", {
  data <- fake_tdm_data(year = c(2020, 2025, 2030))
  out <- calculate_tdm(data, 2020)
  expect_true(is.nan(out$tdm_tech))
  expect_true(is.nan(out$tdm_sec))
})

test_that("outputs is ungrouped", {
  data <- fake_tdm_data(year = c(2020, 2025, 2030))
  out <- calculate_tdm(data, 2020)
  expect_false(is_grouped_df(out))
})

test_that("joins quietly", {
  data <- fake_tdm_data(year = c(2020, 2025, 2030))
  expect_no_message(calculate_tdm(data, 2020))
})

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name) {
    pacta_results <- fake_tdm_data(year = c(2020, 2025, 2030))
    bad <- rename(pacta_results, bad = all_of(name))
    expect_error(calculate_tdm(bad, 2020), class = "missing_names")
  }

  expect_error_missing_names("allocation")
  expect_error_missing_names("ald_sector")
  expect_error_missing_names("technology")
  expect_error_missing_names("year")
  expect_error_missing_names("plan_carsten")
  expect_error_missing_names("scen_alloc_wt_tech_prod")
  expect_error_missing_names("plan_alloc_wt_tech_prod")
})

test_that("if only `allocation` is 'ownership_weight' outputs a 0-row tibble", {
  fake_tdm_data(allocation = "ownership_weight", year = c(2020, 2025, 2030)) %>%
    calculate_tdm(2020) %>%
    expect_s3_class("tbl") %>%
    expect_warning(class = "has_zero_rows")
})

test_that("additional groups extend the minimum output", {
  data <- fake_tdm_data(
    technology = rep(c("RenewablesCap", "OilCap"), each = 4),
    year = rep(c(2020, 2021, 2025, 2030), 2),
    plan_alloc_wt_tech_prod = c(1, 1, 1, 1, 1, 2, 3, 4),
    scen_alloc_wt_tech_prod = c(1, 2, 3, 4, 1, 0.75, 0.5, 0.25),
    plan_carsten = c(0.5, 0.3, 0.25, 0.2, 0.5, 0.7, 0.75, 0.8),
  )

  minimum <- calculate_tdm(data, 2020)
  additional_groups <- c("investor_name", "portfolio_name")
  extended <- calculate_tdm(data, 2020, additional_groups)
  expect_equal(setdiff(names(extended), names(minimum)), additional_groups)
})

test_that("is sensitive to additional groups", {
  additional_groups <- c("investor_name", "portfolio_name")
  n_groups <- length(crucial_tdm_groups()) + length(additional_groups)
  start_year <- 2020
  # Each group has values for years at t0, t0+5, and t0+10
  years <- rep(c(start_year, start_year + 5, start_year + 10), n_groups)

  data <- fake_tdm_data(
    portfolio_name = rep(c("portfolio a", "portfolio b"), each = 6),
    technology = rep(rep(c("RenewablesCap", "OilCap"), each = 3), 2),
    year = years,
    plan_alloc_wt_tech_prod = c(1, 1, 1, 1, 3, 4, 1, 2, 3, 1, 1, 1),
    scen_alloc_wt_tech_prod = c(1, 3, 4, 1, 0.5, 0.25, 1, 3, 4, 1, 0.5, 0.25),
    plan_carsten = c(0.5, 0.25, 0.2, 0.5, 0.75, 0.8, 0.5, 0.66, 0.75, 0.5, 0.33, 0.25),
  )

  minimum <- calculate_tdm(data, start_year)
  extended <- calculate_tdm(data, start_year, additional_groups)
  expect_false(identical(minimum, extended[names(minimum)]))
})

test_that("with known input outputs as expected", {
  data <- fake_tdm_data(
    technology = rep(c("RenewablesCap", "OilCap"), each = 4),
    year = rep(c(2020, 2021, 2025, 2030), 2),
    plan_alloc_wt_tech_prod = c(1, 1, 1, 1, 1, 2, 3, 4),
    scen_alloc_wt_tech_prod = c(1, 2, 3, 4, 1, 0.75, 0.5, 0.25),
    plan_carsten = c(0.5, 0.3, 0.25, 0.2, 0.5, 0.7, 0.75, 0.8),
  )

  out <- calculate_tdm(data, 2020)

  out <- out %>%
    mutate(
      tdm_tech = round(tdm_tech, 2),
      tdm_sec = round(tdm_sec, 2)
      ) %>%
    split(.$technology)

  expect_equal(out$OilCap$tdm_tech, 7.33)
  expect_equal(out$OilCap$tdm_sec, 4.67)
  expect_equal(out$RenewablesCap$tdm_tech, 2)
  expect_equal(out$RenewablesCap$tdm_sec, 4.67)

})
