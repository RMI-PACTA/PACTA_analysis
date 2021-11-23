test_that("with bad `data` errors with informative message", {
  expect_error(calculate_tdm("bad", 2020), "data.frame.*not.*TRUE")
  expect_error(calculate_tdm(fake_tdm_data(), "bad"), "numeric.*not.*TRUE")
  expect_error(calculate_tdm(fake_tdm_data(), 2020, delta_t1 = "bad"), "numeric.*not.*TRUE")
  expect_error(calculate_tdm(fake_tdm_data(), 2020, delta_t2 = "bad"), "numeric.*not.*TRUE")
  expect_error(calculate_tdm(fake_tdm_data(), 2020, additional_groups = TRUE), "character.*not.*TRUE")
})

test_that("outputs the expected tibble", {
  data <- fake_tdm_data(
    year = c(2020, 2025, 2030),
    scen_alloc_wt_tech_prod = 1:3
  )

  out <- calculate_tdm(data, 2020)

expect_equal(
    sort(names(out)),
    c(
      "ald_sector",
      "tdm_delta_t1",
      "tdm_delta_t2",
      "tdm_metric",
      "tdm_portfolio_value",
      "tdm_sector_value",
      "tdm_t0",
      "tdm_technology_value",
      "technology"
      )
  )

out <- split(out, out$tdm_metric)

expect_equal(out$portfolio$tdm_technology_value, 2L)
expect_equal(out$portfolio$tdm_sector_value, 2L)
expect_equal(out$portfolio$tdm_portfolio_value, 2L)
expect_equal(out$portfolio$tdm_t0, 2020L)
expect_equal(out$portfolio$tdm_delta_t1, 5L)
expect_equal(out$portfolio$tdm_delta_t2, 10L)

expect_equal(out$scenario$tdm_technology_value, 1L)
expect_equal(out$scenario$tdm_sector_value, 1L)
expect_equal(out$scenario$tdm_portfolio_value, 1L)
expect_equal(out$scenario$tdm_t0, 2020L)
expect_equal(out$scenario$tdm_delta_t1, 5L)
expect_equal(out$scenario$tdm_delta_t2, 10L)

})

test_that("outputs the expected names", {
  expected <- names(tdm_prototype())
  data <- fake_tdm_data(
    year = c(2020, 2025, 2030),
    scen_alloc_wt_tech_prod = 1:3
  )

  actual <- names(calculate_tdm(data, 2020))
  expect_equal(setdiff(expected, actual), character(0))
})

test_that("outputs 0 for `tdm_technology_value` and `tdm_sector_value`, if `scen_alloc_wt_tech_prod
          doesn't change over 10 year period`", {
  data <- fake_tdm_data(year = c(2020, 2025, 2030))
  out <- calculate_tdm(data, 2020) %>%
    split(.$tdm_metric)

  expect_true(out$portfolio$tdm_technology_value == 0)
  expect_true(out$portfolio$tdm_sector_value == 0)
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

test_that("if no `scenarios` found in data, outputs a 0-row tibble", {
  fake_tdm_data(scenario = "bad scenario", year = c(2020, 2025, 2030)) %>%
    calculate_tdm(2020) %>%
    expect_s3_class("tbl") %>%
    expect_warning(class = "has_zero_rows")
})

test_that("errors if data has multiple values per year", {
  fake_tdm_data(
    year = c(2020, 2020, 2025, 2030),
    plan_alloc_wt_tech_prod = c(1, 1, 3, 4),
    scen_alloc_wt_tech_prod = c(1, 2, 3, 4),
    plan_carsten = c(0.5, 0.3, 0.25, 0.2),
  ) %>%
    calculate_tdm(2020) %>%
    expect_error(class = "multiple_values_per_year")
})

test_that("errors if crucial years missing", {
  t0 <- 2020
  delta_t1 <- 5
  delta_t2 <- 10

  fake_tdm_data(
    year = c(2020, 2020, 2025, 2031),
    plan_alloc_wt_tech_prod = c(1, 1, 3, 4),
    scen_alloc_wt_tech_prod = c(1, 2, 3, 4),
    plan_carsten = c(0.5, 0.3, 0.25, 0.2),
  ) %>%
    calculate_tdm(t0, delta_t1, delta_t2) %>%
    expect_error(class = "missing_crucial_years")
})

test_that("errors if input data isn't grouped appropriately to ensure unique
          values per year", {
  additional_groups <- c("investor_name", "portfolio_name")
  n_groups <- length(crucial_tdm_groups()) + length(additional_groups)
  t0 <- 2020
  delta_t1 <- 5
  delta_t2 <- 10
  # Each group has values for years at t0, t0+5, and t0+10
  years <- rep(c(t0, t0 + delta_t1, t0 + delta_t2), n_groups)

  data <- fake_tdm_data(
    portfolio_name = rep(c("portfolio a", "portfolio b"), each = 6),
    technology = rep(rep(c("RenewablesCap", "OilCap"), each = 3), 2),
    year = years,
    plan_alloc_wt_tech_prod = c(1, 1, 1, 1, 3, 4, 1, 2, 3, 1, 1, 1),
    scen_alloc_wt_tech_prod = c(1, 3, 4, 1, 0.5, 0.25, 1, 3, 4, 1, 0.5, 0.25),
    plan_carsten = c(0.5, 0.25, 0.2, 0.5, 0.75, 0.8, 0.5, 0.66, 0.75, 0.5, 0.33, 0.25),
  )

  calculate_tdm(data, t0, delta_t1, delta_t2) %>%
    expect_error(class = "multiple_values_per_year")

  extended <- calculate_tdm(data, t0, delta_t1, delta_t2, additional_groups) %>%
    split(.$tdm_metric)

  expect_equal(round(extended$portfolio$tdm_technology_value, 2), c(2, 7.33, 1.33, 2))
  expect_equal(round(extended$portfolio$tdm_sector_value, 2), c(4.67, 4.67, 1.67, 1.67))
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
    filter(
      tdm_metric == "portfolio"
    ) %>%
    mutate(
      tdm_technology_value = round(tdm_technology_value, 2),
      tdm_sector_value = round(tdm_sector_value, 2)
    ) %>%
    split(.$technology)

  expect_equal(out$OilCap$tdm_technology_value, 7.33)
  expect_equal(out$OilCap$tdm_sector_value, 4.67)
  expect_equal(out$RenewablesCap$tdm_technology_value, 2)
  expect_equal(out$RenewablesCap$tdm_sector_value, 4.67)
})

test_that("outputs correctly for non-monotonic scenarios", {
  data <- fake_tdm_data(
    technology = rep(c("RenewablesCap", "OilCap"), each = 4),
    year = rep(c(2020, 2025, 2030, 2050), 2),
    plan_alloc_wt_tech_prod = c(rep(0.1, 4), rep(5, 4)),
    scen_alloc_wt_tech_prod = c(1, 0.5, 0.25, 4, 1, 2, 3, 0.25),
    plan_carsten = 0.5,
  )

  out <- calculate_tdm(data, 2020)

  out <- out %>%
    filter(
      tdm_metric == "portfolio"
    ) %>%
    mutate(
      tdm_technology_value = round(tdm_technology_value, 2),
      tdm_sector_value = round(tdm_sector_value, 2)
    ) %>%
    split(.$technology)

  expect_equal(out$OilCap$tdm_technology_value, 2)
  expect_equal(out$RenewablesCap$tdm_technology_value, 0.4)
})

test_that("only start_year carsten is used to aggregate tdm", {

  # only initial carsten value should be used
  carsten <- c(0.5, 0.5, 0.1, 0.9, 0, 1)

  data <- fake_tdm_data(
    technology = rep(c("RenewablesCap", "OilCap"), 3),
    year = rep(rep(c(2020, 2025, 2030), 2)),
    plan_alloc_wt_tech_prod = rep(c(1, 2, 3), each = 2),
    scen_alloc_wt_tech_prod = rep(c(1, 3, 5), each = 2),
    plan_carsten = carsten,
  )

  out <- calculate_tdm(data, t0 = 2020) %>%
    filter(tdm_metric == "portfolio") %>%
    split(.$technology)

  expect_equal(
    out$OilCap$tdm_sector_value,
    out$RenewablesCap$tdm_sector_value
    )

  expect_equal(
    out$OilCap$tdm_portfolio_value,
    out$RenewablesCap$tdm_portfolio_value
    )
})
