test_that("with bad `data` errors with informative message", {
  expect_error(
    calculate_transition_disruption_metric("bad", 2020),
    "data.frame.*not.*TRUE"
  )

  expect_error(
    calculate_transition_disruption_metric(fake_pacta_results(), "bad"),
    "numeric.*not.*TRUE"
  )
})

test_that("outputs a tibble", {
  out <- calculate_transition_disruption_metric(
    fake_pacta_results(
      year = c(2020, 2025, 2030)
    ),
    2020
  )
  expect_is(out, "tbl_df")
})

test_that("outputs is ungrouped", {
  out <- calculate_transition_disruption_metric(
    fake_pacta_results(
      year = c(2020, 2025, 2030)
    ),
    2020
  )
  expect_false(dplyr::is_grouped_df(out))
})

test_that("with data lacking crucial columns errors with informative message", {
  expect_error_missing_names <- function(name) {
    bad_pacta_results <- dplyr::rename(
      fake_pacta_results(year = c(2020, 2025, 2030)),
      bad = name
    )

    expect_error(
      class = "missing_names",
      calculate_transition_disruption_metric(
        bad_pacta_results,
        2020
      )
    )
  }

  expect_error_missing_names("allocation")
  expect_error_missing_names("ald_sector")
  expect_error_missing_names("technology")
  expect_error_missing_names("year")
  expect_error_missing_names("plan_carsten")
  expect_error_missing_names("scen_alloc_wt_tech_prod")
  expect_error_missing_names("plan_alloc_wt_tech_prod")
})

test_that("warns if input data has only `ownership_weight` values of allocation", {
  expect_warning(
    calculate_transition_disruption_metric(
      fake_pacta_results(
        allocation = "ownership_weight",
        year = c(2020, 2025, 2030)
      ),
      2020
    ),
    class = "has_zero_rows"
  )
})
