test_that("`quit_if_no_pacta_relevant_data()` quits if no ABCD in non-interactive", {
  expect_null({
    callr::r(function() {
      devtools::load_all()
      source(here::here("0_global_functions.R"))
      port_with_no_data <- data.frame(has_asset_level_data = FALSE)
      outputs_path <<- tempdir()
      portfolio_name_ref_all <<- "TestPort"
      log_path <<- outputs_path
      dir.create(
        path = file.path(outputs_path, portfolio_name_ref_all, "report"),
        recursive = TRUE
      )
      dir.create(
        path = file.path("inst", "rmd"),
        recursive = TRUE
      )
      file.copy(here::here("inst/rmd/user_errors.Rmd"), "inst/rmd/user_errors.Rmd")
      quit_if_no_pacta_relevant_data(port_with_no_data)
      TRUE
    })
  })
})

test_that("`quit_if_no_pacta_relevant_data()` passes if has ABCD in non-interactive", {
  expect_true({
    callr::r(function() {
      devtools::load_all()
      source(here::here("0_global_functions.R"))
      port_with_no_data <- data.frame(has_asset_level_data = TRUE)
      outputs_path <<- tempdir()
      portfolio_name_ref_all <<- "TestPort"
      log_path <<- outputs_path
      dir.create(
        path = file.path(outputs_path, portfolio_name_ref_all, "report"),
        recursive = TRUE
      )
      dir.create(
        path = file.path("inst", "rmd"),
        recursive = TRUE
      )
      file.copy(here::here("inst/rmd/user_errors.Rmd"), "inst/rmd/user_errors.Rmd")
      quit_if_no_pacta_relevant_data(port_with_no_data)
      TRUE
    })
  })
})
