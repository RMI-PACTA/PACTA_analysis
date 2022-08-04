set_web_parameters <- function(file_path) {
  cfg <- config::get(file = file_path)

  project_location_ext <<- cfg$paths$project_location_ext
  data_location_ext <<- cfg$paths$data_location_ext
  template_path <<- cfg$paths$template_location
  stress_test_path <<- cfg$paths$stress_test_location
  stress_test_data_location <<- cfg$paths$stress_test_data_location
  user_results_path <<- cfg$paths$user_data_location

  project_name <<- cfg$parameters$project_name
  twodii_internal <<- cfg$parameters$twodii_internal
  new_data <<- cfg$parameters$new_data

  financial_timestamp <<- cfg$parameters$financial_timestamp
}
