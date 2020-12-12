test_that("can use configuration file via options", {
  config_file <- glue::glue("
    default:
      a_field: a_value
  ")
  config_path <- tempfile()
  readr::write_lines(config_file, config_path)

  withr::local_options(list(r2dii_config = config_path))
  expect_equal(get_param("a_field")(), "a_value")
})

test_that("outputs the expected value", {
  config_file <- glue::glue("
    default:
      a_field: a_value
  ")
  config_path <- tempfile()
  readr::write_lines(config_file, config_path)

  expect_equal(get_param("a_field")(config_path), "a_value")
})

test_that("with inexistent field outputs NULL", {
  config_file <- glue::glue("
    default:
      a_field: a_value
  ")
  config_path <- tempfile()
  readr::write_lines(config_file, config_path)

  expect_equal(get_param("bad_field")(config_path), NULL)
})

test_that("with NULL value and if_null = stop, errors gracefully", {
  config_file <- glue::glue("
    default:
      a_field:
  ")
  config_path <- tempfile()
  readr::write_lines(config_file, config_path)

  expect_error(get_param("bad_field")(config_path), NA)
  expect_error(
    get_param("bad_field", if_null = stop)(config_path),
    "must be not `NULL`"
  )
})
