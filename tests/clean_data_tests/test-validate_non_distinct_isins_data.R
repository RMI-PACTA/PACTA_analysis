setwd(here::here())
function_name <- "validate_non_distinct_isins_data"
example_data <- readRDS("data/non_distinct_isins.rds")


test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns TRUE for example data"), {
  expect_true(
    do.call(function_name, list(example_data))
  )
})

test_that(paste0(function_name, "() returns FALSE for data with no columns"), {
  expect_false(
    do.call(function_name, list(data.frame()))
  )
})

test_that(paste0(function_name, "() returns FALSE for data with a specified column missing"), {
  expect_false(
    do.call(function_name, list(select(example_data, -1)))
  )
})

test_that(paste0(function_name, "() returns FALSE for data with an unspecified column"), {
  expect_false(
    do.call(function_name, list(mutate(example_data, XXX = TRUE)))
  )
})

test_that(paste0(function_name, "() returns FALSE for data with a column of a different type"), {
  expect_false(
    do.call(function_name, list(mutate(example_data, !!names(example_data)[1] := TRUE)))
  )
})

