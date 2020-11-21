setwd(here::here())
function_name <- "validate_ungrouped"


test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns TRUE if the data is an ungrouped data frame"), {
  expect_true(
    validate_ungrouped(data.frame(a = 1))
  )
})

test_that(paste0(function_name, "() returns TRUE if the data is an ungrouped tibble"), {
  expect_true(
    validate_ungrouped(tibble(a = 1))
  )
})

test_that(paste0(function_name, "() returns FALSE if the data is a grouped tibble"), {
  expect_false(
    validate_ungrouped(group_by(tibble(a = 1), a))
  )
})
