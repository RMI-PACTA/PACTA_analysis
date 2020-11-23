setwd(here::here())
function_name <- "sector_from_bics_subgroup"


test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns NA for an unknown string"), {
  expect_true(
    is.na(sector_from_bics_subgroup("XXX"))
  )
})

test_that(paste0(function_name, "() returns an appropriately matched string for an exisitng BICS group"), {
  expect_true(
    sector_from_bics_subgroup("Advertising Agencies") == "Other"
  )
})
