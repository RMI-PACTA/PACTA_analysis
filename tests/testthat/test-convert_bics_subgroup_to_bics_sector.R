test_that("convert_bics_subgroup_to_bics_sector() function exists", {
  expect_true(
    class(convert_bics_subgroup_to_bics_sector) == "function"
  )
})

test_that("convert_bics_subgroup_to_bics_sector() returns NA for an unknown string", {
  skip_check_but_run_test()
  setwd(here::here())
  expect_true(
    is.na(convert_bics_subgroup_to_bics_sector("XXX"))
  )
})

test_that("convert_bics_subgroup_to_bics_sector() returns an appropriately matched string for an exisitng BICS group", {
  skip_check_but_run_test()
  setwd(here::here())
  expect_true(
    convert_bics_subgroup_to_bics_sector("Airlines") == "Consumer Discretionary"
  )
})
