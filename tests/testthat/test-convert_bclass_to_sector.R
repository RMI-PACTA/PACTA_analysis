test_that("convert_bclass_to_sector() function exists", {
  expect_true(
    class(convert_bclass_to_sector) == "function"
  )
})

test_that("convert_bclass_to_sector() returns NA for an unknown string", {
  skip_check_but_run_test()
  setwd(here::here())
  expect_true(
    is.na(convert_bclass_to_sector("XXX"))
  )
})

test_that("convert_bclass_to_sector() returns an appropriately matched string for an exisitng BICS group", {
  skip_check_but_run_test()
  setwd(here::here())
  expect_true(
    convert_bclass_to_sector("Natural Gas") == "Oil&Gas"
  )
})
