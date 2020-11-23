setwd(here::here())


test_that("convert_icb_to_sector() function exists", {
  expect_true(
    class(convert_icb_to_sector) == "function"
  )
})

test_that("convert_icb_to_sector() returns NA for an unknown string", {
  expect_true(
    is.na(convert_icb_to_sector("XXX"))
  )
})

test_that("convert_icb_to_sector() returns an appropriately matched string for an exisitng ICB group", {
  expect_true(
    convert_icb_to_sector("Aerospace") == "Other"
  )
})
