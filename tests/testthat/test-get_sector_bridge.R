test_that("get_sector_bridge() function exists", {
  expect_true(
    class(get_sector_bridge) == "function"
  )
})

test_that("get_sector_bridge() returns a data.frame", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_sector_bridge()
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_sector_bridge() returns a data.frame with 8 columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_sector_bridge()
  expect_true(
    ncol(result) == 8
  )
})

test_that("get_sector_bridge() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_sector_bridge()
  expect_equal(
    names(result),
    c("industry_classification", "source", "sector", "sector_ipr",
      "subsector_ipr", "sector_dnb", "sector_boe", "subsector_boe")
  )
})

test_that("get_sector_bridge() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_sector_bridge()
  char_cols <- c("industry_classification", "source", "sector", "sector_ipr",
                 "subsector_ipr", "sector_dnb", "sector_boe", "subsector_boe")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})
