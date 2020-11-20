setwd(here::here())
function_name <- "get_sector_bridge"
result <- get_sector_bridge()

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})

test_that(paste0(function_name, "() returns a data.frame"), {
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that(paste0(function_name, "() returns a data.frame with 8 columns"), {
  expect_true(
    ncol(result) == 8
  )
})

test_that(paste0(function_name, "() returns a data.frame with the proper column names"), {
  expect_equal(
    names(result),
    c("industry_classification", "source", "sector", "sector_ipr",
      "subsector_ipr", "sector_dnb", "sector_boe", "subsector_boe")
  )
})

test_that(paste0(function_name, "() returns a data.frame with proper character class columns"), {
  char_cols <- c("industry_classification", "source", "sector", "sector_ipr",
                 "subsector_ipr", "sector_dnb", "sector_boe", "subsector_boe")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})
