test_that("get_bics_bridge() function exists", {
  expect_true(
    class(get_bics_bridge) == "function"
  )
})

test_that("get_bics_bridge() uses a function name that conforms to the list of data_object_names", {
  expect_true(
    sub("^get_", "", deparse(substitute(get_bics_bridge))) %in% data_object_names
  )
})

test_that("get_bics_bridge() returns a data.frame", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_bics_bridge()
  expect_true(
    inherits(result, "data.frame")
  )
})

test_that("get_bics_bridge() returns a data.frame with 2 columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_bics_bridge()
  expect_true(
    ncol(result) == 2
  )
})

test_that("get_bics_bridge() returns a data.frame with the proper column names", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_bics_bridge()
  expect_equal(
    names(result),
    c("bics_sector", "bics_subgroup")
  )
})

test_that("get_bics_bridge() returns a data.frame with proper character class columns", {
  skip_check_but_run_test()
  setwd(here::here())
  result <- get_bics_bridge()
  char_cols <- c("bics_sector", "bics_subgroup")
  expect_true(
    all(sapply(result[char_cols], class) == "character")
  )
})
