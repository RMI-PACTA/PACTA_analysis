setwd(here::here())
function_name <- "save_files_to"

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})
