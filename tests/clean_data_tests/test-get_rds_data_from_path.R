setwd(here::here())
function_name <- "get_rds_data_from_path"
result <- get_rds_data_from_path(path = "data", filename = "bics_bridge.rds")

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})
