setwd(here::here())
function_name <- "convert_industry_classification"
result <- convert_industry_classification(c("XXX"), from = "BICS", to = "sector")

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})
