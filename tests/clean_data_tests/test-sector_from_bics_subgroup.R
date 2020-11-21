setwd(here::here())
function_name <- "sector_from_bics_subgroup"
result <- sector_from_bics_subgroup(c("XXX"))

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})
