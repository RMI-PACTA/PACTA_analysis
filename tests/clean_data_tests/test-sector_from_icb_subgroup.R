setwd(here::here())
function_name <- "sector_from_icb_subgroup"
result <- sector_from_icb_subgroup(c("XXX"))

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})
