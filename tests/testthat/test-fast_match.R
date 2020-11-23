setwd(here::here())
function_name <- "fast_match"
result <- fast_match(x = c("A", "B"),
                     dict = data.frame(from = c("A", "B"), to = c("a", "b")))

test_that(paste0(function_name, "() function exists"), {
  expect_true(
    exists(function_name)
  )
})
