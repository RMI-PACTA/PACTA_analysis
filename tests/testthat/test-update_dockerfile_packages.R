test_that("updates a dockerfile with the given R packages file", {
  dockerfile <- tempfile()
  writeLines(create_empty_dockerfile("library(dplyr)"), dockerfile)
  update_dockerfile_packages(dockerfile, pkg_names = c("a", "b"))

  expect_true(any(grepl("a", readLines(dockerfile))))
  expect_true(any(grepl("b", readLines(dockerfile))))
  expect_false(any(grepl("dplyr", readLines(dockerfile))))
})

