test_that("updates a dockerfile with the given R packages file", {
  dockerfile <- tempfile()
  writeLines(create_empty_dockerfile("library(dplyr)"), dockerfile)

  r_packages <- tempfile()
  writeLines("library(fs)", r_packages)

  update_dockerfile_packages(dockerfile, r_packages)
  expect_true(any(grepl("fs", readLines(dockerfile))))
  expect_false(any(grepl("dplyr", readLines(dockerfile))))
})

