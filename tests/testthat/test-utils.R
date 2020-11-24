test_that("use_r_packages() puts listed packages on search() path", {
  package <- "grid"
  use_package <- sprintf("library('%s')", package)

  path <- tempfile("packages", fileext = ".R")
  writeLines(use_package, path)

  is_attached <- function(package) package %in% .packages()

  if (is_attached(package)) {
    stop("This test is useful only if `package` is not in .packages().")
  } else {
    use_r_packages(path)
    on.exit(detach(paste0("package:", package), character.only = TRUE))
    expect_true(is_attached(package))
  }
})

test_that("helper skip_check_but_run_test() works as expected", {
  # This test should not be skipped during devtools::test()
  # This test should be skipped during devtools::check()
  # If you remove `skip_check_but_run_test()`, devtools::check() should error
  skip_check_but_run_test()

  # This file should be available only when the source code is accessible, i.e.
  # not during R CMD check, when the code runs from the system installation
  expect_true(fs::file_exists(here::here(".pacta")))
})
