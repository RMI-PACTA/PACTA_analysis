test_that("use_r_packages() puts listed packages on search() path", {
  package <- "grid"
  use_package <- sprintf("library('%s')", package)

  path <- tempfile("packages", fileext = ".R")
  writeLines(use_package, path)

  is_attached <- function(package) package %in% .packages()

  if (!is_attached(package)) {
    use_r_packages(path)
    on.exit(detach(paste0("package:", package), character.only = TRUE))
  } else {
    stop("This test is wrong.")
  }

  expect_true(is_attached(package))
})
