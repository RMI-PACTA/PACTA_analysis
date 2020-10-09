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
