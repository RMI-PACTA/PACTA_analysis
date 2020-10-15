test_that("outputs the expected tibble", {
  project <- path(tempdir(), "project")
  dir_create(project)
  on.exit(dir_delete(project), add = TRUE)

  writeLines("library(fs)", path(project, "r_packages.R"))
  out <- find_dependencies(project)

  is_tibble <- "tbl_df" %in% class(out)
  expect_true(is_tibble)

  expect_named(out, c("source", "package", "require", "version", "dev"))
})

test_that("detects the expected dependency", {
  project <- path(tempdir(), "project")
  dir_create(project)
  on.exit(dir_delete(project), add = TRUE)

  writeLines("library(fs)", path(project, "r_packages.R"))
  out <- find_dependencies(project)

  expect_equal(out$package, "fs")
})

test_that("ignores files in .renvignore", {
  project <- path(tempdir(), "project")
  dir_create(project)
  on.exit(dir_delete(project), add = TRUE)

  # Helps renv find the project root
  # https://github.com/rstudio/renv/issues/439#issuecomment-634268728
  dir_create(path(project, ".git"))

  writeLines("library(fs)", path(project, "find_me.R"))
  writeLines("library(glue)", path(project, "ignore_me.R"))
  writeLines("ignore_me.R", path(project, ".renvignore"))
  out <- find_dependencies(project)

  expect_equal(out$package, "fs")
})
