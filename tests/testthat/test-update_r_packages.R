test_that("updates a file as epected", {
  project <- path(tempdir(), "project")
  dir_create(project)
  on.exit(dir_delete(project), add = TRUE)

  pkgs_path <- path(project, "r_packages.R")
  writeLines("library(fs)", pkgs_path)
  update_r_packages(pkgs_path, project)

  expect_equal(readLines(pkgs_path), "library(fs)")
})
