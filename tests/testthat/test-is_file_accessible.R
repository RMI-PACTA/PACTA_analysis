test_that("outputs logical vector", {
  directory <- file.path(tempdir(), "directory")
  dir.create(directory)

  empty_file <- tempfile()
  invisible(file.create(empty_file))

  no_read_access <- tempfile()
  writeLines("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  accessible_file <- tempfile()
  writeLines("XXX", accessible_file)

  files <- c(directory, empty_file, no_read_access, accessible_file)

  expect_vector(is_file_accessible(files[1]), ptype = logical(), size = 1)
  expect_vector(is_file_accessible(files[2]), ptype = logical(), size = 1)
  expect_vector(is_file_accessible(files[3]), ptype = logical(), size = 1)
  expect_vector(is_file_accessible(files[4]), ptype = logical(), size = 1)
  expect_vector(is_file_accessible(files), ptype = logical(), size = 4)

  files_df <- data.frame(file = files)
  expect_vector(dplyr::mutate(files_df, accessible = is_file_accessible(file))$accessible, ptype = logical(), size = 4)
  expect_vector(is_file_accessible(files_df$file), ptype = logical(), size = 4)
  expect_vector(is_file_accessible(files_df["file"]), ptype = logical(), size = 4)
  expect_vector(is_file_accessible(files_df[1]), ptype = logical(), size = 4)
  expect_vector(is_file_accessible(files_df[["file"]]), ptype = logical(), size = 4)
  expect_vector(is_file_accessible(files_df[[1]]), ptype = logical(), size = 4)

  expect_vector(is_file_accessible(1:2), ptype = logical(), size = 2)
  expect_vector(is_file_accessible(c(TRUE, FALSE)), ptype = logical(), size = 2)
  expect_vector(is_file_accessible(c(NA, NA)), ptype = logical(), size = 2)
})
