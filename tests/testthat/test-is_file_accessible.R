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

  expect_vector(is_file_accessible(files[1L]), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(files[2L]), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(files[3L]), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(files[4L]), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(files), ptype = logical(), size = 4L)

  files_df <- data.frame(file = files)
  expect_vector(dplyr::mutate(files_df, accessible = is_file_accessible(file))$accessible, ptype = logical(), size = 4L)
  expect_vector(is_file_accessible(files_df$file), ptype = logical(), size = 4L)
  expect_vector(is_file_accessible(files_df["file"]), ptype = logical(), size = 4L)
  expect_vector(is_file_accessible(files_df[1L]), ptype = logical(), size = 4L)
  expect_vector(is_file_accessible(files_df[["file"]]), ptype = logical(), size = 4L)
  expect_vector(is_file_accessible(files_df[[1L]]), ptype = logical(), size = 4L)

  expect_vector(is_file_accessible(1L:2L), ptype = logical(), size = 2L)
  expect_vector(is_file_accessible(c(TRUE, FALSE)), ptype = logical(), size = 2L)
  expect_vector(is_file_accessible(c(NA, NA)), ptype = logical(), size = 2L)
})
