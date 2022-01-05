test_that("outputs logical vector", {
  directory <- withr::local_tempdir()

  empty_file <- withr::local_tempfile()
  invisible(file.create(empty_file))

  no_read_access <- withr::local_tempfile()
  saveRDS("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  nonbinary_file <- withr::local_tempfile()
  writeLines("XXX", nonbinary_file)

  accessible_file <- withr::local_tempfile()
  saveRDS("XXX", accessible_file)

  files <- c(directory, empty_file, no_read_access, nonbinary_file, accessible_file)

  expect_vector(is_binary_file(files[1L]), ptype = logical(), size = 1L)
  expect_vector(is_binary_file(files[2L]), ptype = logical(), size = 1L)
  expect_vector(is_binary_file(files[3L]), ptype = logical(), size = 1L)
  expect_vector(is_binary_file(files[4L]), ptype = logical(), size = 1L)
  expect_vector(is_binary_file(files[5L]), ptype = logical(), size = 1L)
  expect_vector(is_binary_file(files), ptype = logical(), size = 5L)

  files_df <- data.frame(file = files)
  expect_vector(dplyr::mutate(files_df, is_binary = is_binary_file(file))$is_binary, ptype = logical(), size = 5L)
  expect_vector(is_binary_file(files_df$file), ptype = logical(), size = 5L)
  expect_vector(is_binary_file(files_df["file"]), ptype = logical(), size = 5L)
  expect_vector(is_binary_file(files_df[1L]), ptype = logical(), size = 5L)
  expect_vector(is_binary_file(files_df[["file"]]), ptype = logical(), size = 5L)
  expect_vector(is_binary_file(files_df[[1L]]), ptype = logical(), size = 5L)

  expect_vector(is_binary_file(1L:2L), ptype = logical(), size = 2L)
  expect_vector(is_binary_file(c(TRUE, FALSE)), ptype = logical(), size = 2L)
  expect_vector(is_binary_file(NA), ptype = logical(), size = 1L)
  expect_vector(is_binary_file(c(NA, NA)), ptype = logical(), size = 2L)
})
