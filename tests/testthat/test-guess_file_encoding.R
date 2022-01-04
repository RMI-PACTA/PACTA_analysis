test_that("outputs character vector", {
  directory <- file.path(tempdir(), "directory")
  dir.create(directory, showWarnings = FALSE)

  empty_file <- tempfile()
  invisible(file.create(empty_file))

  no_read_access <- tempfile()
  saveRDS("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  binary_file <- tempfile()
  saveRDS("XXX", binary_file)

  accessible_file <- tempfile()
  writeLines("XXX", accessible_file)

  files <- c(directory, empty_file, no_read_access, binary_file, accessible_file)

  expect_vector(guess_file_encoding(files[1L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[2L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[3L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[4L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[5L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files), ptype = character(), size = 5L)

  files_df <- data.frame(file = files)
  expect_vector(dplyr::mutate(files_df, encoding = guess_file_encoding(file))$encoding, ptype = character(), size = 5L)
  expect_vector(guess_file_encoding(files_df$file), ptype = character(), size = 5L)
  expect_vector(guess_file_encoding(files_df["file"]), ptype = character(), size = 5L)
  expect_vector(guess_file_encoding(files_df[1L]), ptype = character(), size = 5L)
  expect_vector(guess_file_encoding(files_df[["file"]]), ptype = character(), size = 5L)
  expect_vector(guess_file_encoding(files_df[[1L]]), ptype = character(), size = 5L)

  expect_vector(guess_file_encoding(1L:2L), ptype = character(), size = 2L)
  expect_vector(guess_file_encoding(c(TRUE, FALSE)), ptype = character(), size = 2L)
  expect_vector(guess_file_encoding(NA), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(c(NA, NA)), ptype = character(), size = 2L)
})
