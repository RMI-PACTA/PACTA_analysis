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

  expect_vector(guess_delimiter(files[1L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[2L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[3L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[4L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[5L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files), ptype = character(), size = 5L)

  files_df <- data.frame(file = files)
  expect_vector(mutate(files_df, encoding = guess_delimiter(file))$encoding, ptype = character(), size = 5)
  expect_vector(guess_delimiter(files_df$file), ptype = character(), size = 5L)
  expect_vector(guess_delimiter(files_df["file"]), ptype = character(), size = 5L)
  expect_vector(guess_delimiter(files_df[1L]), ptype = character(), size = 5L)
  expect_vector(guess_delimiter(files_df[["file"]]), ptype = character(), size = 5L)
  expect_vector(guess_delimiter(files_df[[1L]]), ptype = character(), size = 5L)

  expect_vector(guess_delimiter(1L:2L), ptype = character(), size = 2L)
  expect_vector(guess_delimiter(c(TRUE, FALSE)), ptype = character(), size = 2L)
  expect_vector(guess_delimiter(NA), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(c(NA, NA)), ptype = character(), size = 2L)
})
