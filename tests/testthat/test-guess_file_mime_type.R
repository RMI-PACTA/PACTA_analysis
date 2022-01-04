test_that("outputs character vector", {
  directory <- file.path(tempdir(), "directory")
  dir.create(directory, showWarnings = FALSE)

  empty_file <- tempfile()
  invisible(file.create(empty_file))

  no_read_access <- tempfile()
  saveRDS("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  accessible_file <- tempfile()
  writeLines("XXX", accessible_file)

  files <- c(directory, empty_file, no_read_access, accessible_file)

  expect_vector(guess_file_mime_type(files[1L]), ptype = character(), size = 1L)
  expect_vector(guess_file_mime_type(files[2L]), ptype = character(), size = 1L)
  expect_vector(guess_file_mime_type(files[3L]), ptype = character(), size = 1L)
  expect_vector(guess_file_mime_type(files[4L]), ptype = character(), size = 1L)
  expect_vector(guess_file_mime_type(files), ptype = character(), size = 4L)

  files_df <- data.frame(file = files)
  expect_vector(dplyr::mutate(files_df, mime = guess_file_mime_type(file))$mime, ptype = character(), size = 4L)
  expect_vector(guess_file_mime_type(files_df$file), ptype = character(), size = 4L)
  expect_vector(guess_file_mime_type(files_df["file"]), ptype = character(), size = 4L)
  expect_vector(guess_file_mime_type(files_df[1L]), ptype = character(), size = 4L)
  expect_vector(guess_file_mime_type(files_df[["file"]]), ptype = character(), size = 4L)
  expect_vector(guess_file_mime_type(files_df[[1L]]), ptype = character(), size = 4L)

  expect_vector(guess_file_mime_type(1L:2L), ptype = character(), size = 2L)
  expect_vector(guess_file_mime_type(c(TRUE, FALSE)), ptype = character(), size = 2L)
  expect_vector(guess_file_mime_type(NA), ptype = character(), size = 1L)
  expect_vector(guess_file_mime_type(c(NA, NA)), ptype = character(), size = 2L)
})
