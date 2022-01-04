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

  expect_vector(guess_file_mime_type(files[1]), ptype = character(), size = 1)
  expect_vector(guess_file_mime_type(files[2]), ptype = character(), size = 1)
  expect_vector(guess_file_mime_type(files[3]), ptype = character(), size = 1)
  expect_vector(guess_file_mime_type(files[4]), ptype = character(), size = 1)
  expect_vector(guess_file_mime_type(files), ptype = character(), size = 4)

  files_df <- data.frame(file = files)
  expect_vector(dplyr::mutate(files_df, mime = guess_file_mime_type(file))$mime, ptype = character(), size = 4)
  expect_vector(guess_file_mime_type(files_df$file), ptype = character(), size = 4)
  expect_vector(guess_file_mime_type(files_df["file"]), ptype = character(), size = 4)
  expect_vector(guess_file_mime_type(files_df[1]), ptype = character(), size = 4)
  expect_vector(guess_file_mime_type(files_df[["file"]]), ptype = character(), size = 4)
  expect_vector(guess_file_mime_type(files_df[[1]]), ptype = character(), size = 4)

  expect_vector(guess_file_mime_type(1:2), ptype = character(), size = 2)
  expect_vector(guess_file_mime_type(c(TRUE, FALSE)), ptype = character(), size = 2)
  expect_vector(guess_file_mime_type(NA), ptype = character(), size = 1)
  expect_vector(guess_file_mime_type(c(NA, NA)), ptype = character(), size = 2)
})
