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

  expect_vector(guess_file_encoding(files[1]), ptype = character(), size = 1)
  expect_vector(guess_file_encoding(files[2]), ptype = character(), size = 1)
  expect_vector(guess_file_encoding(files[3]), ptype = character(), size = 1)
  expect_vector(guess_file_encoding(files[4]), ptype = character(), size = 1)
  expect_vector(guess_file_encoding(files[5]), ptype = character(), size = 1)
  expect_vector(guess_file_encoding(files), ptype = character(), size = 5)

  files_df <- data.frame(file = files)
  expect_vector(mutate(files_df, encoding = guess_file_encoding(file))$encoding, ptype = character(), size = 5)
  expect_vector(guess_file_encoding(files_df$file), ptype = character(), size = 5)
  expect_vector(guess_file_encoding(files_df["file"]), ptype = character(), size = 5)
  expect_vector(guess_file_encoding(files_df[1]), ptype = character(), size = 5)
  expect_vector(guess_file_encoding(files_df[["file"]]), ptype = character(), size = 5)
  expect_vector(guess_file_encoding(files_df[[1]]), ptype = character(), size = 5)

  expect_vector(guess_file_encoding(1:2), ptype = character(), size = 2)
  expect_vector(guess_file_encoding(c(TRUE, FALSE)), ptype = character(), size = 2)
  expect_vector(guess_file_encoding(NA), ptype = character(), size = 1)
  expect_vector(guess_file_encoding(c(NA, NA)), ptype = character(), size = 2)
})
