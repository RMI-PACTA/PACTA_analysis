test_that("always outputs a character vector", {
  # possible filepaths
  non_existant_file <- "xxx"

  directory <- withr::local_tempdir()

  empty_file <- withr::local_tempfile()
  invisible(file.create(empty_file))

  binary_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS("XXX", binary_file)

  txt_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("XXX", txt_file)

  csv_file <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1, b = 2), file = csv_file)

  tsv_file <- withr::local_tempfile(fileext = ".tsv")
  write.table(data.frame(a = 1, b = 2), sep = "\t", row.names = FALSE,
              quote = FALSE, file = tsv_file)

  tar_file <- withr::local_tempfile(fileext = ".tar")
  tar(tar_file, csv_file)

  tar_file_with_csv_ext <- withr::local_tempfile(fileext = ".csv")
  tar(tar_file_with_csv_ext, csv_file)

  files <- c(non_existant_file, directory, empty_file, binary_file, txt_file,
             csv_file, tsv_file, tar_file, tar_file_with_csv_ext)

  expect_vector(guess_file_encoding(files[1L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[2L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[3L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[4L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[5L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[6L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[7L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[8L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files[9L]), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(files), ptype = character(), size = 9L)

  # expected possible uses
  files_df <- data.frame(file = files)

  out <- dplyr::mutate(files_df, encoding = guess_file_encoding(file))$encoding
  expect_vector(out, ptype = character(), size = 9L)

  out <- guess_file_encoding(files_df$file)
  expect_vector(out, ptype = character(), size = 9L)

  out <- guess_file_encoding(files_df["file"])
  expect_vector(out, ptype = character(), size = 9L)

  out <- guess_file_encoding(files_df[1L])
  expect_vector(out, ptype = character(), size = 9L)

  out <- guess_file_encoding(files_df[["file"]])
  expect_vector(out, ptype = character(), size = 9L)

  out <- guess_file_encoding(files_df[[1L]])
  expect_vector(out, ptype = character(), size = 9L)

  # unexpected input types
  expect_vector(guess_file_encoding(1L), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(1L:2L), ptype = character(), size = 2L)
  expect_vector(guess_file_encoding(TRUE), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(FALSE), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(c(TRUE, FALSE)), ptype = character(), size = 2L)
  expect_vector(guess_file_encoding(NA), ptype = character(), size = 1L)
  expect_vector(guess_file_encoding(c(NA, NA)), ptype = character(), size = 2L)
})
