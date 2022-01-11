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
  expect_error(guess_file_encoding(1L))
  expect_error(guess_file_encoding(1L:2L))
  expect_error(guess_file_encoding(TRUE))
  expect_error(guess_file_encoding(FALSE))
  expect_error(guess_file_encoding(c(TRUE, FALSE)))
  expect_error(guess_file_encoding(NA))
  expect_error(guess_file_encoding(c(NA, NA)))
})

test_that("returns expected values", {
  # typical usage
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

  expect_identical(guess_file_encoding(files), c(NA, NA, NA, "binary", "ascii", "ascii", "ascii", "binary", "binary"))

  # test various encodings
  ascii <- withr::local_tempfile(fileext = ".csv")
  utf8 <- withr::local_tempfile(fileext = ".csv")
  iso8859_1 <- withr::local_tempfile(fileext = ".csv")
  iso8859_2 <- withr::local_tempfile(fileext = ".csv")
  # windows1252 <- withr::local_tempfile(fileext = ".csv")
  # cp850 <- withr::local_tempfile(fileext = ".csv")
  # cp1252 <- withr::local_tempfile(fileext = ".csv")

  write.csv(data.frame(a = 1, b = 2), file = ascii)
  write.csv(data.frame(a = "\u00b5"), file = utf8, fileEncoding = "UTF-8")
  write.csv(data.frame(a = "\u00b5"), file = iso8859_1, fileEncoding = "ISO-8859-1")
  # write.csv(data.frame(a = "\u00b5"), file = iso8859_2, fileEncoding = "ISO-8859-2")
  # write.csv(data.frame(a = "\u00b5"), file = windows1252, fileEncoding = "windows1252")
  # write.csv(data.frame(a = "\x81"), file = cp850, fileEncoding = "cp850")
  # write.csv(data.frame(a = "\xf8"), file = cp1252, fileEncoding = "cp1252")

  expect_identical(guess_file_encoding(ascii), "ascii")
  expect_identical(guess_file_encoding(utf8), "UTF-8")
  expect_identical(guess_file_encoding(iso8859_1), "ISO-8859-1")
  # expect_identical(guess_file_encoding(iso8859_2), "ISO-8859-1")
  # expect_identical(guess_file_encoding(windows1252), "windows1252")
  # expect_identical(guess_file_encoding(cp850), "cp850")
  # expect_identical(guess_file_encoding(cp1252), "cp1252")
})
