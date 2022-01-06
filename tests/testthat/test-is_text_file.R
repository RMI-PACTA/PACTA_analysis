test_that("always outputs a logical vector", {
  # possible filepaths
  non_existant_file <- "xxx"

  directory <- withr::local_tempdir()

  empty_file <- withr::local_tempfile()
  invisible(file.create(empty_file))

  no_read_access <- withr::local_tempfile()
  saveRDS("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  binary_file <- withr::local_tempfile()
  saveRDS("XXX", binary_file)

  text_file <- withr::local_tempfile()
  writeLines("XXX", text_file)

  files <- c(non_existant_file, directory, empty_file, no_read_access, binary_file, text_file)

  expect_vector(is_text_file(files[1L]), ptype = logical(), size = 1L)
  expect_vector(is_text_file(files[2L]), ptype = logical(), size = 1L)
  expect_vector(is_text_file(files[3L]), ptype = logical(), size = 1L)
  expect_vector(is_text_file(files[4L]), ptype = logical(), size = 1L)
  expect_vector(is_text_file(files[5L]), ptype = logical(), size = 1L)
  expect_vector(is_text_file(files[6L]), ptype = logical(), size = 1L)
  expect_vector(is_text_file(files), ptype = logical(), size = 6L)


  # expected possible uses
  files_df <- data.frame(file = files)

  out <- dplyr::mutate(files_df, is_text = is_text_file(file))
  expect_vector(out$is_text, ptype = logical(), size = 6L)

  out <- is_text_file(files_df$file)
  expect_vector(out, ptype = logical(), size = 6L)

  out <- is_text_file(files_df["file"])
  expect_vector(out, ptype = logical(), size = 6L)

  out <- is_text_file(files_df[1L])
  expect_vector(out, ptype = logical(), size = 6L)

  out <- is_text_file(files_df[["file"]])
  expect_vector(out, ptype = logical(), size = 6L)

  out <- is_text_file(files_df[[1L]])
  expect_vector(out, ptype = logical(), size = 6L)


  # unexpected input types
  expect_vector(is_text_file(1L), ptype = logical(), size = 1L)
  expect_vector(is_text_file(1L:2L), ptype = logical(), size = 2L)
  expect_vector(is_text_file(TRUE), ptype = logical(), size = 1L)
  expect_vector(is_text_file(c(TRUE, FALSE)), ptype = logical(), size = 2L)
  expect_vector(is_text_file(NA), ptype = logical(), size = 1L)
  expect_vector(is_text_file(c(NA, NA)), ptype = logical(), size = 2L)
})


test_that("uses `wand::get_content_type()` if system's `file` is not available", {
  withr::local_path(action = "replace")
  expect_true(Sys.which("file") == "")

  txt_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("XXX", txt_file)

  csv_file <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1, b = 2), file = csv_file)

  tsv_file <- withr::local_tempfile(fileext = ".tsv")
  write.table(data.frame(a = 1, b = 2), sep = "\t", row.names = FALSE,
              quote = FALSE, file = tsv_file)

  tar_file_with_csv_ext <- withr::local_tempfile(fileext = ".csv")
  tar(tar_file_with_csv_ext, csv_file)

  expect_true(is_text_file(txt_file))
  expect_true(is_text_file(csv_file))
  expect_true(is_text_file(tsv_file))
  expect_false(is_text_file(tar_file_with_csv_ext))
})

test_that("returns expected values", {
  skip_if(Sys.which("file") == "")

  # typical usage
  non_existant_file <- "xxx"

  directory <- withr::local_tempdir()

  empty_file <- withr::local_tempfile()
  invisible(file.create(empty_file))

  binary_file <- withr::local_tempfile()
  saveRDS("XXX", binary_file)

  text_file <- withr::local_tempfile()
  writeLines("XXX", text_file)

  files <- c(non_existant_file, directory, empty_file,
             binary_file, text_file, NA_character_)

  expect_identical(is_text_file(files), c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))

  # unexpected input types
  expect_identical(is_text_file(NA), FALSE)
  expect_identical(is_text_file(NA_character_), FALSE)
  expect_identical(is_text_file(c(TRUE, FALSE)), c(FALSE, FALSE))
  expect_identical(is_text_file(1L:2L), c(FALSE, FALSE))
})

test_that("returns expected values when system's `file` is not available", {
  withr::local_path(action = "replace")
  expect_true(Sys.which("file") == "")

  # typical usage
  non_existant_file <- "xxx"

  directory <- withr::local_tempdir()

  empty_file <- withr::local_tempfile()
  invisible(file.create(empty_file))

  no_read_access <- withr::local_tempfile()
  writeLines("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  binary_file <- withr::local_tempfile()
  saveRDS("XXX", binary_file)

  # `wand::get_content_type()` will not be able to identify the next two file as
  # text because text files do not have a magic number and they don't have an
  # extension that wand can infer their content from

  text_file_no_ext <- withr::local_tempfile()
  writeLines("XXX", text_file_no_ext)

  csv_file_no_ext <- withr::local_tempfile()
  write.csv(data.frame(a = 1, b = 2), file = csv_file_no_ext)

  # unfortunately, `wand::get_content_type()` does not recognize the ".R" file
  # extension, so the following file also will return `FALSE`. However, th
  # `is_text_file()` function is primarily intended to identify delimited text
  # files, e.g. CSV or TSV files, so identifying .R files is not necessary.
  R_file_with_ext <- withr::local_tempfile(fileext = ".R")
  writeLines("XXX", R_file_with_ext)

  csv_file_with_ext <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1, b = 2), file = csv_file_with_ext)

  tsv_file_with_ext <- withr::local_tempfile(fileext = ".tsv")
  write.table(data.frame(a = 1, b = 2), sep = "\t", row.names = FALSE,
              quote = FALSE, file = tsv_file_with_ext)

  files <- c(non_existant_file, directory, empty_file, no_read_access,
             binary_file, text_file_no_ext, csv_file_no_ext, R_file_with_ext,
             csv_file_with_ext, tsv_file_with_ext, NA_character_)

  expect_identical(is_text_file(files), c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))

  # unexpected input types
  expect_identical(is_text_file(NA), FALSE)
  expect_identical(is_text_file(NA_character_), FALSE)
  expect_identical(is_text_file(c(TRUE, FALSE)), c(FALSE, FALSE))
  expect_identical(is_text_file(1L:2L), c(FALSE, FALSE))
})
