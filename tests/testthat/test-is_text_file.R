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
  expect_error(is_text_file(1L))
  expect_error(is_text_file(1L:2L))
  expect_error(is_text_file(TRUE))
  expect_error(is_text_file(c(TRUE, FALSE)))
  expect_error(is_text_file(NA))
  expect_error(is_text_file(c(NA, NA)))
  expect_error(is_text_file(data.frame(a = 1, b = 2)))
  expect_error(is_text_file(data.frame(a = 1:2, b = 2:3)))
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
})
