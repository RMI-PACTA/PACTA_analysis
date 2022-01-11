test_that("always outputs a logical vector", {
  # possible filepaths
  non_existant_file <- "xxx"

  directory <- withr::local_tempdir()

  empty_file <- withr::local_tempfile()
  invisible(file.create(empty_file))

  accessible_file <- withr::local_tempfile()
  writeLines("XXX", accessible_file)

  files <- c(non_existant_file, directory, empty_file, accessible_file)

  expect_vector(is_file_accessible(files[1L]), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(files[2L]), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(files[3L]), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(files[4L]), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(files), ptype = logical(), size = 4L)

  # expected possible uses
  files_df <- data.frame(file = files)

  out <- dplyr::mutate(files_df, accessible = is_file_accessible(file))$accessible
  expect_vector(out, ptype = logical(), size = 4L)

  out <- is_file_accessible(files_df$file)
  expect_vector(out, ptype = logical(), size = 4L)

  out <- is_file_accessible(files_df["file"])
  expect_vector(out, ptype = logical(), size = 4L)

  out <- is_file_accessible(files_df[1L])
  expect_vector(out, ptype = logical(), size = 4L)

  out <- is_file_accessible(files_df[["file"]])
  expect_vector(out, ptype = logical(), size = 4L)

  out <- is_file_accessible(files_df[[1L]])
  expect_vector(out, ptype = logical(), size = 4L)

  # unexpected input types
  expect_vector(is_file_accessible(1L), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(1L:2L), ptype = logical(), size = 2L)
  expect_vector(is_file_accessible(TRUE), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(FALSE), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(c(TRUE, FALSE)), ptype = logical(), size = 2L)
  expect_vector(is_file_accessible(NA), ptype = logical(), size = 1L)
  expect_vector(is_file_accessible(c(NA, NA)), ptype = logical(), size = 2L)
})

test_that("returns expected values", {
  # typical usage
  non_existant_file <- "xxx"

  directory <- withr::local_tempdir()

  empty_file <- withr::local_tempfile()
  invisible(file.create(empty_file))

  accessible_file <- withr::local_tempfile()
  writeLines("XXX", accessible_file)

  files <- c(non_existant_file, directory, empty_file, accessible_file)

  expect_identical(is_file_accessible(files), c(FALSE, FALSE, FALSE, TRUE))

  # unexpected input types
  expect_identical(is_file_accessible(NA), FALSE)
  expect_identical(is_file_accessible(NA_character_), FALSE)
  expect_identical(is_file_accessible(c(TRUE, FALSE)), c(FALSE, FALSE))
  expect_identical(is_file_accessible(1L:2L), c(FALSE, FALSE))
})

test_that("works as expected on a file that does not have read permission", {
  skip_on_os(c("windows", "solaris"))

  no_read_access <- withr::local_tempfile()
  writeLines("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  expect_vector(is_file_accessible(no_read_access), ptype = logical(), size = 1L)
  expect_identical(is_file_accessible(no_read_access), FALSE)
})
