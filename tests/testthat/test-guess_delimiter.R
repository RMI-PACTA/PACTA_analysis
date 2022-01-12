test_that("outputs character vector", {
  directory <- withr::local_tempdir()

  empty_file <- withr::local_tempfile()
  invisible(file.create(empty_file))

  no_read_access <- withr::local_tempfile()
  saveRDS("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  binary_file <- withr::local_tempfile()
  saveRDS("XXX", binary_file)

  accessible_file <- withr::local_tempfile()
  writeLines("XXX", accessible_file)

  files <- c(directory, empty_file, no_read_access, binary_file, accessible_file)

  expect_vector(guess_delimiter(files[1L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[2L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[3L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[4L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[5L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files), ptype = character(), size = 5L)

  files_df <- data.frame(file = files)
  expect_vector(dplyr::mutate(files_df, encoding = guess_delimiter(file))$encoding, ptype = character(), size = 5L)
  expect_vector(guess_delimiter(files_df$file), ptype = character(), size = 5L)
  expect_vector(guess_delimiter(files_df["file"]), ptype = character(), size = 5L)
  expect_vector(guess_delimiter(files_df[1L]), ptype = character(), size = 5L)
  expect_vector(guess_delimiter(files_df[["file"]]), ptype = character(), size = 5L)
  expect_vector(guess_delimiter(files_df[[1L]]), ptype = character(), size = 5L)
})

test_that("returns error for unexpected types", {
  # unexpected input types
  expect_error(guess_delimiter(1L))
  expect_error(guess_delimiter(1L:2L))
  expect_error(guess_delimiter(TRUE))
  expect_error(guess_delimiter(FALSE))
  expect_error(guess_delimiter(c(TRUE, FALSE)))
  expect_error(guess_delimiter(NA))
  expect_error(guess_delimiter(c(NA, NA)))
  expect_error(guess_delimiter(data.frame(a = 1, b = 2)))
})

test_that("returns expected values", {
  comma <- withr::local_tempfile()
  writeLines("a,b,c\n1,2,3", comma)

  semicolon <- withr::local_tempfile()
  writeLines("a;b;c\n1;2;3", semicolon)

  bar <- withr::local_tempfile()
  writeLines("a|b|c\n1|2|3", bar)

  space <- withr::local_tempfile()
  writeLines("a b c\n1 2 3", space)

  tab <- withr::local_tempfile()
  writeLines("a\tb\tc\n1\t2\t3", tab)

  files <- c(comma, semicolon, bar, space, tab)

  expect_identical(guess_delimiter(comma), ",")
  expect_identical(guess_delimiter(semicolon), ";")
  expect_identical(guess_delimiter(bar), "|")
  expect_identical(guess_delimiter(space), " ")
  expect_identical(guess_delimiter(tab), "\t")
  expect_identical(guess_delimiter(files), c(",", ";", "|", " ", "\t"))
})
