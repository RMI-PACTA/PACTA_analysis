test_that("returns expected values", {
  comma <- withr::local_tempfile()
  writeLines("a,b,c\n1,2,3", comma)

  semicolon <- withr::local_tempfile()
  writeLines("a;b;c\n1;2;3", semicolon)

  colon <- withr::local_tempfile()
  writeLines("a:b:c\n1:2:3", colon)

  bar <- withr::local_tempfile()
  writeLines("a|b|c\n1|2|3", bar)

  tab <- withr::local_tempfile()
  writeLines("a\tb\tc\n1\t2\t3", tab)

  files <- c(comma, semicolon, colon, bar, tab)

  expect_identical(guess_delimiter(comma), ",")
  expect_identical(guess_delimiter(semicolon), ";")
  expect_identical(guess_delimiter(colon), ":")
  expect_identical(guess_delimiter(bar), "|")
  expect_identical(guess_delimiter(tab), "\t")
  expect_identical(guess_delimiter(files), c(",", ";", ":", "|", "\t"))

  # test expected use cases
  files_df <- data.frame(file = files)
  expected_output <- c(",", ";", ":", "|", "\t")

  out <- dplyr::mutate(files_df, delimiter = guess_delimiter(files))$delimiter
  expect_identical(out, expected_output)
  out <- guess_delimiter(files_df$file)
  expect_identical(out, expected_output)
  out <- guess_delimiter(files_df["file"])
  expect_identical(out, expected_output)
  out <- guess_delimiter(files_df[1L])
  expect_identical(out, expected_output)
  out <- guess_delimiter(files_df[["file"]])
  expect_identical(out, expected_output)
  out <- guess_delimiter(files_df[[1L]])
  expect_identical(out, expected_output)
})

test_that("returns appropriate value for strange cases", {
  lots_of_spaces <- withr::local_tempfile()
  writeLines("a b c d e f g,h,1\na b c d e f g,2,3,4,5,6,7,8", lots_of_spaces)
  expect_identical(guess_delimiter(lots_of_spaces), ",")
})

test_that("returns `NA` for filepaths that cannot be used", {
  directory <- withr::local_tempdir()

  empty_file <- withr::local_tempfile()
  invisible(file.create(empty_file))

  no_read_access <- withr::local_tempfile()
  saveRDS("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  binary_file <- withr::local_tempfile()
  saveRDS("XXX", binary_file)

  files <- c(directory, empty_file, no_read_access, binary_file)

  expect_vector(guess_delimiter(files[1L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[2L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[3L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files[4L]), ptype = character(), size = 1L)
  expect_vector(guess_delimiter(files), ptype = character(), size = 4L)

  expect_identical(guess_delimiter(directory), NA_character_)
  expect_identical(guess_delimiter(empty_file), NA_character_)
  expect_identical(guess_delimiter(no_read_access), NA_character_)
  expect_identical(guess_delimiter(binary_file), NA_character_)
})

test_that("returns error for unexpected types", {
  expect_error(guess_delimiter(1L))
  expect_error(guess_delimiter(1L:2L))
  expect_error(guess_delimiter(TRUE))
  expect_error(guess_delimiter(FALSE))
  expect_error(guess_delimiter(c(TRUE, FALSE)))
  expect_error(guess_delimiter(NA))
  expect_error(guess_delimiter(c(NA, NA)))
  expect_error(guess_delimiter(data.frame(a = 1, b = 2)))
})
