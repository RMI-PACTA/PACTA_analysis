test_that("abort_if_file_doesnt_exist does what its name promises", {
  file <- tempfile()
  expect_error(abort_if_file_doesnt_exist(file), "must exist")

  fs::file_create(file)
  withr::defer(fs::file_delete(file))
  expect_error(abort_if_file_doesnt_exist(file), NA)
})
