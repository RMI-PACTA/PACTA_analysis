test_that("outputs logical vector", {
  directory <- file.path(tempdir(), "directory")
  dir.create(directory)

  empty_file <- tempfile()
  invisible(file.create(empty_file))

  no_read_access <- tempfile()
  saveRDS("XXX", no_read_access)
  Sys.chmod(no_read_access, mode = "222")

  nonbinary_file <- tempfile()
  writeLines("XXX", nonbinary_file)

  accessible_file <- tempfile()
  saveRDS("XXX", accessible_file)

  files <- c(directory, empty_file, no_read_access, nonbinary_file, accessible_file)

  expect_vector(is_binary_file(files[1]), ptype = logical(), size = 1)
  expect_vector(is_binary_file(files[2]), ptype = logical(), size = 1)
  expect_vector(is_binary_file(files[3]), ptype = logical(), size = 1)
  expect_vector(is_binary_file(files[4]), ptype = logical(), size = 1)
  expect_vector(is_binary_file(files[5]), ptype = logical(), size = 1)
  expect_vector(is_binary_file(files), ptype = logical(), size = 5)

  files_df <- data.frame(file = files)
  expect_vector(dplyr::mutate(files_df, is_binary = is_binary_file(file))$is_binary, ptype = logical(), size = 5)
  expect_vector(is_binary_file(files_df$file), ptype = logical(), size = 5)
  expect_vector(is_binary_file(files_df["file"]), ptype = logical(), size = 5)
  expect_vector(is_binary_file(files_df[1]), ptype = logical(), size = 5)
  expect_vector(is_binary_file(files_df[["file"]]), ptype = logical(), size = 5)
  expect_vector(is_binary_file(files_df[[1]]), ptype = logical(), size = 5)

  expect_vector(is_binary_file(1:2), ptype = logical(), size = 2)
  expect_vector(is_binary_file(c(TRUE, FALSE)), ptype = logical(), size = 2)
  expect_vector(is_binary_file(NA), ptype = logical(), size = 1)
  expect_vector(is_binary_file(c(NA, NA)), ptype = logical(), size = 2)
})
