# Avoid conflict with renv::load()
load <- base::load

test_that("can read an .RData file saved with extention .rda", {
  rdata <- withr::local_tempfile(fileext = ".rda")

  dataset <- data.frame(x = 1)
  save(dataset, file = rdata)

  expect_equal(read_file(rdata), dataset)
})

test_that("can read an .rds file saved with extention .rda", {
  rds <- withr::local_tempfile(fileext = ".rda")

  dataset <- data.frame(x = 1)
  saveRDS(dataset, rds)

  expect_equal(read_file(rds), dataset)
})

test_that("can read an .fst file saved with extention .fst", {
  fst <- withr::local_tempfile(fileext = ".fst")

  dataset <- data.frame(x = 1)
  fst::write_fst(dataset, fst)

  expect_equal(read_file(fst), dataset)
})

test_that("can read a .csv file saved with extention .csv", {
  rdata <- withr::local_tempfile(fileext = ".csv")

  dataset <- data.frame(x = 1)
  readr::write_csv(dataset, rdata)

  # Attributes may differ
  expect_equivalent(read_file(rdata), dataset)
})

test_that("with .RData with more than one object, errors gracefully", {
  rdata <- withr::local_tempfile(fileext = ".rda")

  dataset1 <- data.frame(x = 1)
  dataset2 <- data.frame(y = 2)
  save(list = c("dataset1", "dataset2"), file = rdata)

  expect_error(read_file(rdata), "must have a single object")
})

test_that("with unsoported extension in path, errors gracefully", {
  file <- tempfile(fileext = ".bad")
  expect_error(read_file(file, "Unsupported.*extension"))
})

test_that("with inexistent file, throws a warning", {
  # FIXME: A warning seems like a bad idea. It should likely be an error
  expect_warning(read_file("dont.exist"), "not exist")
})
