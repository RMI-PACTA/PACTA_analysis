# Avoid conflict with renv::load()
load <- base::load

test_that("can read an .RData file saved with extention .rda", {
  rdata <- withr::local_tempfile(fileext = ".rda")

  dataset <- data.frame(x = 1)
  save(dataset, file = rdata)

  expect_equal(read_rda(rdata), dataset)
})

test_that("can read an .rds file saved with extention .rda", {
  rds <- withr::local_tempfile(fileext = ".rda")

  dataset <- data.frame(x = 1)
  saveRDS(dataset, rds)

  expect_equal(read_rda(rds), dataset)
})

test_that("with .RData with more than one object, errors gracefully", {
  rdata <- withr::local_tempfile(fileext = ".rda")

  dataset1 <- data.frame(x = 1)
  dataset2 <- data.frame(y = 2)
  save(list = c("dataset1", "dataset2"), file = rdata)

  expect_error(read_rda(rdata), "must have a single object")
})

test_that("can read an .rds file", {
  rds <- withr::local_tempfile(fileext = ".rds")

  dataset <- data.frame(x = 1)
  saveRDS(dataset, file = rds)

  expect_equal(read_rda(rds), dataset)
})
