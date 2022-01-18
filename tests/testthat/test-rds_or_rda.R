test_that("returns expected output", {
  rds_ascii <- withr::local_tempfile()
  rds_binary <- withr::local_tempfile()
  rds_ascii_gz <- withr::local_tempfile()
  rds_binary_gz <- withr::local_tempfile()

  rda_ascii <- withr::local_tempfile()
  rda_binary <- withr::local_tempfile()
  rda_ascii_gz <- withr::local_tempfile()
  rda_binary_gz <- withr::local_tempfile()

  obj <- "xyz"

  saveRDS(obj, file = rds_ascii, ascii = TRUE, compress = FALSE)
  saveRDS(obj, file = rds_binary, ascii = FALSE, compress = FALSE)
  saveRDS(obj, file = rds_ascii_gz, ascii = TRUE, compress = TRUE)
  saveRDS(obj, file = rds_binary_gz, ascii = FALSE, compress = TRUE)

  save(obj, file = rda_ascii, ascii = TRUE, compress = FALSE)
  save(obj, file = rda_binary, ascii = FALSE, compress = FALSE)
  save(obj, file = rda_ascii_gz, ascii = TRUE, compress = TRUE)
  save(obj, file = rda_binary_gz, ascii = FALSE, compress = TRUE)

  files <- c(rds_ascii, rds_binary, rds_ascii_gz, rds_binary_gz,
             rda_ascii, rda_binary, rda_ascii_gz, rda_binary_gz)

  expect_equal(rds_or_rda(files), c(rep("rds", times = 4), rep("rda", times = 4)))
})
