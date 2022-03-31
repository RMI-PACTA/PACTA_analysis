portfolio_min <-
  tibble::tibble(
    investor_name = "ABC Investor",
    portfolio_name = "ABC Portfolio 1",
    isin = "XS1088274672",
    market_value = 1000.34,
    currency = "USD"
  )

test_that("reads a proper portfolio CSV correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(portfolio_min, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with old column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <-
    c("Investor.Name",
      "Portfolio.Name",
      "ISIN",
      "Market.Value",
      "Currency")

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with upper case column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- toupper(names(portfolio_alt))

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with a space as the separator in column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- gsub("_", " ", names(portfolio_alt))

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with a dot as the separator in column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- gsub("_", ".", names(portfolio_alt))

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with no separator in column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- gsub("_", "", names(portfolio_alt))

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with leading and trailing white space in column names", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  names(portfolio_alt) <- paste0(" ", names(portfolio_alt), " ")

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with extra columns correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$X <- "XXX"

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with a different column order correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt <- portfolio_alt[, 5:1]

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV in a different file encoding correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_min <-
    tibble::tibble(
      investor_name = "\u00dcber Investor",
      portfolio_name = "\u00dcber Portfolio 1",
      isin = "XS1088274672",
      market_value = 1,
      currency = "USD"
    )

  write.csv(portfolio_min, file = csv_file, fileEncoding = "ISO-8859-1")

  result <- read_portfolio_csv(csv_file)
  expect_equal(guess_file_encoding(csv_file), "ISO-8859-1")
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with a grouping marker correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$market_value <- "1,000.34"

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with an '.' grouping marker correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$market_value <- "1.000,34"

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})

test_that("reads a portfolio CSV with an ' ' grouping marker correctly", {
  csv_file <- withr::local_tempfile(fileext = ".csv")

  portfolio_alt <- portfolio_min
  portfolio_alt$market_value <- "1 000,34"

  readr::write_csv(portfolio_alt, file = csv_file)

  result <- read_portfolio_csv(csv_file)
  expect_equal(unlist(result), unlist(portfolio_min))
})
