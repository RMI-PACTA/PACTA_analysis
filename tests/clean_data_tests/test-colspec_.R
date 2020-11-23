all_colspec_names <-
  c(
    "average_sector_intensity_data",
    "bics_bridge_data",
    "company_emissions_data",
    "consolidated_financial_data",
    "currency_data",
    "debt_financial_data",
    "fin_sector_overrides_data",
    "fund_data",
    "non_distinct_isins_data",
    "revenue_data",
    "sector_bridge",
    "security_financial_data"
  )


for (colspec_name in all_colspec_names) {
  function_name <- paste0("colspec_",  colspec_name)
  result <- do.call(function_name, args = list())

  basic_vector_classes_and_Date <- c(
    "character",
    "complex",
    "double",
    "expression",
    "integer",
    "list",
    "logical",
    "numeric",
    "single",
    "raw",
    "Date"
  )

  test_that(paste0(function_name, "() function returns a character vector"), {
    expect_true(
      inherits(result, "character")
    )
  })

  test_that(paste0(function_name, "() function returns a named vector"), {
    expect_true(
      !is.null(attr(result, "names"))
    )
  })

  test_that(paste0(function_name, "() function returns a vector that contains only values within the list of basic vector classes or 'Date'"), {
    expect_true(
      all(result %in% basic_vector_classes_and_Date)
    )
  })
}


for (colspec_name in all_colspec_names) {
  test_that(paste0("colspec_by_name() function can use the colspec_", colspec_name, "() functions by name and get the same result"), {
    result <- colspec_by_name(colspec_name)
    function_name <- paste0("colspec_",  colspec_name)
    check <- do.call(function_name, args = list())

    expect_true(
      identical(result, check)
    )
  })
}
