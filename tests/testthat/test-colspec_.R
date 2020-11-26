for (data_object_name in data_object_names) {
  function_name <- paste0("colspec_",  data_object_name)
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

  test_that(paste0(function_name, "() function exists for ", data_object_name), {
    expect_true(
      class(base::get(function_name)) == "function"
    )
  })

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


for (data_object_name in data_object_names) {
  test_that(paste0("colspec_by_name() function can use the colspec_", data_object_name, "() function by name and get the same result"), {
    result <- colspec_by_name(data_object_name)
    function_name <- paste0("colspec_",  data_object_name)
    check <- do.call(function_name, args = list())

    expect_true(
      identical(result, check)
    )
  })
}
