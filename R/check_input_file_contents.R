check_input_file_contents <- function(portfolio_, portfolio_name, investor_name) {
  portfolio_clean <- clean_colnames_portfolio_input_file(portfolio_)

  necessary_columns <- c(grouping_variables, "market_value", "currency", "isin")

  # if portfolio_name and investor_name are not among the columns of the input file, they are created using the values from the parameter file
  if (!"portfolio_name" %in% colnames(portfolio_clean)) {
    portfolio_clean$portfolio_name <- portfolio_name
  }
  if (!"investor_name" %in% colnames(portfolio_clean)) {
    portfolio_clean$investor_name <- investor_name
  }

  if (length(setdiff(necessary_columns, colnames(portfolio_clean))) > 0) {
    missing_cols <- setdiff(necessary_columns, colnames(portfolio_clean))

    write_log(
      msg = paste(
        "The uploaded portfolio file contains the following missing variables:", str_c(missing_cols, collapse = ", "),
        "\n For correct analysis, please ensure the following required variables are included in your uploaded portfolio file:",
        str_c(necessary_columns, collapse = ", ")
      ),
      file_path = log_path
    )
    stop(paste0("Missing inputs for this portfolio: ", str_c(missing_cols, collapse = ", ")))
  }

  return(portfolio_clean)
}
