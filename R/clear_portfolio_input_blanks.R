clear_portfolio_input_blanks <- function(portfolio) {
  if (any(portfolio[, grouping_variables] == "" | is.na(portfolio[, grouping_variables]))) {
    print("Warning: missing grouping variables, corresponding rows removed")
    write_log(msg = paste(
      "Warning: some entries of the uploaded portfolio file were removed
              because of missing values in at least one of the variables", str_c(grouping_variables, collapse = ", "),
      "\n To ensure complete analysis, please upload a file without
                          missing values in these columns."
    ),
    file_path = log_path)

    portfolio <- portfolio %>% filter_at(
      grouping_variables, all_vars(!is.na(.))
    )
  }

  portfolio
}
