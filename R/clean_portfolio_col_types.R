clean_portfolio_col_types <- function(portfolio, grouping_variables) {

  # portfolio[,grouping_variables] <- lapply(portfolio[,grouping_variables], clean_punctuation)

  if (is.character(portfolio$investor_name) == FALSE) {
    write_log(msg = paste0(
      "Wrong variable class for investor_name. Should be character, but is ",
      class(portfolio$investor_name),
      ". This can introduce errors in further calculations!"
    ),
    file_path = log_path)
  }
  if (is.character(portfolio$portfolio_name) == FALSE) {
    write_log(msg = paste0(
      "Wrong variable class for portfolio_name Should be character, but is ",
      class(portfolio$portfolio_name),
      ". This can introduce errors in further calculations!"
    ),
    file_path = log_path)
  }
  if (is.numeric(portfolio$market_value) == FALSE) {
    write_log(msg = paste0(
      "Wrong variable class for market_value Should be numeric, but is ",
      class(portfolio$market_value),
      ". This can introduce errors in further calculations!"
    ),
    file_path = log_path)
  }
  if("number_of_shares" %in% colnames(portfolio)){
    if (is.numeric(portfolio$number_of_shares) == FALSE) {
      write_log(msg = paste0(
        "Wrong variable class for number_of_shares Should be numeric, but is ",
        class(portfolio$number_of_shares),
        ". This can introduce errors in further calculations!"
      ),
      file_path = log_path)
    }
    portfolio$number_of_shares <- suppressWarnings(as.numeric(portfolio$number_of_shares))

  }
  if (is.character(portfolio$currency) == FALSE) {
    write_log(msg = paste0(
      "Wrong variable class for currency Should be character, but is ",
      class(portfolio$currency),
      ". This can introduce errors in further calculations!"
    ),
    file_path = log_path)
  }
  if (is.character(portfolio$isin) == FALSE) {
    write_log(msg = paste0(
      "Wrong variable class for isin Should be character, but is ",
      class(portfolio$isin),
      ". This can introduce errors in further calculations!"
    ),
    file_path = log_path)
  }

  portfolio$market_value <- as.numeric(portfolio$market_value)
  portfolio$currency <- as.character(portfolio$currency)

  portfolio$currency <- if_else(portfolio$currency == "Euro", "EUR", portfolio$currency)

  portfolio
}
