get_currency_data <- function() {
  readRDS("data/currencies.rds")
}


get_currency_data_for_timestamp <-
  function(financial_timestamp,
           currency_data = NULL,
           exchange_rate_colname = paste0("ExchangeRate_", financial_timestamp),
           currency_code_colname = "Currency_abbr") {
    if (is.null(currency_data)) {
      currency_data <- get_currency_data()
    }

    stopifnot(inherits(currency_data, "data.frame"))
    stopifnot(currency_code_colname %in% names(currency_data))
    stopifnot(exchange_rate_colname %in% names(currency_data))
    stopifnot(inherits(currency_data[[currency_code_colname]], "character"))
    stopifnot(inherits(currency_data[[exchange_rate_colname]], "numeric"))

    currency_data <-
      currency_data[, c(currency_code_colname, exchange_rate_colname)]
    names(currency_data) <- c("currency", "exchange_rate")
    currency_data <-
      currency_data[!is.na(currency_data$currency) &
                      currency_data$currency != "",]
    currency_data <- currency_data[!duplicated(currency_data),]

    currency_data
  }
