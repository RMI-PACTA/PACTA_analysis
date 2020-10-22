read_rds_or_rda <-
  function(file_path) {
    tryCatch(
      readRDS(file_path),
      error = function(e) read_rdata_as_rda(file_path)
    )
  }


read_rdata_as_rda <-
  function(file_path) {
    tmp_env <- new.env()
    obj_list <- load(file_path, envir = tmp_env)
    stopifnot(length(obj_list) == 1)
    tmp_env[[obj_list]]
  }


get_currency_data_for_timestamp <-
  function(currency_data_path = "data/currencies.rda",
           financial_timestamp = c(parent.frame()$financial_timestamp, "2019Q4")) {

    suppressPackageStartupMessages(require(dplyr))
    stopifnot(file.exists(currency_data_path))

    read_rds_or_rda(currency_data_path) %>%
      select(currency = Currency_abbr,
             exchange_rate = paste0("ExchangeRate_", financial_timestamp)) %>%
      filter(!is.na(currency) & currency != "") %>%
      distinct() %>%
      mutate(exchange_rate = as.numeric(exchange_rate))
}
