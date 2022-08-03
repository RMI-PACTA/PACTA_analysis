save_if_exists <- function(df, portfolio_name, save_name, csv_or_rds = "rds") {
  if (data_check(df)) {
    df <- df %>% filter(portfolio_name == .env$portfolio_name)
  }

  if (data_check(df)) {
    if (csv_or_rds == "rds") {
      saveRDS(df, save_name)
    } else if (csv_or_rds == "csv") {
      write_csv_file(df, file = save_name)
    }
  }
}
