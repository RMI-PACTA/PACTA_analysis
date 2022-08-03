export_audit_textvar_json <- function(portfolio_total_, export_path_full) {
  total <- round(sum(portfolio_total_$value_usd, na.rm = TRUE))

  included <-
    portfolio_total_ %>%
    dplyr::filter(has_valid_input == TRUE) %>%
    dplyr::pull(value_usd) %>%
    sum(na.rm = TRUE) %>%
    round()

  bonds <-
    portfolio_total_ %>%
    dplyr::filter(asset_type == "Bonds") %>%
    dplyr::pull(value_usd) %>%
    sum(na.rm = TRUE) %>%
    round()

  equity <-
    portfolio_total_ %>%
    dplyr::filter(asset_type == "Equity") %>%
    dplyr::pull(value_usd) %>%
    sum(na.rm = TRUE) %>%
    round()

  output <- list("total" = total, "included" = included, "bonds" = bonds, "equity" = equity)

  json_output <- jsonlite::toJSON(output, na = "null", auto_unbox = TRUE)

  write(json_output, file = export_path_full)
}
