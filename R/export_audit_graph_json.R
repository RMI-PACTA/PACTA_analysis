export_audit_graph_json <- function(audit_file__, export_path_full) {
  audit_file__ <- audit_file__ %>% select("isin", "holding_id", "flag")

  all_flags <- c("Missing currency information", "Negative or missing input value", "Invalid or missing ISIN", "Holding not in financial database", "Included in analysis")
  flags_in_auditfile <- unique(audit_file__$flag)


  indicator <- TRUE
  for (i in 1:length(flags_in_auditfile)) {
    indicator <- indicator & (flags_in_auditfile[i] %in% all_flags)
  }
  if (isFALSE(indicator)) { stop("`indicator` is not `TRUE`") }

  number_of_isin <- length(audit_file__$holding_id)

  missing_currency <- audit_file__ %>% subset(flag == "Missing currency information")
  number_missing_currency <- length(missing_currency$holding_id)

  negative_missing_input <- audit_file__ %>% subset(flag == "Negative or missing input value")
  number_negative_missing_input <- length(negative_missing_input$holding_id)

  invalid_input <- audit_file__ %>% subset(flag == "Invalid or missing ISIN")
  number_invalid_input <- length(invalid_input$holding_id)

  not_in_financial <- audit_file__ %>% subset(flag == "Holding not in financial database")
  number_not_in_financial <- length(not_in_financial$holding_id)

  included_in_analysis <- audit_file__ %>% subset(flag == "Included in analysis")
  number_included_in_analysis <- length(included_in_analysis$holding_id)

  all_flags_numbers <- c(number_missing_currency, number_negative_missing_input, number_invalid_input, number_not_in_financial, number_included_in_analysis)
  if (isFALSE(all.equal(number_of_isin, sum(all_flags_numbers)))) {
    stop("`number_of_isin` and `sum(all_flags_numbers)` are not equal")
  }

  number_all_invalid_input <- sum(c(number_missing_currency, number_negative_missing_input, number_invalid_input))

  legend <- c("\"Invalid input\"", "\"No data coverage\"", "\"Included in analysis\"")
  keys <- c("\"key_1\"", "\"key_2\"", "\"key_3\"")
  values <- c(number_all_invalid_input, number_not_in_financial, number_included_in_analysis)

  if (isFALSE(all.equal(length(legend), length(keys)))) {
    stop("`length(legend)` and `length(keys)` are not equal")
  }
  if (isFALSE(all.equal(length(values), length(keys)))) {
    stop("`length(values)` and `length(keys)` are not equal")
  }

  json_head <- "{"
  json_tail <- "}"

  chart_information <- json_head
  chart_legend <- json_head
  for (i in 1:(length(keys) - 1)) {
    chart_information <- paste0(chart_information, " ", keys[i], ": ", values[i], ",")
    chart_legend <- paste0(chart_legend, " ", keys[i], ": ", legend[i], ",")
  }
  i <- length(keys)
  chart_information <- paste0(chart_information, " ", keys[i], ": ", values[i])
  chart_legend <- paste0(chart_legend, " ", keys[i], ": ", legend[i])

  chart_information <- paste0(chart_information, json_tail)
  chart_legend <- paste0(chart_legend, json_tail)

  write(chart_legend, file = paste0(export_path_full, "legend.json"))
  write(chart_information, file = paste0(export_path_full, ".json"))
}
