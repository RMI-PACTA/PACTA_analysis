# Convert to JSON functions

to_jsonp <-
  function(x, obj_name, pretty = FALSE, auto_unbox = TRUE, na = "null", digits = NA, ...) {
    json <- jsonlite::toJSON(x,
      pretty = pretty, auto_unbox = auto_unbox,
      na = na, digits = digits, ...
    )
    paste0("var ", obj_name, " = ", json, ";")
  }


export_report_content_variables_json <- function(audit_file__ = audit_file,
                                                 results_port_eq__ = port_all_eq,
                                                 results_port_cb__ = port_all_cb,
                                                 results_company_eq__ = company_all_eq,
                                                 results_company_cb__ = company_all_cb,

                                                 investor_name_ = "Meta Investor",
                                                 portfolio_name_ = "Meta Portfolio",


                                                 scenario_ = "SDS",
                                                 scenario_geography_ = "Global",
                                                 allocation_ = "portfolio_weight",
                                                 export_path_full = paste0(outputs_path, "/", investor_name_, "_", portfolio_name_, "_", allocation_, "_gitbook_variables.json")) {
  this_investor_name <- investor_name_
  this_portfolio_name <- portfolio_name_


  # I wouldn't redefine the start year. We need a flag rather if it is not equal to what we have.
  start_year <- min(min(unique(results_port_cb__$year)), min(unique(results_port_eq__$year)),
    min(unique(results_company_eq__$year)), min(unique(results_company_cb__$year)),
    na.rm = T
  )

  assertthat::are_equal(this_investor_name %in% audit_file__$investor_name, T)

  audit_file__ <- audit_file__ %>%
    filter(
      investor_name == this_investor_name,
      portfolio_name == this_portfolio_name
    )

  results_port_eq__ <- results_port_eq__ %>%
    filter(
      investor_name == this_investor_name,
      portfolio_name == this_portfolio_name,
      year == start_year,
      scenario == scenario_,
      scenario_geography == scenario_geography_,
      allocation == allocation_
    )

  results_port_cb__ <- results_port_cb__ %>%
    filter(
      investor_name == this_investor_name,
      portfolio_name == this_portfolio_name,
      year == start_year,
      scenario == scenario_,
      scenario_geography == scenario_geography_,
      allocation == allocation_
    )


  results_company_eq__ <- results_company_eq__ %>%
    filter(
      investor_name == this_investor_name,
      portfolio_name == this_portfolio_name,
      year == start_year,
      scenario == scenario_,
      scenario_geography == scenario_geography_,
      allocation == allocation_
    )


  results_company_cb__ <- results_company_cb__ %>%
    filter(
      investor_name == this_investor_name,
      portfolio_name == this_portfolio_name,
      year == start_year,
      scenario == scenario_,
      scenario_geography == scenario_geography_,
      allocation == allocation_
    )

  technologies <- results_port_eq__$technology
  green_brown_ <- c()

  for (i in 1:length(technologies)) {
    green_brown_[i] <- (green_brown(technologies[i]))
  }
  results_port_eq__["green_brown_"] <- green_brown_

  technologies <- results_port_cb__$technology
  green_brown_ <- c()

  for (i in 1:length(technologies)) {
    green_brown_[i] <- (green_brown(technologies[i]))
  }
  results_port_cb__["green_brown_"] <- green_brown_



  unique_isins <-
    audit_file__ %>%
    filter(
      investor_name == this_investor_name,
      portfolio_name == this_portfolio_name
    ) %>%
    pull(isin) %>%
    unique() %>%
    length()


  total_portfolio_value_usd <- audit_file__ %>%
    filter(direct_holding == T) %>%
    pull(value_usd) %>%
    sum(na.rm = T)


  total_portfolio_percentage_equity <- (audit_file__ %>%
    filter(asset_type == "Equity") %>% pull(value_usd) %>% sum(na.rm = T)) / total_portfolio_value_usd


  total_portfolio_percentage_bonds <- (audit_file__ %>%
    filter(asset_type == "Bonds") %>% pull(value_usd) %>% sum(na.rm = T)) / total_portfolio_value_usd


  total_portfolio_percentage_other_asset_classes <- (audit_file__ %>%
    filter(asset_type == "Others") %>% pull(value_usd) %>% sum(na.rm = T)) / total_portfolio_value_usd



  total_portfolio_percentage_coverage <- (total_portfolio_percentage_equity + total_portfolio_percentage_bonds)





  total_portfolio_percentage_climate_rel_emission <- "XXX_1"


  results_absolute_value_equity <- audit_file__ %>%
    filter(asset_type == "Equity", valid_input == T) %>%
    pull(value_usd) %>%
    sum(na.rm = T)


  results_absolute_value_bonds <- audit_file__ %>%
    filter(asset_type == "Bonds", valid_input == T) %>%
    pull(value_usd) %>%
    sum(na.rm = T)

  results_absolute_value_valid_input <- results_absolute_value_equity + results_absolute_value_bonds

  results_percentage_climate_rel_value_bonds <- results_port_cb__ %>%
    filter(ald_sector != "Other") %>%
    pull("plan_carsten") %>%
    sum(na.rm = T)


  results_percentage_climate_rel_value_equity <- results_port_eq__ %>%
    filter(ald_sector != "Other") %>%
    pull("plan_carsten") %>%
    sum(na.rm = T)


  results_percentage_climate_rel_value <- ((results_percentage_climate_rel_value_bonds * results_absolute_value_bonds) +
    (results_percentage_climate_rel_value_equity * results_absolute_value_equity)) / results_absolute_value_valid_input


  results_percentage_lowcarb_value_equity <- results_port_eq__ %>%
    filter(green_brown_ == "green") %>%
    pull("plan_carsten") %>%
    sum(na.rm = T)

  results_percentage_lowcarb_value_bonds <- results_port_cb__ %>%
    filter(green_brown_ == "green") %>%
    pull("plan_carsten") %>%
    sum(na.rm = T)

  results_percentage_highcarb_value_equity <- results_port_eq__ %>%
    filter(green_brown_ == "brown") %>%
    pull("plan_carsten") %>%
    sum(na.rm = T)

  results_percentage_highcarb_value_bonds <- results_port_cb__ %>%
    filter(green_brown_ == "brown") %>%
    pull("plan_carsten") %>%
    sum(na.rm = T)

  results_absolute_lowcarb_value_equity <- results_percentage_lowcarb_value_equity * results_absolute_value_equity
  results_absolute_highcarb_value_equity <- results_percentage_highcarb_value_equity * results_absolute_value_equity

  results_absolute_lowcarb_value_bonds <- results_percentage_lowcarb_value_bonds * results_absolute_value_bonds
  results_absolute_highcarb_value_bonds <- results_percentage_highcarb_value_bonds * results_absolute_value_bonds

  results_percentage_highcarb_value <- (results_absolute_highcarb_value_bonds + results_absolute_highcarb_value_equity) / (results_absolute_value_bonds + results_absolute_value_equity)
  results_percentage_lowcarb_value <- (results_absolute_lowcarb_value_bonds + results_absolute_lowcarb_value_equity) / (results_absolute_value_bonds + results_absolute_value_equity)

  results_company_nr_relevent_companies <- "5"



  parameters_list <-
    list(
      InvestorName = this_investor_name,
      PortfolioName = this_portfolio_name,
      unique_isins = unique_isins,

      total_portfolio_value_usd = total_portfolio_value_usd,
      total_portfolio_percentage_equity = total_portfolio_percentage_equity,
      total_portfolio_percentage_bonds = total_portfolio_percentage_bonds,
      total_portfolio_percentage_other_asset_classes = total_portfolio_percentage_other_asset_classes,
      total_portfolio_percentage_coverage = total_portfolio_percentage_coverage,

      results_percentage_climate_rel_value = results_percentage_climate_rel_value,

      total_portfolio_percentage_climate_rel_emission = total_portfolio_percentage_climate_rel_emission,

      results_absolute_value_equity = results_absolute_value_equity,
      results_absolute_value_bonds = results_absolute_value_bonds,

      results_percentage_climate_rel_value_bonds = results_percentage_climate_rel_value_bonds,
      results_percentage_climate_rel_value_equity = results_percentage_climate_rel_value_equity,

      results_absolute_highcarb_value_bonds = results_absolute_highcarb_value_bonds,
      results_absolute_highcarb_value_equity = results_absolute_highcarb_value_equity,

      results_percentage_highcarb_value_bonds = results_percentage_highcarb_value_bonds,
      results_percentage_highcarb_value_equity = results_percentage_highcarb_value_equity,

      results_percentage_lowcarb_value_bonds = results_percentage_lowcarb_value_bonds,
      results_percentage_lowcarb_value_equity = results_percentage_lowcarb_value_equity,

      results_percentage_lowcarb_value = results_percentage_lowcarb_value,
      results_percentage_highcarb_value = results_percentage_highcarb_value,

      results_company_nr_relevent_companies = results_company_nr_relevent_companies
    )


  parameters_list %>%
    to_jsonp("data_parameters") %>%
    writeLines(path(export_path_full))
}


export_audit_information_jsons <- function(audit_file_ = audit_file,
                                           portfolio_total_ = portfolio_total,
                                           folder_path = proc_input_path,
                                           project_name_ = NA) {

  # Check format
  assertthat::is.string(folder_path)
  assertthat::are_equal(is.data.frame(audit_file_), TRUE)

  if ("Meta Investor" %in% audit_file_$investor_name) {
    audit_file_ <- subset(audit_file_, investor_name != "Meta Investor")
  }
  if ("Meta Investor" %in% portfolio_total_$investor_name) {
    portfolio_total_ <- subset(portfolio_total_, investor_name != "Meta Investor")
  }





  folder_path <- paste0(folder_path, "/")
  if (!is.na(project_name_)) {
    folder_path <- paste0(folder_path, project_name_, "_")
  }
  # function
  export_audit_graph_json(audit_file_, paste0(folder_path, "coveragegraph"))
  export_audit_invalid_json(portfolio_total_, paste0(folder_path, "invalidsecurities"))
  export_audit_textvar_json(portfolio_total_, paste0(folder_path, "coveragetextvar"))
}




export_audit_graph_json <- function(audit_file__, export_path_full) {
  audit_file__ <- audit_file__ %>% select("isin", "holding_id", "flag")

  all_flags <- c("Missing currency information", "Negative or missing input value", "Invalid or missing ISIN", "Holding not in Bloomberg database", "Included in analysis")
  flags_in_auditfile <- unique(audit_file__$flag)


  indicator <- TRUE
  for (i in 1:length(flags_in_auditfile)) {
    indicator <- indicator & (flags_in_auditfile[i] %in% all_flags)
  }
  assertthat::are_equal(indicator, TRUE)


  number_of_isin <- length(audit_file__$holding_id)

  missing_currency <- audit_file__ %>% subset(flag == "Missing currency information")
  number_missing_currency <- length(missing_currency$holding_id)

  negative_missing_input <- audit_file__ %>% subset(flag == "Negative or missing input value")
  number_negative_missing_input <- length(negative_missing_input$holding_id)

  invalid_input <- audit_file__ %>% subset(flag == "Invalid or missing ISIN")
  number_invalid_input <- length(invalid_input$holding_id)

  not_in_bloomberg <- audit_file__ %>% subset(flag == "Holding not in Bloomberg database")
  number_not_in_bloomberg <- length(not_in_bloomberg$holding_id)

  included_in_analysis <- audit_file__ %>% subset(flag == "Included in analysis")
  number_included_in_analysis <- length(included_in_analysis$holding_id)

  all_flags_numbers <- c(number_missing_currency, number_negative_missing_input, number_invalid_input, number_not_in_bloomberg, number_included_in_analysis)
  assertthat::are_equal(number_of_isin, sum(all_flags_numbers))

  number_all_invalid_input <- sum(c(number_missing_currency, number_negative_missing_input, number_invalid_input))

  legend <- c("\"Invalid input\"", "\"No data coverage\"", "\"Included in analysis\"")
  keys <- c("\"key_1\"", "\"key_2\"", "\"key_3\"")
  values <- c(number_all_invalid_input, number_not_in_bloomberg, number_included_in_analysis)

  assertthat::are_equal(length(legend), length(keys))
  assertthat::are_equal(length(values), length(keys))

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

export_audit_invalid_json <- function(portfolio_total_, export_path_full) {
  portfolio_total_ <- portfolio_total_ %>% subset(flag %in% c("Missing currency information", "Negative or missing input value", "Invalid or missing ISIN"))
  portfolio_total_ <- portfolio_total_ %>% select("isin", "market_value", "currency", "flag")
  portfolio_total_ <- portfolio_total_[order(-portfolio_total_$market_value), ]

  colnames(portfolio_total_) <- c("isin", "marketValues", "currency", "flag")
  invalidsecurties <- toJSON(portfolio_total_, dataframe = c("columns"))

  write(invalidsecurties, file = paste0(export_path_full, ".json"))
}

export_audit_textvar_json <- function(portfolio_total_, export_path_full) {
  portfolio_total_ <- portfolio_total_ %>% select("value_usd", "asset_type", "has_valid_input")
  total_v <- sum(portfolio_total_$value_usd)
  included <- portfolio_total_ %>% subset(has_valid_input == TRUE)
  included_v <- sum(included$value_usd)
  bonds <- portfolio_total_ %>% subset(asset_type == "Bonds")
  bonds_v <- sum(bonds$value_usd)
  equity <- portfolio_total_ %>% subset(asset_type == "Equity")
  equity_v <- sum(equity$value_usd)

  assertthat::are_equal(included_v <= total_v, TRUE)
  assertthat::are_equal(bonds_v + equity_v - included_v < 1, TRUE)

  total_v <- round(total_v)
  total_v <- as.character(total_v)
  included_v <- round(included_v)
  bonds_v <- round(bonds_v)
  equity_v <- round(equity_v)

  keys <- c("\"total\"", "\"included\"", "\"bonds\"", "\"equity\"")
  values <- c(total_v, included_v, bonds_v, equity_v)


  json_head <- "{"
  json_tail <- "}"

  text_varibles <- json_head

  for (i in 1:(length(keys) - 1)) {
    text_varibles <- paste0(text_varibles, " ", keys[i], ": ", values[i], ",")
  }
  i <- length(keys)

  text_varibles <- paste0(text_varibles, " ", keys[i], ": ", values[i])

  text_varibles <- paste0(text_varibles, json_tail)

  write(text_varibles, file = paste0(export_path_full, ".json"))
}
