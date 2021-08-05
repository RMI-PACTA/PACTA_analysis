# use this script instead of script 2, in case the logical run_remotely == TRUE

financial_data <- readRDS(file.path(analysis_inputs_path, "financial_data.rda"))
raw_pf <- read_csv(file.path(raw_input_path, paste0(project_name, "_Input.csv")),
                   col_types = "cccnnccn")


prep_pf <- raw_pf %>%
  inner_join(financial_data, by = "isin") %>%
  add_holding_id() %>%
  rename(current_shares_outstanding_all_classes = current_shares_outstanding)

prep_pf <- add_meta_portfolio(prep_pf, inc_meta_portfolio)

# taken from add_portfolio_flags()
prep_pf <- check_isin_format(prep_pf)
prep_pf <- check_valid_value_usd(prep_pf)

# taken from add_flags()
prep_pf <- prep_pf %>%
  mutate(
    flag = case_when(
      !has_valid_value_usd ~ "Negative or missing input value",
      !has_valid_isin ~ "Invalid or missing ISIN",
      TRUE ~ "Included in analysis"
    )
  )

# taken from overall_validity_flag()
portfolio_total <- prep_pf %>%
  mutate(valid_input = case_when(
    !has_valid_value_usd ~ FALSE,
    !has_valid_isin ~ FALSE,
    TRUE ~ TRUE
  ))

portfolio_overview <- portfolio_summary(portfolio_total)

portfolio_overview %>% write_rds(file.path(proc_input_path, paste0(project_name, "_overview_portfolio.rda")))
portfolio_overview %>% write_csv(file.path(proc_input_path, paste0(project_name, "_overview_portfolio.csv")))

port_raw_all_eq <- portfolio_total %>%
  filter(asset_type == "Equity") %>%
  mutate(
    id = company_id,
    id_name = "company_id"
  )

port_raw_all_eq %>% write_rds(file.path(proc_input_path, paste0(project_name, "_equity_portfolio.rda")))


port_raw_all_cb <- portfolio_total %>%
  filter(asset_type == "Bonds") %>%
  mutate(
    id = corporate_bond_ticker,
    id_name = "corporate_bond_ticker"
  )

port_raw_all_cb %>% write_rds(file.path(proc_input_path, paste0(project_name, "_bonds_portfolio.rda")))
