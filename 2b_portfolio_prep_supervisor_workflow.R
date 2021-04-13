# use this script instead of script 2, in case the logical run_remotely == TRUE

financial_data <- readRDS(file.path(analysis_inputs_path, "financial_data.rda"))
raw_pf <- read_csv(file.path(raw_input_path, paste0(project_name, "_Input.csv")),
                   col_types = "cccnnccn")


prep_pf <- raw_pf %>%
  inner_join(financial_data, by = "isin") %>%
  add_holding_id() %>%
  rename(current_shares_outstanding_all_classes = current_shares_outstanding)

port_raw_all_eq <- prep_pf %>%
  filter(asset_type == "Equity") %>%
  mutate(
    id = company_id,
    id_name = "company_id"
  )

port_raw_all_eq %>% write_rds(file.path(proc_input_path, paste0(project_name, "_equity_portfolio.rda")))


port_raw_all_cb <- prep_pf %>%
  filter(asset_type == "Bonds") %>%
  mutate(
    id = corporate_bond_ticker,
    id_name = "corporate_bond_ticker"
  )

port_raw_all_cb %>% write_rds(file.path(proc_input_path, paste0(project_name, "_bonds_portfolio.rda")))
