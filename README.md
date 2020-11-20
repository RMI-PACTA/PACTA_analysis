PACTA\_analysis
================

This repository hosts the web tool. It’s scope covers multiple public
and private repositories at <https://github.com/2DegreesInvesting>:

``` r
pacta_find()
#> [1] "PACTA_analysis"            "StressTestingModelDev"    
#> [3] "create_interactive_report" "pacta-data"
```

This repository PACTA\_analysis is the most closely related to “PACTA”.
The main file is “web\_tool\_script\_1.R”; source it to produce inputs
to other, dependent parts of the web-tool ecosystem:

    #> 
    #> > cli::cli_h1("web_tool_script_1.R")
    #> 
    #> ── web_tool_script_1.R ─────────────────────────────────────────────────────────
    #> 
    #> > devtools::load_all(quiet = TRUE)
    #> 
    #> > use_r_packages()
    #> 
    #> > source("0_portfolio_input_check_functions.R")
    #> 
    #> > source("0_global_functions.R")
    #> 
    #> > source("0_web_functions.R")
    #> 
    #> > source("0_json_functions.R")
    #> 
    #> > source("0_portfolio_test.R")
    #> 
    #> > if (!exists("portfolio_name_ref_all")) {
    #> +     portfolio_name_ref_all <- "TestPortfolio_Input"
    #> + }
    #> 
    #> > if (!exists("portfolio_root_dir")) {
    #> +     portfolio_root_dir <- "working_dir"
    #> + }
    #> 
    #> > setup_project()
    #> 
    #> > working_location <- file.path(working_location)
    #> 
    #> > set_webtool_paths(portfolio_root_dir)
    #> 
    #> > options(r2dii_config = file.path(par_file_path, "AnalysisParameters.yml"))
    #> 
    #> > set_global_parameters(file.path(par_file_path, "AnalysisParameters.yml"))
    #> 
    #> > analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, 
    #> +     data_location_ext, dataprep_timestamp)
    #> 
    #> > create_portfolio_subfolders(portfolio_name_ref_all = portfolio_name_ref_all, 
    #> +     project_location = project_location)
    #> 
    #> > file_location <- file.path(analysis_inputs_path, "cleaned_files")
    #> 
    #> > if (new_data == TRUE) {
    #> +     currencies <- get_and_clean_currency_data()
    #> +     fund_data <- data.frame()
    #> +     fin_data <- get_and_clean_fin_data(f .... [TRUNCATED] 
    #> 
    #> > portfolio_raw <- get_input_files(portfolio_name_ref_all)
    #> Warning in cfg$parameters$portfolio_name: partial match of 'portfolio_name' to
    #> 'portfolio_name_in'
    #> Warning in cfg$parameters$investor_name: partial match of 'investor_name' to
    #> 'investor_name_in'
    #> 
    #> > portfolio <- process_raw_portfolio(portfolio_raw, 
    #> +     fin_data, fund_data, currencies, grouping_variables)
    #> 
    #> > portfolio <- add_revenue_split(has_revenue, portfolio, 
    #> +     revenue_data)
    #> 
    #> > portfolio <- create_ald_flag(portfolio, comp_fin_data, 
    #> +     debt_fin_data)
    #> 
    #> > eq_portfolio <- create_portfolio_subset(portfolio, 
    #> +     "Equity", comp_fin_data)
    #> 
    #> > cb_portfolio <- create_portfolio_subset(portfolio, 
    #> +     "Bonds", debt_fin_data)
    #> 
    #> > portfolio_total <- add_portfolio_flags(portfolio)
    #> 
    #> > portfolio_overview <- portfolio_summary(portfolio_total)
    #> 
    #> > identify_missing_data(portfolio_total)
    #> 
    #> > audit_file <- create_audit_file(portfolio_total)
    #> 
    #> > emissions_totals <- calculate_average_portfolio_emissions(portfolio_total, 
    #> +     comp_fin_data, average_sector_intensity)
    #> 
    #> > port_weights <- pw_calculations(eq_portfolio, cb_portfolio)
    #> 
    #> > file_names <- identify_portfolios(portfolio_total)
    #> 
    #> > portfolio_name <- file_names$portfolio_name
    #> 
    #> > proc_input_path_ <- file.path(proc_input_path, portfolio_name_ref_all)
    #> 
    #> > export_audit_information_jsons(audit_file_ = audit_file %>% 
    #> +     filter(portfolio_name == portfolio_name), portfolio_total_ = portfolio_total %>%  .... [TRUNCATED] 
    #> 
    #> > save_if_exists(audit_file, portfolio_name, file.path(proc_input_path_, 
    #> +     "audit_file.csv"), csv_or_rds = "csv")
    #> 
    #> > save_if_exists(portfolio_total, portfolio_name, file.path(proc_input_path_, 
    #> +     "total_portfolio.rda"))
    #> 
    #> > save_if_exists(eq_portfolio, portfolio_name, file.path(proc_input_path_, 
    #> +     "equity_portfolio.rda"))
    #> 
    #> > save_if_exists(cb_portfolio, portfolio_name, file.path(proc_input_path_, 
    #> +     "bonds_portfolio.rda"))
    #> 
    #> > save_if_exists(portfolio_overview, portfolio_name, 
    #> +     file.path(proc_input_path_, "overview_portfolio.rda"))
    #> 
    #> > save_if_exists(audit_file, portfolio_name, file.path(proc_input_path_, 
    #> +     "audit_file.rda"))
    #> 
    #> > save_if_exists(emissions_totals, portfolio_name, file.path(proc_input_path_, 
    #> +     "emissions.rda"))
    #> 
    #> > if (data_check(port_weights)) {
    #> +     port_weights <- jsonlite::toJSON(x = port_weights)
    #> +     write(x = port_weights, file = file.path(proc_input_p .... [TRUNCATED]
