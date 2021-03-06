
Helpers.

``` r
#' Look into a file
#' @examples
#' tmp <- tempfile()
#' writeLines("a\nb\nc", tmp)
#' look_into(tmp)
#' look_into(tmp, 2)
look_into <- function(path, n = -1L) {
  lines <- readLines(path, n, encoding = "UTF-8")
  writeLines(lines)
}

#' Find a file matchin a regexp
#' @examples
#' find_file("[.]profile$", "~")
#' find_file("[.]profile$", "~", all = TRUE)
find_file <- function(regexp, path = here::here(), all = FALSE) {
  out <- fs::dir_ls(path = path, regexp = regexp, recurse = TRUE, all = all)
  found <- any(fs::file_exists(out))
  if (!found) {
    rlang::warn(glue::glue("No file found matching regexp: {regexp}."))
  }
  
  out
}
```

Packages.

``` r
library(here)
library(fs)
```

### Requirement

Here I list requirements that Clare mentioned. I still don’t know how
they fit in the process but I guess that’ll become clearer soon.

#### Configurations

PACTA inputs are commonly described not as parameters to functions but
as key-value pairs in configuration files.

Here are some lines of a configuration file that I believe is current:

``` r
config_filename <- "ProjectParameters_GENERAL[.]yml"
config_general <- fs::dir_ls(here::here(), regexp = config_filename, recurse = TRUE)
config_general
#> /home/mauro/git/PACTA_analysis/parameter_files/ProjectParameters_GENERAL.yml

stopifnot(any(fs::file_exists(config_general)))

n <- 10L
look_into(config_general[[1]], n)
#> default:
#> 
#>     reporting:
#>         project_report_name: general
#>         display_currency: USD
#>         currency_exchange_value: 1
#> 
#>     parameters:
#>         timestamp: 2019Q4
#>         dataprep_timestamp: 2019Q4_250220
```

For details on what each parameter means, you may see in this demo
configuration file. It may be obsolete but it’s the only documentation I
know of.

``` r
config_demo <- "config_demo.yml"
look_into(config_demo, n)
#> default:
#>   Methodology:
#>     HasBookValue: FALSE # ignore
#>     HasRISK: TRUE # ignore a flag specifying if the physical risk assessment should be done
#>     HasMAP: TRUE # flag to specify if map data (production per country) should be prepared
#>     HasSB: FALSE # ignore for now
#> 
#>   TimeStamps:
#>     # setting the dates of the data-input for financial and asset-level data: this identifies the input files
#>     # The name "DONT-DELETE" is to alert people to not delete this directory
```

#### Portfolio file

The portfolio file matches this pattern:

``` r
pattern <- "_Input[.]csv$"
```

The files matching that pattern are located at these paths:

``` r
find_file(pattern)
#> /home/mauro/git/PACTA_analysis/sample_files/20_input_files/TestPortfolio_Input.csv
#> /home/mauro/git/PACTA_analysis/working_dir/20_Raw_Inputs/TestPortfolio_Input.csv
```

#### Analysis inputs

This section describes requirements from directories in 2DII’s dropbox
under “PortCheck/00\_Data/07\_AnalysisInputs/”. This is private data.
This example assumes you have this specific directory, alongside
PACTA\_analysis/:

``` r
analysis_input <- "../2020Q4_02082021"
fs::dir_exists(analysis_input)
#> ../2020Q4_02082021 
#>               TRUE

fs::dir_tree(analysis_input)
#> ../2020Q4_02082021
#> ├── 0_MarketPortfolios_bonds_portfolio.rda
#> ├── 0_MarketPortfolios_equity_portfolio.rda
#> ├── average_sector_intensity.rda
#> ├── bonds_ald_scenario.rda
#> ├── bonds_ald_scenario_long.rda
#> ├── bonds_ald_scenario_map.rda
#> ├── cleaned_files
#> ├── company_emissions.rda
#> ├── consolidated_financial_data.rda
#> ├── dataprep.yml
#> ├── debt_financial_data.rda
#> ├── equity_ald_scenario.rda
#> ├── equity_ald_scenario_long.rda
#> ├── equity_ald_scenario_map.rda
#> ├── masterdata_debt_datastore.rda
#> ├── masterdata_debt_datastore_technology_type_view.rda
#> ├── masterdata_ownership_datastore.rda
#> ├── masterdata_ownership_datastore_technology_type_view.rda
#> ├── oil_and_gas_resource_type_rollup_debt.rda
#> ├── oil_and_gas_resource_type_rollup_ownership.rda
#> ├── scenarios_analysisinput_2021.rda
#> ├── scenarios_analysisinput_long_2021.rda
#> └── security_financial_data.rda
```

Required files must be found:

``` r
required <- c(
  "security_financial_data.rda",
  "consolidated_financial_data.rda",
  "debt_financial_data.rda",
  "bonds_ald_scenario.rda",
  "equity_ald_scenario.rda",
  "masterdata_ownership_datastore.rda",
  "masterdata_debt_datastore.rda"
)

exists_in <- function(x, path) fs::file_exists(find_file(x, path))
found <- purrr::map(required, exists_in, analysis_input)
stopifnot(all(unlist(found)))
```

Optional files may or may not be found:

``` r
optional <- c(
  # For example: fund_data_2019Q4.rda
  "fund_data_20..Q.[.]rda",
  "revenue_data_member_ticker.rda",
  "company_emissions.rda",
  "average_sector_intensity.rda"
)

found <- purrr::map(optional, exists_in, analysis_input)
#> Warning: No file found matching regexp: fund_data_20..Q.[.]rda.
#> Warning: No file found matching regexp: revenue_data_member_ticker.rda.
unlist(found)
#>        ../2020Q4_02082021/company_emissions.rda 
#>                                            TRUE 
#> ../2020Q4_02082021/average_sector_intensity.rda 
#>                                            TRUE
```

The absence of some files determines affects some of the values in the
configuration file. For example, if the file
“revenue\_data\_member\_ticker.rda” is missing, the parameter
`has_revenue` should be set to `FALSE`.

``` r
if_missing_param_is_false <- function(pattern,
                                      param,
                                      dir = analysis_input,
                                      config = config_general) {
  methodology <- config::get("methodology", file = config)
  
  available <- isTRUE(suppressWarnings(exists_in(pattern, dir)))
  if (!available) {
    param_is_false <- isFALSE(methodology[[param]])
    stopifnot(param_is_false)
  }
  
  invisible(pattern)
}

if_missing_param_is_false("revenue_data_member_ticker.rda", "has_revenue")
if_missing_param_is_false("company_emissions.rda", "inc_emissionfactors")
if_missing_param_is_false("average_sector_intensity.rda", "inc_emissionfactors")
```
