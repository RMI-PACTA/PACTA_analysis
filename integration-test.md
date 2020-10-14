Integration test: Run the web tool
================
2020-10-13

## Introduction

This document provides a reproducible example of how to run the so
called “web tool”. It is based on [these
instructions](https://bit.ly/2RCRJn7). It is useful in three ways:

  - To document this repository.
  - To onboard contributors.
  - As an integration test – to test running this file produces the
    expected output. We do this both locally and on a continuous
    integration service (GitHub actions) with multiple platforms and
    versions of R.

## Environment

Packages used in this file:

``` r
library(purrr)
library(devtools)
#> Loading required package: usethis
library(testthat)
#> 
#> Attaching package: 'testthat'
#> The following object is masked from 'package:devtools':
#> 
#>     test_file
#> The following object is masked from 'package:purrr':
#> 
#>     is_null
library(config)
#> 
#> Attaching package: 'config'
#> The following objects are masked from 'package:base':
#> 
#>     get, merge
library(rlang)
#> 
#> Attaching package: 'rlang'
#> The following objects are masked from 'package:testthat':
#> 
#>     is_false, is_null, is_true
#> The following objects are masked from 'package:purrr':
#> 
#>     %@%, as_function, flatten, flatten_chr, flatten_dbl, flatten_int,
#>     flatten_lgl, flatten_raw, invoke, list_along, modify, prepend,
#>     splice
library(renv)
#> 
#> Attaching package: 'renv'
#> The following object is masked from 'package:rlang':
#> 
#>     modify
#> The following object is masked from 'package:devtools':
#> 
#>     install
#> The following object is masked from 'package:purrr':
#> 
#>     modify
#> The following object is masked from 'package:stats':
#> 
#>     update
#> The following objects are masked from 'package:utils':
#> 
#>     history, upgrade
#> The following objects are masked from 'package:base':
#> 
#>     load, remove
library(glue)
library(fs)
library(here)
#> here() starts at /home/mauro/git/PACTA_analysis
library(conflicted)

devtools::load_all()
#> Loading PACTA.analysis
resolve_conflicts()
#> [conflicted] Will prefer dplyr::filter over any other package
#> [conflicted] Will prefer dplyr::lag over any other package
#> [conflicted] Will prefer dplyr::mutate over any other package
#> [conflicted] Will prefer here::here over any other package
#> [conflicted] Will prefer dplyr::rename over any other package
#> [conflicted] Will prefer dplyr::summarise over any other package
#> [conflicted] Will prefer dplyr::arrange over any other package
```

All packages detected in the directory PACTA\_analysis:

``` r
detect_packages <- function() {
  packages <- renv::dependencies()$Package
  sort(unique(packages))
}

detect_packages()
#> Finding R package dependencies ... Done!
#>  [1] "assertthat"     "base"           "bookdown"       "config"        
#>  [5] "conflicted"     "countrycode"    "data.table"     "devtools"      
#>  [9] "dplyr"          "fs"             "fst"            "ggplot2"       
#> [13] "glue"           "here"           "janitor"        "jsonlite"      
#> [17] "knitr"          "magrittr"       "PACTA.analysis" "plyr"          
#> [21] "purrr"          "R"              "readr"          "readxl"        
#> [25] "renv"           "reshape2"       "rlang"          "rmarkdown"     
#> [29] "rstudioapi"     "scales"         "stringr"        "testthat"      
#> [33] "tibble"         "tidyr"          "tidyselect"     "tools"         
#> [37] "usethis"        "withr"          "writexl"        "zoo"
```

<details>

<summary>Session information</summary>

``` r
devtools::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 4.0.3 (2020-10-10)
#>  os       Ubuntu 18.04.5 LTS          
#>  system   x86_64, linux-gnu           
#>  ui       X11                         
#>  language en_US:en                    
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       America/Chicago             
#>  date     2020-10-13                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  ! package        * version     date       lib
#>    assertthat       0.2.1       2019-03-21 [1]
#>    backports        1.1.10      2020-09-15 [1]
#>    callr            3.5.0       2020-10-08 [1]
#>    cli              2.0.2       2020-02-28 [1]
#>    config         * 0.3         2018-03-27 [1]
#>    conflicted     * 1.0.4       2019-06-21 [1]
#>    crayon           1.3.4.9000  2020-09-03 [1]
#>    desc             1.2.0       2018-05-01 [1]
#>    devtools       * 2.3.2       2020-09-18 [1]
#>    digest           0.6.25      2020-02-23 [1]
#>    dplyr            1.0.2       2020-08-18 [1]
#>    ellipsis         0.3.1       2020-05-15 [1]
#>    evaluate         0.14        2019-05-28 [1]
#>    fansi            0.4.1       2020-01-08 [1]
#>    fs             * 1.5.0       2020-07-31 [1]
#>    fst              0.9.4       2020-08-27 [1]
#>    generics         0.0.2       2018-11-29 [1]
#>    glue           * 1.4.2       2020-08-27 [1]
#>    here           * 0.1         2017-05-28 [1]
#>    hms              0.5.3       2020-01-08 [1]
#>    htmltools        0.5.0.9001  2020-10-08 [1]
#>    janitor          2.0.1.9000  2020-05-14 [1]
#>    knitr            1.30        2020-09-22 [1]
#>    lifecycle        0.2.0       2020-03-06 [1]
#>    lubridate        1.7.9       2020-06-08 [1]
#>    magrittr         1.5.0.9000  2020-09-28 [1]
#>    memoise          1.1.0       2017-04-21 [1]
#>  P PACTA.analysis * 0.0.0.9000  2020-10-01 [?]
#>    pillar           1.4.6       2020-07-10 [1]
#>    pkgbuild         1.1.0       2020-07-13 [1]
#>    pkgconfig        2.0.3       2019-09-22 [1]
#>    pkgload          1.1.0       2020-05-29 [1]
#>    prettyunits      1.1.1       2020-01-24 [1]
#>    processx         3.4.4       2020-09-03 [1]
#>    ps               1.4.0       2020-10-07 [1]
#>    purrr          * 0.3.4       2020-04-17 [1]
#>    R6               2.4.1       2019-11-12 [1]
#>    Rcpp             1.0.5       2020-07-06 [1]
#>    readr            1.3.1       2018-12-21 [1]
#>    remotes          2.2.0       2020-07-21 [1]
#>    renv           * 0.12.0      2020-08-28 [1]
#>    rlang          * 0.4.8       2020-10-08 [1]
#>    rmarkdown        2.4         2020-09-30 [1]
#>    rprojroot        1.3-2       2018-01-03 [1]
#>    rstudioapi       0.11        2020-02-07 [1]
#>    sessioninfo      1.1.1       2018-11-05 [1]
#>    snakecase        0.11.0      2019-05-25 [1]
#>    stringi          1.5.3       2020-09-09 [1]
#>    stringr          1.4.0       2019-02-10 [1]
#>    testthat       * 2.99.0.9000 2020-10-11 [1]
#>    tibble           3.0.3       2020-07-10 [1]
#>    tidyselect       1.1.0       2020-05-11 [1]
#>    usethis        * 1.6.3       2020-09-17 [1]
#>    vctrs            0.3.4       2020-08-29 [1]
#>    withr            2.3.0       2020-09-22 [1]
#>    xfun             0.18        2020-09-29 [1]
#>    yaml             2.2.1       2020-02-01 [1]
#>  source                             
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.2)                     
#>  RSPM (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  RSPM (R 4.0.2)                     
#>  Github (r-lib/crayon@6b3f0c6)      
#>  CRAN (R 4.0.0)                     
#>  RSPM (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  RSPM (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  RSPM (R 4.0.2)                     
#>  RSPM (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  RSPM (R 4.0.2)                     
#>  RSPM (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  Github (rstudio/htmltools@5d42d84) 
#>  Github (sfirke/janitor@ecc9f4e)    
#>  RSPM (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  Github (tidyverse/magrittr@0221e18)
#>  CRAN (R 4.0.0)                     
#>  local                              
#>  CRAN (R 4.0.0)                     
#>  RSPM (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.2)                     
#>  CRAN (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  RSPM (R 4.0.2)                     
#>  RSPM (R 4.0.0)                     
#>  CRAN (R 4.0.2)                     
#>  CRAN (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  RSPM (R 4.0.2)                     
#>  CRAN (R 4.0.0)                     
#>  Github (r-lib/testthat@d10a276)    
#>  CRAN (R 4.0.0)                     
#>  CRAN (R 4.0.0)                     
#>  RSPM (R 4.0.2)                     
#>  RSPM (R 4.0.2)                     
#>  RSPM (R 4.0.2)                     
#>  RSPM (R 4.0.2)                     
#>  RSPM (R 4.0.0)                     
#> 
#> [1] /home/mauro/R/x86_64-pc-linux-gnu-library/4.0
#> [2] /usr/local/lib/R/site-library
#> [3] /usr/lib/R/site-library
#> [4] /usr/lib/R/library
#> 
#>  P ── Loaded and on-disk path mismatch.
```

</details>

## Functions

## Data

Ensure the example data is available.

``` r
file_name <- "TestPortfolio_Input.csv"
example_dataset <- here("sample_files", "20_input_files", file_name)

expect_true(file_exists(example_dataset))
```

Ensure the example data is copied into the expected directory.

``` r
expected_dataset <- here("working_dir", "20_Raw_Inputs", file_name)

if (file_exists(expected_dataset)) {
  warn(glue("Removing existing file: {expected_dataset}"))
  file_delete(expected_dataset)
}
#> Warning: Removing existing file: /home/mauro/git/PACTA_analysis/working_dir/
#> 20_Raw_Inputs/TestPortfolio_Input.csv

file_copy(example_dataset, expected_dataset)

expect_true(file_exists(expected_dataset))
```

## Directories

Ensure the required directories exist, and are empty.

``` r
ensure_empty_directory <- function(directory) {
  if (dir_exists(directory)) {
    not_hidden <- fs::dir_ls(directory)
    file_delete(not_hidden)
  }

  dir_create(directory)
  file_create(path(directory, ".gitkeep.txt"))

  invisible(directory)
}

children <- c("30_Processed_Inputs", "40_Results", "50_Outputs")
(paths <- here("working_dir", children))
#> [1] "/home/mauro/git/PACTA_analysis/working_dir/30_Processed_Inputs"
#> [2] "/home/mauro/git/PACTA_analysis/working_dir/40_Results"         
#> [3] "/home/mauro/git/PACTA_analysis/working_dir/50_Outputs"

walk(paths, ensure_empty_directory)
```

Ensure the following repos are siblings, i.e. they are inside the same
parent directory:

  - “2DegreesInvesting/pacta-data/”
  - “2DegreesInvesting/create\_interactive\_report/”
  - “2DegreesInvesting/PACTA\_analysis/”
  - “2DegreesInvesting/StressTestingModelDev/”

<!-- end list -->

``` r
is_sibling <- function(x) {
  parent <- path_dir(here())
  dir_exists(path(parent, x))
}

repos <- c("pacta-data", "create_interactive_report", "PACTA_analysis", "StressTestingModelDev")
all_siblings <- all(map_lgl(repos, is_sibling))

expect_true(all_siblings)
```

**NOTE: As of this writing the main line of development is not the
standard branch `master` – it is the branch
`current_web_functionality`.**

Ensure the expected working directory.

``` r
expect_equal(path_file(here()), "PACTA_analysis")
```

## `portfolio_name_ref_all <- "TestPortfolio_Input"`

Ensure `portfolio_name_ref_all` takes the value “TestPortfolio\_Input”
in the files which name contains “web\_tool\_scripts”.

``` r
# What value is currently assigned to the variable `portfolio_name_ref_all`?
show_pattern_in_file <- function(file, pattern) {
  grep(pattern, readLines(file), value = TRUE)
}

pattern <- "set_portfolio-name-ref-all_working-location_and_web-parameters.R"
(files <- dir_ls("deduplicate", regexp = pattern))
#> deduplicate/set_portfolio-name-ref-all_working-location_and_web-parameters.R

this_pattern <- "portfolio_name_ref_all.*<-"
matched <- map(files, show_pattern_in_file, pattern = this_pattern)
walk(matched, writeLines)
#>   portfolio_name_ref_all <- get_portfolio_name()
#>   portfolio_name_ref_all <- c("TestPortfolio_Input") # must be the same name as in the _PortfolioParameters.yml

script_has_this_pattern <- grepl(this_pattern, matched)
expect_true(any(script_has_this_pattern))
```

**NOTE: If the value of `portfolio_name_ref_all` comes from the user, we
should provide an interface for the user to supply it – so that the user
needs not to change the source code.**

## Configurations

Ensure this configuration file exists:

``` r
config_1 <- here(
  "working_dir",
  "10_Parameter_File",
  "TestPortfolio_Input_PortfolioParameters.yml"
)

expect_true(file_exists(config_1))
```

``` r
look_into <- function(path, n = -1L) {
  lines <- readLines(path, n, encoding = "UTF-8")
  writeLines(lines)
}

look_into(config_1)
#> default:
#>     parameters:
#>         portfolio_name_in: TestPortfolio_Input
#>         investor_name_in: Test
```

Ensure this other configuration file also exists:

``` r
config_2 <- here("parameter_files", "WebParameters_2dii.yml")

expect_true(file_exists(config_2))
```

Ensure the paths in the configuration file work both locally and on
GitHub actions:

``` r
uses_relative_paths <- function(lines) {
  patterns <- c(
    "project_location_ext:[ ]?\\.\\./",
    "data_location_ext:[ ]?\\.\\./",
    "template_location:[ ]?\\.\\./",
    "stress_test_location:[ ]?\\.\\./"
  )
  
  all(purrr::map_lgl(patterns, ~any(grepl(.x, lines))))
}

if (!uses_relative_paths(readLines(config_2))) {
  abort("Paths in the configuration file must use the form ../this_repo/")
}

expect_true(uses_relative_paths(readLines(config_2)))
```

``` r
config_paths <- config::get(file = config_2)$paths
all_paths_exist <- all(map_lgl(config_paths, dir_exists))

expect_true(all_paths_exist)
```

``` r
look_into(config_2)
#> default:
#>     paths:
#>        # Use form "../this_repo/" (not "./" or "/path/to/this_repo")
#>         project_location_ext: ../PACTA_analysis/
#>         data_location_ext: ../pacta-data/2019Q4/
#>         template_location: ../create_interactive_report/
#>         stress_test_location: ../StressTestingModelDev/
#>     parameters:
#>         project_name: working_dir
#>         twodii_internal: FALSE
#>         new_data: FALSE
```

## Scripts

Populate the directory for processed inputs:

``` r
dir_has_files <- function(path) {
  stopifnot(is_dir(path))

  contents <- dir_ls(path, recurse = TRUE)
  has_files <- any(map_lgl(contents, is_file))
  has_files
}

out_1 <- path("working_dir", "30_Processed_Inputs")

expect_false(dir_has_files(out_1))
source("web_tool_script_1.R")
#> [1] "No Equity in portfolio"
expect_true(dir_has_files(out_1))
```

Populate the directory for results:

``` r
out_2 <- path("working_dir", "40_Results")

expect_false(dir_has_files(out_2))
source("web_tool_script_2.R")
#> [1] "1: Test"
#> [1] "Autmotive scenario values for 2035 and 2040 are recalculated as there was an error in the scenario data (we now extrapolate using 2025 and 2030 figures. Check scenario data if error is still present, might be fixed already"
#> [1] "Autmotive scenario values for 2035 and 2040 are recalculated as there was an error in the scenario data (we now extrapolate using 2025 and 2030 figures. Check scenario data if error is still present, might be fixed already"
#> [1] "Autmotive scenario values for 2035 and 2040 are recalculated as there was an error in the scenario data (we now extrapolate using 2025 and 2030 figures. Check scenario data if error is still present, might be fixed already"
#> [1] "Autmotive scenario values for 2035 and 2040 are recalculated as there was an error in the scenario data (we now extrapolate using 2025 and 2030 figures. Check scenario data if error is still present, might be fixed already"
#> [1] "Autmotive scenario values for 2035 and 2040 are recalculated as there was an error in the scenario data (we now extrapolate using 2025 and 2030 figures. Check scenario data if error is still present, might be fixed already"
#> [1] "Autmotive scenario values for 2035 and 2040 are recalculated as there was an error in the scenario data (we now extrapolate using 2025 and 2030 figures. Check scenario data if error is still present, might be fixed already"
#> [1] "Autmotive scenario values for 2035 and 2040 are recalculated as there was an error in the scenario data (we now extrapolate using 2025 and 2030 figures. Check scenario data if error is still present, might be fixed already"
#> [1] "Autmotive scenario values for 2035 and 2040 are recalculated as there was an error in the scenario data (we now extrapolate using 2025 and 2030 figures. Check scenario data if error is still present, might be fixed already"
#> [1] "Autmotive scenario values for 2035 and 2040 are recalculated as there was an error in the scenario data (we now extrapolate using 2025 and 2030 figures. Check scenario data if error is still present, might be fixed already"
#> Warning: `data_frame()` is deprecated as of tibble 1.1.0.
#> Please use `tibble()` instead.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_warnings()` to see where this warning was generated.
#> [1] "No Equity Portfolio Data available. Skipping!"
#> [1] "Calculate Stress Test for Bonds Portfolio"
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.
#> Warning: `mutate_()` is deprecated as of dplyr 0.7.0.
#> Please use `mutate()` instead.
#> See vignette('programming') for more help
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_warnings()` to see where this warning was generated.
#> Warning in data.table::melt(., id.vars = c("scenario_name", "year_of_shock", :
#> The melt generic in data.table has been passed a tbl_df and will attempt
#> to redirect to the relevant reshape2 method; please note that reshape2 is
#> deprecated, and this redirection is now deprecated as well. To continue using
#> melt methods from reshape2 while both libraries are attached, e.g. melt.list,
#> you can prepend the namespace like reshape2::melt(.). In the next version, this
#> warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The dcast generic in data.table has been passed a grouped_df
#> and will attempt to redirect to the reshape2::dcast; please note that reshape2
#> is deprecated, and this redirection is now deprecated as well. Please do this
#> redirection yourself like reshape2::dcast(.). In the next version, this warning
#> will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df and
#> will attempt to redirect to the relevant reshape2 method; please note that
#> reshape2 is deprecated, and this redirection is now deprecated as well. To
#> continue using melt methods from reshape2 while both libraries are attached,
#> e.g. melt.list, you can prepend the namespace like reshape2::melt(.). In the
#> next version, this warning will become an error.
expect_true(dir_has_files(out_2))
```

Populate the directory for outputs:

``` r
out_3 <- path("working_dir", "50_Outputs")

expect_false(dir_has_files(out_3))
source("web_tool_script_3.R")
expect_true(dir_has_files(out_3))
```

Ensure the output includes specific types of files:

``` r
outputs <- path("working_dir", "50_Outputs")

css <- dir_ls(outputs, recurse = TRUE, regexp = "[.]css")
expect_true(length(css) > 0L)

js <- dir_ls(outputs, recurse = TRUE, regexp = "[.]js")
expect_true(length(js) > 0L)

index <- dir_ls(outputs, recurse = TRUE, regexp = "index[.]html")
expect_true(length(index) > 0L)

zip <- dir_ls(outputs, recurse = TRUE, regexp = "[.]zip")
expect_true(length(zip) > 0L)
```

## Output

``` r
look_into(index, n = 20L)
#> <!DOCTYPE html>
#> <html lang="" xml:lang="">
#> <head>
#> 
#>   <meta charset="utf-8" />
#>   <meta http-equiv="X-UA-Compatible" content="IE=edge" />
#>   <title>Interactive Portfolio Report</title>
#>   <meta name="description" content="" />
#>   <meta name="generator" content="bookdown 0.19 and GitBook 2.6.7" />
#> 
#>   <meta property="og:title" content="Interactive Portfolio Report" />
#>   <meta property="og:type" content="book" />
#>   
#>   
#>   
#>   
#> 
#>   <meta name="twitter:card" content="summary" />
#>   <meta name="twitter:title" content="Interactive Portfolio Report" />
#> 

dir_tree(path(outputs, "TestPortfolio_Input"), recurse = FALSE)
#> working_dir/50_Outputs/TestPortfolio_Input
#> ├── 2dii_gitbook_style.css
#> ├── TestPortfolio_Input_results.zip
#> ├── company_charts
#> ├── css
#> ├── data
#> ├── export
#> ├── img
#> ├── index.html
#> ├── js
#> ├── libs
#> └── search_index.json
```
