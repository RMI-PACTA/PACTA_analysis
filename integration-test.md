Integration test: Run the web tool
================
2020-10-02

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
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(devtools))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(config))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(renv))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(fs))
library(here)
```

All packages detected in the directory PACTA\_analysis:

``` r
detect_packages <- function() {
  packages <- renv::dependencies()$Package
  sort(unique(packages))
}

detect_packages()
#> Finding R package dependencies ...
#> Warning in readLines(file, warn = FALSE, encoding = "UTF-8"): invalid input
#> found on input connection '/Users/jacobkastl/Documents/git/PACTA_analysis/
#> 0_fund_processing.R'
#> [5/22] [6/22] [7/22] [8/22] [9/22] [10/22] [11/22] [12/22] [13/22] [14/22] [15/22] [16/22] [17/22] [18/22] [19/22] [20/22] [21/22] [22/22] Done!
#>  [1] "assertthat"   "base"         "config"       "countrycode" 
#>  [5] "cowplot"      "devtools"     "dplyr"        "extrafont"   
#>  [9] "fs"           "fst"          "ggforce"      "ggmap"       
#> [13] "ggplot2"      "ggrepel"      "ggthemes"     "glue"        
#> [17] "grid"         "gridExtra"    "here"         "janitor"     
#> [21] "jsonlite"     "knitr"        "lme4"         "matrixStats" 
#> [25] "plyr"         "purrr"        "r2dii.utils"  "RColorBrewer"
#> [29] "readr"        "readxl"       "renv"         "reshape2"    
#> [33] "rlang"        "rmarkdown"    "rstudioapi"   "rworldmap"   
#> [37] "scales"       "sitools"      "stringr"      "testthat"    
#> [41] "tidyr"        "tidyselect"   "tidyverse"    "tools"       
#> [45] "xml2"
```

<details>

<summary>Session information</summary>

``` r
devtools::session_info()
#> ─ Session info ────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 4.0.2 (2020-06-22)
#>  os       macOS Catalina 10.15.4      
#>  system   x86_64, darwin17.0          
#>  ui       RStudio                     
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       Europe/Berlin               
#>  date     2020-10-02                  
#> 
#> ─ Packages ────────────────────────────────────────────────────────────────
#>  package     * version    date       lib
#>  assertthat    0.2.1      2019-03-21 [1]
#>  backports     1.1.8      2020-06-17 [1]
#>  blob          1.2.1      2020-01-20 [1]
#>  broom         0.5.6      2020-04-20 [1]
#>  callr         3.4.3      2020-03-28 [1]
#>  cellranger    1.1.0      2016-07-27 [1]
#>  cli           2.0.2      2020-02-28 [1]
#>  colorspace    1.4-1      2019-03-18 [1]
#>  config      * 0.3        2018-03-27 [1]
#>  countrycode * 1.2.0      2020-05-22 [1]
#>  crayon        1.3.4      2017-09-16 [1]
#>  data.table    1.12.8     2019-12-09 [1]
#>  DBI           1.1.0      2019-12-15 [1]
#>  dbplyr        1.4.4      2020-05-27 [1]
#>  desc          1.2.0      2018-05-01 [1]
#>  devtools    * 2.3.1      2020-07-21 [1]
#>  digest        0.6.25     2020-02-23 [1]
#>  dplyr       * 1.0.0      2020-05-29 [1]
#>  ellipsis      0.3.1      2020-05-15 [1]
#>  evaluate      0.14       2019-05-28 [1]
#>  fansi         0.4.1      2020-01-08 [1]
#>  forcats     * 0.5.0      2020-03-01 [1]
#>  fs          * 1.5.0      2020-07-31 [1]
#>  fst         * 0.9.2      2020-04-01 [1]
#>  generics      0.0.2      2018-11-29 [1]
#>  ggplot2     * 3.3.1      2020-05-28 [1]
#>  glue        * 1.4.1      2020-05-13 [1]
#>  gtable        0.3.0      2019-03-25 [1]
#>  haven         2.3.1      2020-06-01 [1]
#>  here        * 0.1        2017-05-28 [1]
#>  hms           0.5.3      2020-01-08 [1]
#>  htmltools     0.5.0      2020-06-16 [1]
#>  httr          1.4.2      2020-07-20 [1]
#>  janitor       2.0.1.9000 2020-06-17 [1]
#>  jsonlite    * 1.7.0      2020-06-25 [1]
#>  knitr         1.28       2020-02-06 [1]
#>  lattice       0.20-41    2020-04-02 [1]
#>  lifecycle     0.2.0      2020-03-06 [1]
#>  lubridate     1.7.9      2020-06-08 [1]
#>  magrittr      1.5        2014-11-22 [1]
#>  memoise       1.1.0      2017-04-21 [1]
#>  modelr        0.1.8      2020-05-19 [1]
#>  munsell       0.5.0      2018-06-12 [1]
#>  nlme          3.1-148    2020-05-24 [1]
#>  packrat       0.5.0      2018-11-14 [1]
#>  pillar        1.4.6      2020-07-10 [1]
#>  pkgbuild      1.0.8      2020-05-07 [1]
#>  pkgconfig     2.0.3      2019-09-22 [1]
#>  pkgload       1.1.0      2020-05-29 [1]
#>  plyr          1.8.6      2020-03-03 [1]
#>  prettyunits   1.1.1      2020-01-24 [1]
#>  processx      3.4.2      2020-02-09 [1]
#>  ps            1.3.3      2020-05-08 [1]
#>  purrr       * 0.3.4      2020-04-17 [1]
#>  r2dii.utils * 0.0.0.9003 2020-06-17 [1]
#>  R6            2.4.1      2019-11-12 [1]
#>  Rcpp          1.0.4.6    2020-04-09 [1]
#>  readr       * 1.3.1      2018-12-21 [1]
#>  readxl      * 1.3.1      2019-03-13 [1]
#>  remotes       2.2.0      2020-07-21 [1]
#>  renv        * 0.12.0     2020-08-28 [1]
#>  reprex        0.3.0      2019-05-16 [1]
#>  reshape2    * 1.4.4      2020-04-09 [1]
#>  rlang       * 0.4.7      2020-07-09 [1]
#>  rmarkdown     2.3        2020-06-18 [1]
#>  rprojroot     1.3-2      2018-01-03 [1]
#>  rstudioapi    0.11       2020-02-07 [1]
#>  rvest         0.3.5      2019-11-08 [1]
#>  scales      * 1.1.1      2020-05-11 [1]
#>  sessioninfo   1.1.1      2018-11-05 [1]
#>  snakecase     0.11.0     2019-05-25 [1]
#>  stringi       1.4.6      2020-02-17 [1]
#>  stringr     * 1.4.0      2019-02-10 [1]
#>  testthat    * 2.3.2      2020-03-02 [1]
#>  tibble      * 3.0.3      2020-07-10 [1]
#>  tidyr       * 1.1.0      2020-05-20 [1]
#>  tidyselect  * 1.1.0      2020-05-11 [1]
#>  tidyverse   * 1.3.0      2019-11-21 [1]
#>  usethis     * 1.6.1      2020-04-29 [1]
#>  vctrs         0.3.2      2020-07-15 [1]
#>  withr         2.2.0      2020-04-20 [1]
#>  writexl     * 1.3        2020-05-05 [1]
#>  xfun          0.14       2020-05-20 [1]
#>  xml2          1.3.2      2020-04-23 [1]
#>  yaml          2.2.1      2020-02-01 [1]
#>  source                                        
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.1)                                
#>  CRAN (R 4.0.2)                                
#>  Github (sfirke/janitor@d040478)               
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  Github (2DegreesInvesting/r2dii.utils@fd29303)
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#> 
#> [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
```

</details>

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
#> Warning: Removing existing file: /Users/jacobkastl/Documents/git/
#> PACTA_analysis/working_dir/20_Raw_Inputs/TestPortfolio_Input.csv

file_copy(example_dataset, expected_dataset)

expect_true(file_exists(expected_dataset))
```

## Directories

Ensure the required directories exist, and are empty.

``` r
ensure_empty_directory <- function(directory) {
  if (dir_exists(directory)) {
    dir_delete(directory)
  }

  dir_create(directory)
  file_create(path(directory, ".gitkeep.txt"))

  invisible(directory)
}

children <- c("30_Processed_Inputs", "40_Results", "50_Outputs")
(paths <- here("working_dir", children))
#> [1] "/Users/jacobkastl/Documents/git/PACTA_analysis/working_dir/30_Processed_Inputs"
#> [2] "/Users/jacobkastl/Documents/git/PACTA_analysis/working_dir/40_Results"         
#> [3] "/Users/jacobkastl/Documents/git/PACTA_analysis/working_dir/50_Outputs"

walk(paths, ensure_empty_directory)
```

Ensure the following repos are siblings, i.e. they are inside the same
parent directory:

  - “2DegreesInvesting/pacta-data/”
  - “2DegreesInvesting/create\_interactive\_report/”
  - “2DegreesInvesting/PACTA\_analysis/”

<!-- end list -->

``` r
is_sibling <- function(x) {
  parent <- path_dir(here())
  dir_exists(path(parent, x))
}

repos <- c("pacta-data", "create_interactive_report", "PACTA_analysis")
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

(files <- dir_ls(regexp = "web_tool_script"))
#> web_tool_script_1.R web_tool_script_2.R web_tool_script_3.R

this_pattern <- "portfolio_name_ref_all.*<-"
matched <- map(files, show_pattern_in_file, pattern = this_pattern)
walk(matched, writeLines)
#>   portfolio_name_ref_all <- c("TestPortfolio_Input") # must be the same name as in the _PortfolioParameters.yml
#>   portfolio_name_ref_all <- c("TestPortfolio_Input") # must be the same name as in the _PortfolioParameters.yml
#>   portfolio_name_ref_all <- c("TestPortfolio_Input") # must be the same name as in the _PortfolioParameters.yml

script_has_this_pattern <- grepl(this_pattern, matched)
expect_true(all(script_has_this_pattern))
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
make_config_portable <- function(config) {
  lines <- readLines(config, encoding = "UTF-8")
  lines <- make_paths_portable(lines)
  writeLines(lines, config)
  
  invisible(config)
}

make_paths_portable <- function(x) {
  x %>% 
    root_field_path("project_location_ext", pattern = "PACTA_analysis") %>% 
    root_field_path("data_location_ext", pattern = "pacta-data") %>% 
    root_field_path("template_location", pattern = "create_interactive_report")
}

root_field_path <- function(x, field, pattern) {
  parent <- path_dir(here())
  value <- path(parent, extract_from(x, pattern))
  sub(glue("({field}:[ ]?).*"), glue("\\1{value}/"), x)
}

extract_from <- function(x, pattern) {
  line <- grep(pattern, x, value = TRUE)
  sub(glue(".*({pattern}.*)"), "\\1", line)
}

make_config_portable(config_2)
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
#>         project_location_ext: /Users/jacobkastl/Documents/git/PACTA_analysis/
#>         data_location_ext: /Users/jacobkastl/Documents/git/pacta-data/2019Q4/
#>         template_location: /Users/jacobkastl/Documents/git/create_interactive_report/
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
#> Warning in read_file(paste0(file_location, "/fund_data.fst")): /Users/
#> jacobkastl/Documents/git/pacta-data/2019Q4/cleaned_files/fund_data.fst does
#> not exist
#> [1] "No Equity in portfolio"
expect_true(dir_has_files(out_1))
```

Populate the directory for results:

``` r
out_2 <- path("working_dir", "40_Results")

expect_false(dir_has_files(out_2))
source("web_tool_script_2.R")
#> Warning in dir.create(.x): '/Users/jacobkastl/Documents/git/PACTA_analysis/
#> working_dir//30_Processed_Inputs/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/Users/jacobkastl/Documents/git/PACTA_analysis/
#> working_dir//40_Results/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/Users/jacobkastl/Documents/git/PACTA_analysis/
#> working_dir//50_Outputs/TestPortfolio_Input' already exists
#> [1] "1: Test"
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

## TODO

  - Some warnings may be avoided if required directories are created
    only if they don’t already exist.

<!-- end list -->

``` r
Warning message:
In read_file(paste0(file_location, "/fund_data.fst")) :
  ~/git/pacta-data/2019Q4/cleaned_files/fund_data.fst does not exist
```

  - I’m not sure if this dataset is crucial, but it’s missing from my
    clone of pacta-data/:

<!-- end list -->

``` r
dir_ls(path("..", "pacta-data", "2019Q4", "cleaned_files"))
#> ../pacta-data/2019Q4/cleaned_files/average_sector_intensity.fst
#> ../pacta-data/2019Q4/cleaned_files/comp_fin_data.fst
#> ../pacta-data/2019Q4/cleaned_files/company_emissions.fst
#> ../pacta-data/2019Q4/cleaned_files/currencies.fst
#> ../pacta-data/2019Q4/cleaned_files/debt_fin_data.fst
#> ../pacta-data/2019Q4/cleaned_files/fin_data.fst
```
