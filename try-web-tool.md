Running the web tool: A reproducible example
================
2020-09-22

## Introduction

This document aims to provide a reproducible example of how to run the
web tool that PACTA\_analysis provides. It is based on [these
instructions by Jacob](https://bit.ly/2RCRJn7). Here are some of the
ways you may use this document:

  - To document the repository, by incorporating it into the README
    file.
  - To onboard contributors.
  - To test the output of PACTA\_analysis remains unchanged, maybe by
    running this document on a continuous integration service like
    GitHub actions.

## Setup

### Dependencies

This report uses the following packages:

``` r
library(testthat)
library(glue)
library(config)
library(rlang)
library(here)
library(renv)
library(fs)
library(tidyverse)
library(devtools)
```

These are all the packages detected in the PACTA\_analysis project:

``` r
pkg <- renv::dependencies()
#> Finding R package dependencies ... Done!
sort(unique(pkg$Package))
#>  [1] "assertthat"   "base"         "bookdown"     "config"       "countrycode" 
#>  [6] "cowplot"      "dplyr"        "extrafont"    "fs"           "fst"         
#> [11] "ggforce"      "ggmap"        "ggplot2"      "ggrepel"      "ggthemes"    
#> [16] "glue"         "grid"         "gridExtra"    "here"         "janitor"     
#> [21] "jsonlite"     "knitr"        "lme4"         "matrixStats"  "plyr"        
#> [26] "purrr"        "r2dii.utils"  "RColorBrewer" "readr"        "readxl"      
#> [31] "renv"         "reshape2"     "rlang"        "rmarkdown"    "rstudioapi"  
#> [36] "rworldmap"    "scales"       "sitools"      "stringr"      "testthat"    
#> [41] "tidyr"        "tidyselect"   "tidyverse"    "tools"        "withr"       
#> [46] "xml2"
```

The published
[README](https://github.com/2DegreesInvesting/PACTA_analysis#system-requirements)
suggest this project has a number of additional dependencies. That
information seems quite restrictive and outdated.

<details>

<summary>Here is the information for this session</summary>

``` r
devtools::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 4.0.2 (2020-06-22)
#>  os       Ubuntu 18.04.5 LTS          
#>  system   x86_64, linux-gnu           
#>  ui       X11                         
#>  language en_US:en                    
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       America/Chicago             
#>  date     2020-09-22                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  package     * version     date       lib
#>  assertthat    0.2.1       2019-03-21 [1]
#>  backports     1.1.10      2020-09-15 [1]
#>  blob          1.2.1       2020-01-20 [1]
#>  broom         0.7.0       2020-07-09 [1]
#>  callr         3.4.4       2020-09-07 [1]
#>  cellranger    1.1.0       2016-07-27 [1]
#>  cli           2.0.2       2020-02-28 [1]
#>  colorspace    1.4-1       2019-03-18 [1]
#>  config      * 0.3         2018-03-27 [1]
#>  countrycode * 1.2.0       2020-05-22 [1]
#>  crayon        1.3.4.9000  2020-09-03 [1]
#>  DBI           1.1.0       2019-12-15 [1]
#>  dbplyr        1.4.4       2020-05-27 [1]
#>  desc          1.2.0       2018-05-01 [1]
#>  devtools      2.3.1.9000  2020-08-18 [1]
#>  digest        0.6.25      2020-02-23 [1]
#>  dplyr       * 1.0.2       2020-08-18 [1]
#>  ellipsis      0.3.1       2020-05-15 [1]
#>  evaluate      0.14        2019-05-28 [1]
#>  fansi         0.4.1       2020-01-08 [1]
#>  forcats     * 0.5.0       2020-03-01 [1]
#>  fs          * 1.5.0       2020-07-31 [1]
#>  fst         * 0.9.4       2020-08-27 [1]
#>  generics      0.0.2       2018-11-29 [1]
#>  ggplot2     * 3.3.2       2020-06-19 [1]
#>  glue        * 1.4.2       2020-08-27 [1]
#>  gtable        0.3.0       2019-03-25 [1]
#>  haven         2.3.1       2020-06-01 [1]
#>  here        * 0.1         2017-05-28 [1]
#>  hms           0.5.3       2020-01-08 [1]
#>  htmltools     0.5.0       2020-06-16 [1]
#>  httr          1.4.2       2020-07-20 [1]
#>  jsonlite    * 1.7.1       2020-09-07 [1]
#>  knitr         1.29        2020-06-23 [1]
#>  lifecycle     0.2.0       2020-03-06 [1]
#>  lubridate     1.7.9       2020-06-08 [1]
#>  magrittr      1.5.0.9000  2020-08-31 [1]
#>  memoise       1.1.0       2017-04-21 [1]
#>  modelr        0.1.8       2020-05-19 [1]
#>  munsell       0.5.0       2018-06-12 [1]
#>  pillar        1.4.6       2020-07-10 [1]
#>  pkgbuild      1.1.0       2020-07-13 [1]
#>  pkgconfig     2.0.3       2019-09-22 [1]
#>  pkgload       1.1.0       2020-05-29 [1]
#>  plyr          1.8.6       2020-03-03 [1]
#>  prettyunits   1.1.1       2020-01-24 [1]
#>  processx      3.4.4       2020-09-03 [1]
#>  ps            1.3.4       2020-08-11 [1]
#>  purrr       * 0.3.4       2020-04-17 [1]
#>  r2dii.utils * 0.0.0.9003  2020-05-14 [1]
#>  R6            2.4.1       2019-11-12 [1]
#>  Rcpp          1.0.5       2020-07-06 [1]
#>  readr       * 1.3.1       2018-12-21 [1]
#>  readxl      * 1.3.1       2019-03-13 [1]
#>  remotes       2.2.0       2020-07-21 [1]
#>  renv        * 0.12.0      2020-08-28 [1]
#>  reprex        0.3.0       2019-05-16 [1]
#>  reshape2    * 1.4.4       2020-04-09 [1]
#>  rlang       * 0.4.7       2020-07-09 [1]
#>  rmarkdown     2.3         2020-06-18 [1]
#>  rprojroot     1.3-2       2018-01-03 [1]
#>  rstudioapi    0.11        2020-02-07 [1]
#>  rvest         0.3.6       2020-07-25 [1]
#>  scales      * 1.1.1       2020-05-11 [1]
#>  sessioninfo   1.1.1       2018-11-05 [1]
#>  stringi       1.5.3       2020-09-09 [1]
#>  stringr     * 1.4.0       2019-02-10 [1]
#>  testthat    * 2.99.0.9000 2020-08-18 [1]
#>  tibble      * 3.0.3       2020-07-10 [1]
#>  tidyr       * 1.1.2       2020-08-27 [1]
#>  tidyselect  * 1.1.0       2020-05-11 [1]
#>  tidyverse   * 1.3.0       2019-11-21 [1]
#>  usethis       1.6.1       2020-04-29 [1]
#>  vctrs         0.3.4       2020-08-29 [1]
#>  withr         2.2.0       2020-04-20 [1]
#>  writexl     * 1.3.1       2020-08-26 [1]
#>  xfun          0.17        2020-09-09 [1]
#>  xml2          1.3.2       2020-04-23 [1]
#>  yaml          2.2.1       2020-02-01 [1]
#>  source                                        
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  Github (r-lib/crayon@6b3f0c6)                 
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  Github (r-lib/devtools@df619ce)               
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  Github (tidyverse/magrittr@15f6f07)           
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  Github (2DegreesInvesting/r2dii.utils@fd29303)
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  Github (r-lib/testthat@9e643d8)               
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#> 
#> [1] /home/mauro/R/x86_64-pc-linux-gnu-library/4.0
#> [2] /usr/local/lib/R/site-library
#> [3] /usr/lib/R/site-library
#> [4] /usr/lib/R/library
```

</details>

### Required data

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

### Required files-system structure

If they don’t already exist, create the required directories.

``` r
create_directory_if_it_doesnt_exist <- function(directory) {
  if (!fs::dir_exists(directory)) {
    fs::dir_create(directory)
  }
  
  invisible(directory)
}

children <- c("30_Processed_Inputs", "40_Results", "50_Outputs")
(paths <- here("working_dir", children))
#> [1] "/home/mauro/git/PACTA_analysis/working_dir/30_Processed_Inputs"
#> [2] "/home/mauro/git/PACTA_analysis/working_dir/40_Results"         
#> [3] "/home/mauro/git/PACTA_analysis/working_dir/50_Outputs"

walk(paths, create_directory_if_it_doesnt_exist)
```

Ensure the following repos are siblings of PACTA\_analysis/ (

  - 2DegreesInvesting/pacta-data
  - 2DegreesInvesting/create\_interactive\_report
  - 2DegreesInvesting/PACTA\_analysis

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

Ensure the repo PACTA\_analysis is on the branch
`current_web_functionality`, or a PR off it. This is not a test but a
visual inspection.

``` bash
git log --oneline --graph --no-merges upstream/current_web_functionality..HEAD
#> * e593f8e WIP include calls to expect_true in previous chunks
#> * 13fe0d2 WIP Write intro
#> * 7d4b106 WIP polish: create directories
#> * 8caa1af WIP polish
#> * 443217e Document dependencies
#> * d74885e WIP: extend try-web-tool.Rmd
#> * 7787647 WIP: New wip.R creates interactive report
#> * 8cf4935 WIP clean this commit: show example of create_interactive_report
#> * fda04ce Polish
#> * fc799b1 Polish: define required stuff at the top
#> * 779a9ef Cleanup my notes
#> * 23604e9 Simplify definition of working_location
#> * 8e5ed03 Cleanup
#> * aa517d6 Add notes
#> * 6784cbf Cleanup: Show rather than explain changes
#> * c453ebf Fix working_location
#> * 4ccdd59 Find a few more errors
#> * 73f2032 Find error in web_tool_script_1.R: line 34
#> * 809375b Remove horrible --hard reset
#> * 762dcf1 Style
#> * 78aa9c4 Try runnign web tool following Jacob's guide
#> * 10c80a7 Expect sibling repo
```

Ensure you are on the required working directory.

``` r
expect_equal(path_file(here()), "PACTA_analysis")
```

TODO: The branch `current_web_functionaliry` is long-lived, which makes
the workflow more complex than the standard GitHub workflow (by which
the only long-lived branch is `master`). I suggest merging
`current_web_functionality` into master, or extracting the
non-overlapping code into a separate repo.

### Ensure the value of `portfolio_name_ref_all` is “TestPortfolio\_Input”

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
#> portfolio_name_ref_all <- c("TestPortfolio_Input")
#> portfolio_name_ref_all <- c("TestPortfolio_Input")
#> portfolio_name_ref_all <- c("TestPortfolio_Input")

script_has_this_pattern <- grepl(this_pattern, matched)
expect_true(all(script_has_this_pattern))
```

TODO: If the value of `portfolio_name_ref_all` comes from the user, we
need an interface for the user to supply it – instead of asking a user
to change the source code.

### 3 Configuration files

Ensure the file “TestPortfolio\_Input\_PortfolioParameters.yml” exists.

``` r
config_1 <- here(
  "working_dir", 
  "10_Parameter_File", 
  "TestPortfolio_Input_PortfolioParameters.yml"
)

look_into <- function(path, n = -1L) {
  lines <- readLines(path, n, encoding = "UTF-8")
  writeLines(lines)
}

look_into(config_1)
#> default:
#>     parameters:
#>         portfolio_name_in: test_portfolio
#>         investor_name_in: Test

expect_true(file_exists(config_1))
```

Ensure this other configuration file also exists, and has correct paths:

``` r
config_2 <- here("parameter_files", "WebParameters_2dii.yml")
look_into(config_2)
#> default:
#>     paths:
#>         project_location_ext: ~/git/PACTA_analysis/
#>         data_location_ext: ~/git/pacta-data/2019Q4/
#>         template_location: ~/git/create_interactive_report/
#>     parameters:
#>         project_name: working_dir
#>         twodii_internal: FALSE
#>         new_data: FALSE
```

``` r
expect_true(file_exists(config_2))
```

``` r
config_paths <- config::get(file = config_2)$paths
str(config_paths)
#> List of 3
#>  $ project_location_ext: chr "~/git/PACTA_analysis/"
#>  $ data_location_ext   : chr "~/git/pacta-data/2019Q4/"
#>  $ template_location   : chr "~/git/create_interactive_report/"

all_paths_exist <- all(map_lgl(config_paths, dir_exists))
expect_true(all_paths_exist)
```

TODO: It would be nice to set these paths from a more flexible
interface, like the arguments of a function or the parameters of a
parametrized rmarkdown file.

### Run scripts

``` r
# Populate the directory "working\_dir/30\_Processed\_Inputs/"
source("web_tool_script_1.R")
#> Warning in read_file(paste0(file_location, "/fund_data.fst")): ~/git/pacta-data/
#> 2019Q4/cleaned_files/fund_data.fst does not exist
#> [1] "No Equity in portfolio"
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 30_Processed_Inputs/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 40_Results/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 50_Outputs/TestPortfolio_Input' already exists

#  Populate working_dir/40_Results/
source("web_tool_script_2.R")
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 30_Processed_Inputs/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 40_Results/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 50_Outputs/TestPortfolio_Input' already exists
#> [1] "1: Test"

# Feed previous results plus data from the pacta-data/ into
# `create_interactive_report()`.
source("web_tool_script_3.R") 
```

Ensure the directory “working\_dir/50\_Outputs/” is now populated with
some .css file, some .js file, the file “index.html”, and some .zip
file.

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

The output looks good\!

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
```

``` r
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

NOTES

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

  - I see duplicated code in the first lines of each script. For
    example, you may extract all calls to `library()` into a file
    packages.R; and you may also extract the repeated definitions of
    `working_location` (which I think should be `working_location <-
    here()`).
