---
title: "Integration test: Run the web tool"
date: "2020-10-08"
output: github_document
---

## Introduction

This document provides a reproducible example of how to run the so called "web tool". It is based on [these instructions](https://bit.ly/2RCRJn7). It is useful in three ways:

* To document this repository.
* To onboard contributors.
* As an integration test -- to test running this file produces the expected output. We do this both locally and on a continuous integration service (GitHub actions) with multiple platforms and versions of R.

## Environment



Packages used in this file:


```r
library(tidyverse)
library(devtools)
library(testthat)
library(config)
library(rlang)
library(renv)
library(glue)
library(fs)
library(here)
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr")
#> [conflicted] Removing existing preference
#> [conflicted] Will prefer dplyr::filter over any other package
conflicted::conflict_prefer("lag", "dplyr")
#> [conflicted] Removing existing preference
#> [conflicted] Will prefer dplyr::lag over any other package
```

All packages detected in the directory PACTA\_analysis:


```r
detect_packages <- function() {
  packages <- renv::dependencies()$Package
  sort(unique(packages))
}

detect_packages()
#> Finding R package dependencies ... Done!
#>  [1] "assertthat"     "base"           "config"         "conflicted"    
#>  [5] "countrycode"    "cowplot"        "devtools"       "dplyr"         
#>  [9] "extrafont"      "fs"             "fst"            "ggforce"       
#> [13] "ggmap"          "ggplot2"        "ggrepel"        "ggthemes"      
#> [17] "glue"           "grid"           "gridExtra"      "here"          
#> [21] "janitor"        "jsonlite"       "knitr"          "lme4"          
#> [25] "matrixStats"    "PACTA.analysis" "plyr"           "purrr"         
#> [29] "R"              "r2dii.utils"    "RColorBrewer"   "readr"         
#> [33] "readxl"         "renv"           "reshape2"       "rlang"         
#> [37] "rmarkdown"      "rstudioapi"     "rworldmap"      "scales"        
#> [41] "sitools"        "stringr"        "testthat"       "tidyr"         
#> [45] "tidyselect"     "tidyverse"      "tools"          "usethis"       
#> [49] "withr"          "xml2"
```

<details>

<summary>Session information</summary>


```r
devtools::session_info()
#> ─ Session info ─────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 4.0.2 (2020-06-22)
#>  os       Ubuntu 18.04.5 LTS          
#>  system   x86_64, linux-gnu           
#>  ui       RStudio                     
#>  language en_US:en                    
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       America/Chicago             
#>  date     2020-10-08                  
#> 
#> ─ Packages ─────────────────────────────────────────────────────────────
#>  ! package        * version     date       lib
#>    assertthat       0.2.1       2019-03-21 [1]
#>    backports        1.1.10      2020-09-15 [1]
#>    bitops           1.0-6       2013-08-17 [1]
#>    blob             1.2.1       2020-01-20 [1]
#>    boot             1.3-25      2020-04-26 [1]
#>    broom            0.7.1       2020-10-02 [1]
#>    callr            3.4.4       2020-09-07 [1]
#>    cellranger       1.1.0       2016-07-27 [1]
#>    cli              2.0.2       2020-02-28 [1]
#>    colorspace       1.4-1       2019-03-18 [1]
#>    config         * 0.3         2018-03-27 [1]
#>    conflicted     * 1.0.4       2019-06-21 [1]
#>    countrycode    * 1.2.0       2020-05-22 [1]
#>    cowplot        * 1.1.0       2020-09-08 [1]
#>    crayon           1.3.4.9000  2020-09-03 [1]
#>    data.table     * 1.13.0      2020-07-24 [1]
#>    DBI              1.1.0       2019-12-15 [1]
#>    dbplyr           1.4.4       2020-05-27 [1]
#>    desc             1.2.0       2018-05-01 [1]
#>    devtools       * 2.3.2       2020-09-18 [1]
#>    diffobj          0.3.2       2020-10-05 [1]
#>    digest           0.6.25      2020-02-23 [1]
#>    dotCall64        1.0-0       2018-07-30 [1]
#>    dplyr          * 1.0.2       2020-08-18 [1]
#>    ellipsis         0.3.1       2020-05-15 [1]
#>    evaluate         0.14        2019-05-28 [1]
#>    extrafont      * 0.17        2014-12-08 [1]
#>    extrafontdb      1.0         2012-06-11 [1]
#>    fansi            0.4.1       2020-01-08 [1]
#>    farver           2.0.3       2020-01-16 [1]
#>    fields           11.5        2020-10-04 [1]
#>    forcats        * 0.5.0       2020-03-01 [1]
#>    foreign          0.8-80      2020-05-24 [1]
#>    fs             * 1.5.0       2020-07-31 [1]
#>    fst            * 0.9.4       2020-08-27 [1]
#>    generics         0.0.2       2018-11-29 [1]
#>    ggforce        * 0.3.2       2020-06-23 [1]
#>    ggmap          * 3.0.0       2019-02-05 [1]
#>    ggplot2        * 3.3.2       2020-06-19 [1]
#>    ggrepel        * 0.8.2       2020-03-08 [1]
#>    ggthemes       * 4.2.0       2019-05-13 [1]
#>    glue           * 1.4.2       2020-08-27 [1]
#>    gridExtra      * 2.3         2017-09-09 [1]
#>    gtable           0.3.0       2019-03-25 [1]
#>    haven            2.3.1       2020-06-01 [1]
#>    here           * 0.1         2017-05-28 [1]
#>    hms              0.5.3       2020-01-08 [1]
#>    htmltools        0.5.0.9001  2020-10-08 [1]
#>    httr             1.4.2       2020-07-20 [1]
#>    janitor          2.0.1.9000  2020-05-14 [1]
#>    jpeg             0.1-8.1     2019-10-24 [1]
#>    jsonlite       * 1.7.1       2020-09-07 [1]
#>    knitr          * 1.30        2020-09-22 [1]
#>    lattice          0.20-41     2020-04-02 [1]
#>    lifecycle        0.2.0       2020-03-06 [1]
#>    lme4           * 1.1-23      2020-04-07 [1]
#>    lubridate        1.7.9       2020-06-08 [1]
#>    magrittr         1.5.0.9000  2020-09-28 [1]
#>    maps             3.3.0       2018-04-03 [1]
#>    maptools         1.0-2       2020-08-24 [1]
#>    MASS             7.3-53      2020-09-09 [1]
#>    Matrix         * 1.2-18      2019-11-27 [1]
#>    matrixStats    * 0.57.0      2020-09-25 [1]
#>    memoise          1.1.0       2017-04-21 [1]
#>    minqa            1.2.4       2014-10-09 [1]
#>    modelr           0.1.8       2020-05-19 [1]
#>    munsell          0.5.0       2018-06-12 [1]
#>    nlme             3.1-149     2020-08-23 [1]
#>    nloptr           1.2.2.2     2020-07-02 [1]
#>  P PACTA.analysis * 0.0.0.9000  2020-10-01 [?]
#>    pak            * 0.1.2       2019-02-19 [1]
#>    pillar           1.4.6       2020-07-10 [1]
#>    pkgbuild         1.1.0       2020-07-13 [1]
#>    pkgconfig        2.0.3       2019-09-22 [1]
#>    pkgdown        * 1.6.1       2020-09-12 [1]
#>    pkgload          1.1.0       2020-05-29 [1]
#>    plyr             1.8.6       2020-03-03 [1]
#>    png              0.1-7       2013-12-03 [1]
#>    polyclip         1.10-0      2019-03-14 [1]
#>    prettyunits      1.1.1       2020-01-24 [1]
#>    processx         3.4.4       2020-09-03 [1]
#>    ps               1.4.0       2020-10-07 [1]
#>    purrr          * 0.3.4       2020-04-17 [1]
#>    R.cache          0.14.0      2019-12-06 [1]
#>    R.methodsS3      1.8.1       2020-08-26 [1]
#>    R.oo             1.24.0      2020-08-26 [1]
#>    R.utils          2.10.1      2020-08-26 [1]
#>    r2dii.utils    * 0.0.0.9003  2020-05-14 [1]
#>    R6               2.4.1       2019-11-12 [1]
#>    RColorBrewer   * 1.1-2       2014-12-07 [1]
#>    Rcpp             1.0.5       2020-07-06 [1]
#>    readr          * 1.3.1       2018-12-21 [1]
#>    readxl         * 1.3.1       2019-03-13 [1]
#>    rematch2         2.1.2       2020-05-01 [1]
#>    remotes          2.2.0       2020-07-21 [1]
#>    renv           * 0.12.0      2020-08-28 [1]
#>    reprex         * 0.3.0       2019-05-16 [1]
#>    reshape2       * 1.4.4       2020-04-09 [1]
#>    RgoogleMaps      1.4.5.3     2020-02-12 [1]
#>    rjson            0.2.20      2018-06-08 [1]
#>    rlang          * 0.4.8       2020-10-08 [1]
#>    rmarkdown        2.4         2020-09-30 [1]
#>    rprojroot        1.3-2       2018-01-03 [1]
#>    rstudioapi       0.11        2020-02-07 [1]
#>    Rttf2pt1         1.3.8       2020-01-10 [1]
#>    rvest            0.3.6       2020-07-25 [1]
#>    rworldmap      * 1.3-6       2016-02-03 [1]
#>    scales         * 1.1.1       2020-05-11 [1]
#>    sessioninfo      1.1.1       2018-11-05 [1]
#>    sitools        * 1.4         2012-08-22 [1]
#>    snakecase        0.11.0      2019-05-25 [1]
#>    sp             * 1.4-2       2020-05-20 [1]
#>    spam             2.5-1       2019-12-12 [1]
#>    spelling       * 2.1.9000    2020-09-28 [1]
#>    statmod          1.4.34      2020-02-17 [1]
#>    stringi          1.5.3       2020-09-09 [1]
#>    stringr        * 1.4.0       2019-02-10 [1]
#>    styler           1.3.2       2020-02-23 [1]
#>    testthat       * 2.99.0.9000 2020-10-08 [1]
#>    tibble         * 3.0.3       2020-07-10 [1]
#>    tidyr          * 1.1.2       2020-08-27 [1]
#>    tidyselect     * 1.1.0       2020-05-11 [1]
#>    tidyverse      * 1.3.0       2019-11-21 [1]
#>    tweenr           1.0.1       2018-12-14 [1]
#>    usethis        * 1.6.3       2020-09-17 [1]
#>    vctrs            0.3.4       2020-08-29 [1]
#>    waldo            0.2.1       2020-10-08 [1]
#>    withr            2.3.0       2020-09-22 [1]
#>    writexl        * 1.3.1       2020-08-26 [1]
#>    xfun             0.18        2020-09-29 [1]
#>    xml2           * 1.3.2       2020-04-23 [1]
#>    yaml             2.2.1       2020-02-01 [1]
#>    zoo            * 1.8-8       2020-05-02 [1]
#>  source                                        
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  Github (r-lib/crayon@6b3f0c6)                 
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  Github (rstudio/htmltools@5d42d84)            
#>  RSPM (R 4.0.2)                                
#>  Github (sfirke/janitor@ecc9f4e)               
#>  RSPM (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  Github (tidyverse/magrittr@0221e18)           
#>  RSPM (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  local                                         
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  Github (2DegreesInvesting/r2dii.utils@fd29303)
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.2)                                
#>  RSPM (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  Github (ropensci/spelling@593e477)            
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  Github (r-lib/testthat@8c4b523)               
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.2)                                
#>  RSPM (R 4.0.2)                                
#>  RSPM (R 4.0.0)                                
#>  RSPM (R 4.0.2)                                
#>  CRAN (R 4.0.0)                                
#>  RSPM (R 4.0.0)                                
#>  CRAN (R 4.0.0)                                
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


```r
devtools::load_all()
#> Loading PACTA.analysis
```

## Data

Ensure the example data is available.


```r
file_name <- "TestPortfolio_Input.csv"
example_dataset <- here("sample_files", "20_input_files", file_name)

expect_true(file_exists(example_dataset))
```

Ensure the example data is copied into the expected directory.


```r
expected_dataset <- here("working_dir", "20_Raw_Inputs", file_name)

if (file_exists(expected_dataset)) {
  warn(glue("Removing existing file: {expected_dataset}"))
  file_delete(expected_dataset)
}
#> Warning: Removing existing file: /home/mauro/git/PACTA_analysis/
#> working_dir/20_Raw_Inputs/TestPortfolio_Input.csv

file_copy(example_dataset, expected_dataset)

expect_true(file_exists(expected_dataset))
```

## Directories

Ensure the required directories exist, and are empty.


```r
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

Ensure the following repos are siblings, i.e. they are inside the same parent directory:

* "2DegreesInvesting/pacta-data/"
* "2DegreesInvesting/create\_interactive_report/"
* "2DegreesInvesting/PACTA\_analysis/"
* "2DegreesInvesting/StressTestingModelDev/"


```r
is_sibling <- function(x) {
  parent <- path_dir(here())
  dir_exists(path(parent, x))
}

repos <- c("pacta-data", "create_interactive_report", "PACTA_analysis", "StressTestingModelDev")
all_siblings <- all(map_lgl(repos, is_sibling))

expect_true(all_siblings)
```

**NOTE: As of this writing the main line of development is not the standard branch `master` -- it is the branch `current_web_functionality`.**

Ensure the expected working directory.


```r
expect_equal(path_file(here()), "PACTA_analysis")
```

## `portfolio_name_ref_all <- "TestPortfolio_Input"`

Ensure `portfolio_name_ref_all` takes the value "TestPortfolio_Input" in the files which name contains "web\_tool\_scripts".


```r
# What value is currently assigned to the variable `portfolio_name_ref_all`?
show_pattern_in_file <- function(file, pattern) {
  grep(pattern, readLines(file), value = TRUE)
}

pattern <- "set_portfolio-name-ref-all_working-location_and_web-parameters.R"
(files <- dir_ls("dry", regexp = pattern))
#> dry/set_portfolio-name-ref-all_working-location_and_web-parameters.R

this_pattern <- "portfolio_name_ref_all.*<-"
matched <- map(files, show_pattern_in_file, pattern = this_pattern)
walk(matched, writeLines)
#>   portfolio_name_ref_all <- get_portfolio_name()
#>   portfolio_name_ref_all <- c("TestPortfolio_Input") # must be the same name as in the _PortfolioParameters.yml

script_has_this_pattern <- grepl(this_pattern, matched)
expect_true(any(script_has_this_pattern))
```

**NOTE: If the value of `portfolio_name_ref_all` comes from the user, we should provide an interface for the user to supply it -- so that the user needs not to change the source code.**

## Configurations

Ensure this configuration file exists:


```r
config_1 <- here(
  "working_dir",
  "10_Parameter_File",
  "TestPortfolio_Input_PortfolioParameters.yml"
)

expect_true(file_exists(config_1))
```


```r
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


```r
config_2 <- here("parameter_files", "WebParameters_2dii.yml")
config_2_copy <- tempfile()
fs::file_copy(config_2, config_2_copy)

expect_true(file_exists(config_2))
```

Ensure the paths in the configuration file work both locally and on GitHub actions:


```r
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
    root_field_path("template_location", pattern = "create_interactive_report") %>%
    root_field_path("stress_test_location", pattern = "StressTestingModelDev")
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


```r
config_paths <- config::get(file = config_2)$paths
all_paths_exist <- all(map_lgl(config_paths, dir_exists))

expect_true(all_paths_exist)
```


```r
look_into(config_2)
#> default:
#>     paths:
#>         project_location_ext: /home/mauro/git/PACTA_analysis/
#>         data_location_ext: /home/mauro/git/pacta-data/2019Q4/
#>         template_location: /home/mauro/git/create_interactive_report/
#>         stress_test_location: /home/mauro/git/StressTestingModelDev/
#>     parameters:
#>         project_name: working_dir
#>         twodii_internal: FALSE
#>         new_data: FALSE
```

## Scripts

Populate the directory for processed inputs:


```r
dir_has_files <- function(path) {
  stopifnot(is_dir(path))

  contents <- dir_ls(path, recurse = TRUE)
  has_files <- any(map_lgl(contents, is_file))
  has_files
}

out_1 <- path("working_dir", "30_Processed_Inputs")

expect_false(dir_has_files(out_1))
source("web_tool_script_1.R")
#> Warning: The `path` does not exist: {path}
#> [1] "No Equity in portfolio"
expect_true(dir_has_files(out_1))
```

Populate the directory for results:


```r
out_2 <- path("working_dir", "40_Results")

expect_false(dir_has_files(out_2))
source("web_tool_script_2.R")
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 30_Processed_Inputs/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 40_Results/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 50_Outputs/TestPortfolio_Input' already exists
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
#> [1] "No Equity Portfolio Data available. Skipping!"
#> [1] "Calculate Stress Test for Bonds Portfolio"
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.
#> Warning in data.table::melt(., id.vars = c("scenario_name",
#> "year_of_shock", : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
#> [1] "Integral method selected for calculation of late&sudden production scenarios. \n          Production shocks are function of scenarios (and possibly of company production plans if enabled), they will be calculated in function set_ls_trajectory"
#> [1] TRUE
#> Warning in data.table::dcast(., investor_name + portfolio_name + id
#> + company_name + : The dcast generic in data.table has been passed a
#> grouped_df and will attempt to redirect to the reshape2::dcast; please
#> note that reshape2 is deprecated, and this redirection is now deprecated
#> as well. Please do this redirection yourself like reshape2::dcast(.). In
#> the next version, this warning will become an error.

#> Warning in data.table::dcast(., investor_name + portfolio_name + id +
#> company_name + : The melt generic in data.table has been passed a tbl_df
#> and will attempt to redirect to the relevant reshape2 method; please note
#> that reshape2 is deprecated, and this redirection is now deprecated as
#> well. To continue using melt methods from reshape2 while both libraries
#> are attached, e.g. melt.list, you can prepend the namespace like
#> reshape2::melt(.). In the next version, this warning will become an error.
expect_true(dir_has_files(out_2))
```

Populate the directory for outputs:


```r
out_3 <- path("working_dir", "50_Outputs")

expect_false(dir_has_files(out_3))
source("web_tool_script_3.R")
expect_true(dir_has_files(out_3))
```

Ensure the output includes specific types of files:


```r
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


```r
look_into(index, n = 20L)
#> <!DOCTYPE html>
#> <html lang="" xml:lang="">
#> <head>
#> 
#>   <meta charset="utf-8" />
#>   <meta http-equiv="X-UA-Compatible" content="IE=edge" />
#>   <title>1 Introduction: What to get out of this report and how to read it | Interactive Portfolio Report</title>
#>   <meta name="description" content="" />
#>   <meta name="generator" content="bookdown 0.20 and GitBook 2.6.7" />
#> 
#>   <meta property="og:title" content="1 Introduction: What to get out of this report and how to read it | Interactive Portfolio Report" />
#>   <meta property="og:type" content="book" />
#>   
#>   
#>   
#>   
#> 
#>   <meta name="twitter:card" content="summary" />
#>   <meta name="twitter:title" content="1 Introduction: What to get out of this report and how to read it | Interactive Portfolio Report" />
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

* Some warnings may be avoided if required directories are created only if they don't already exist.

```r
Warning message:
In read_file(paste0(file_location, "/fund_data.fst")) :
  ~/git/pacta-data/2019Q4/cleaned_files/fund_data.fst does not exist
```

* I'm not sure if this dataset is crucial, but it's missing from my clone of pacta-data/:


```r
dir_ls(path("..", "pacta-data", "2019Q4", "cleaned_files"))
#> ../pacta-data/2019Q4/cleaned_files/average_sector_intensity.fst
#> ../pacta-data/2019Q4/cleaned_files/comp_fin_data.fst
#> ../pacta-data/2019Q4/cleaned_files/company_emissions.fst
#> ../pacta-data/2019Q4/cleaned_files/currencies.fst
#> ../pacta-data/2019Q4/cleaned_files/debt_fin_data.fst
#> ../pacta-data/2019Q4/cleaned_files/fin_data.fst
```

## Cleanup

Restore configuration file.


```r
fs::file_copy(config_2_copy, config_2, overwrite = TRUE)
```

