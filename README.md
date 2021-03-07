2021-03-07 05:31:21.

<!-- README.md is generated from README.Rmd. Please edit that file -->

# PACTA\_analysis

The goal of this repository is to assess how well a porfolio aligns with
climate goals.

This documents targets internal 2DII users and developers. It provides
reproducible examples of what you can achieve with the code in this
repository and friends. You may use it as a guide to run your own
analyses, or as an integration test to ensure code changes preserve the
behaviour documented here. Readers outside 2DII may instead see other
related work:
[transitionmonitor.com](https://platform.transitionmonitor.com/start),
[r2dii.data](https://github.com/2DegreesInvesting/r2dii.data),
[r2dii.match](https://github.com/2DegreesInvesting/r2dii.match), and
[r2dii.analysis](https://github.com/2DegreesInvesting/r2dii.analysis).

You may want to analyze a single portfolio and investor, or multiple
ones. This document details two workflows:

-   **Single-inputs workflow**: When the number of portfolios and
    investors is exactly one, we use a specific set of scripts optimized
    for this purpose. This workflow is typically used to generate
    interactive reports. Because this workflow is available
    [online](https://platform.transitionmonitor.com/start), it has been
    referred to as “the online workflow”, and the scripts as “the web
    tool scripts”. This is misleading because this workflow can run
    offline too.
-   **Multiple-outputs workflow**: When the number of portfolios and/or
    investors is not exactly one, we use a different set of optimized
    scripts. This workflow is typically used to produce static (.pdf)
    reports. By contrast with the so-called “online” workflow, this one
    has been usually referred to as “the offline workflow”; again, this
    is misleading.

Both workflows include three steps:

1.  Clean portfolio: Clean the input portfolio and merge in financial
    data, to categorize each holding and identify whether it’s equity
    (EQ) or corporate bonds (CB).
2.  Run Analysis: Merge portfolios with asset-level data and scenarios,
    then group results at company, portfolio and regional level.
3.  Present results: Present the results in a clear output format.

–

Note that “`$`” and “`#>`” precede bash and R output, respectively.

## Setup

Let’s start by loading the packages we use in this file. Notice we
`load_all()` features in the PACTA.analysis package, which lives inside
the PACTA\_analysis repository.

``` r
library(devtools, warn.conflicts = FALSE)
#> Loading required package: usethis
load_all()
#> Loading PACTA.analysis
```

These helpers are for this document only:

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
  
  invisible(path)
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

## Siblings

The PACTA family includes these siblings: create\_interactive\_report,
PACTA\_analysis, pacta-data, StressTestingModelDev. They are all
available at <https://github.com/2DegreesInvesting> and some workflows
assume they all live directly under the same parent directory. Before
running a workflow, you may pull the latest commits of the relevant
siblings from Github, to ensure the code you use is up to date.

## Computing environment

The docker image [rocker/verse](https://hub.docker.com/r/rocker/verse)
provides most of what you need for a functional computing environment,
including system dependencies, a bash terminal and RStudio. The image
[2dii/pacta\_analysis](https://hub.docker.com/r/2dii/pacta_analysis)
extends rocker/verse to include the required R packages. Here it its
Dockerfile:

    FROM rocker/verse:latest
    MAINTAINER "Mauro Lepore" maurolepore@gmail.com

    COPY docker /docker
    RUN Rscript /docker/install.R

Finally you need access to the PACTA [siblings](#Siblings). You can
create one or more volumes mapping the location of the siblings from
your (host) computer to the container.

Here are two ways to access a full computing environment, including
PACTA siblings:

-   To work from with RStudio, ensure your working directory is
    PACTA\_analysis/, then run `docker-compose up`. RStudio is now
    available from your a browser at localhost:8787, with some personal
    configuration files and all siblings available at the home directory
    of the container. When you are finished using this service, run
    `docker-compose down`. The details of this environment are described
    in the file docker-compose.yml:

<!-- -->

    version: "0.0.0.9000"
    services:
      rstudio:
        image: 2dii/pacta_analysis:latest
        container_name: pacta_analysis
        ports:
          - 127.0.0.1:8787:8787
        environment:
        - PASSWORD=123
        - DISSABLE_AUTH=true
        - ROOT=TRUE
        - UMASK=0000
        volumes:
          # Access GitHub when 2FA is enabled (recommended)
          - '~/.ssh:/home/rstudio/.ssh'
          # Access secrets
          - '~/.Renviron:/home/rstudio/.Renviron'
          # Avoid re-configuring git
          - '~/.gitconfig:/home/rstudio/.gitconfig'
          # Access all siblings
          - '..:/home/rstudio'

-   To work from an interactive terminal, ensure your working directory
    is the parent of all PACTA siblings, then run
    `docker run --rm -ti -v "$(pwd):/root" 2dii/pacta_analysis:latest bash`.
    You are now into an ephemeral (`--rm`) docker container running an
    interactive `bash` terminal (`-it`) mapping (`-v`) the parent
    directory of all PACTA siblings to the /root directory.

To prove this environment works, I used this environment to render the
very document you are now reading with `devtools::build_readme()`.

<details>
<summary>
Session information.
</summary>

``` r
devtools::session_info()
#> ─ Session info ───────────────────────────────────────────────────────────────
#>  setting  value                       
#>  version  R version 4.0.4 (2021-02-15)
#>  os       Ubuntu 20.04 LTS            
#>  system   x86_64, linux-gnu           
#>  ui       X11                         
#>  language (EN)                        
#>  collate  en_US.UTF-8                 
#>  ctype    en_US.UTF-8                 
#>  tz       Etc/UTC                     
#>  date     2021-03-07                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  ! package        * version    date       lib source        
#>    assertthat       0.2.1      2019-03-21 [2] RSPM (R 4.0.3)
#>    cachem           1.0.4      2021-02-13 [2] RSPM (R 4.0.3)
#>    callr            3.5.1      2020-10-13 [2] RSPM (R 4.0.3)
#>    cli              2.3.0      2021-01-31 [2] RSPM (R 4.0.3)
#>    codetools        0.2-18     2020-11-04 [3] CRAN (R 4.0.4)
#>    config           0.3.1      2020-12-17 [2] RSPM (R 4.0.3)
#>    conflicted       1.0.4      2019-06-21 [2] RSPM (R 4.0.0)
#>    crayon           1.4.1      2021-02-08 [2] RSPM (R 4.0.3)
#>    DBI              1.1.1      2021-01-15 [2] RSPM (R 4.0.3)
#>    desc             1.2.0      2018-05-01 [2] RSPM (R 4.0.3)
#>    devtools       * 2.3.2      2020-09-18 [2] RSPM (R 4.0.3)
#>    digest           0.6.27     2020-10-24 [2] RSPM (R 4.0.3)
#>    dplyr            1.0.4      2021-02-02 [2] RSPM (R 4.0.3)
#>    ellipsis         0.3.1      2020-05-15 [2] RSPM (R 4.0.3)
#>    evaluate         0.14       2019-05-28 [2] RSPM (R 4.0.3)
#>    fansi            0.4.2      2021-01-15 [2] RSPM (R 4.0.3)
#>    fastmap          1.1.0      2021-01-25 [2] RSPM (R 4.0.3)
#>    fs               1.5.0      2020-07-31 [2] RSPM (R 4.0.3)
#>    fst              0.9.4      2020-08-27 [2] RSPM (R 4.0.3)
#>    generics         0.1.0      2020-10-31 [2] RSPM (R 4.0.3)
#>    glue             1.4.2      2020-08-27 [2] RSPM (R 4.0.3)
#>    here             1.0.1      2020-12-13 [2] RSPM (R 4.0.3)
#>    hms              1.0.0      2021-01-13 [2] RSPM (R 4.0.3)
#>    htmltools        0.5.1.1    2021-01-22 [2] RSPM (R 4.0.3)
#>    janitor          2.1.0      2021-01-05 [2] RSPM (R 4.0.3)
#>    knitr            1.31       2021-01-27 [2] RSPM (R 4.0.3)
#>    lifecycle        1.0.0      2021-02-15 [2] RSPM (R 4.0.3)
#>    lubridate        1.7.9.2    2020-11-13 [2] RSPM (R 4.0.3)
#>    magrittr         2.0.1      2020-11-17 [2] RSPM (R 4.0.3)
#>    memoise          2.0.0      2021-01-26 [2] RSPM (R 4.0.3)
#>  P PACTA.analysis * 0.0.0.9000 2021-03-07 [?] local         
#>    pillar           1.5.0      2021-02-22 [2] RSPM (R 4.0.3)
#>    pkgbuild         1.2.0      2020-12-15 [2] RSPM (R 4.0.3)
#>    pkgconfig        2.0.3      2019-09-22 [2] RSPM (R 4.0.3)
#>    pkgload          1.1.0      2020-05-29 [2] RSPM (R 4.0.3)
#>    prettyunits      1.1.1      2020-01-24 [2] RSPM (R 4.0.3)
#>    processx         3.4.5      2020-11-30 [2] RSPM (R 4.0.3)
#>    ps               1.5.0      2020-12-05 [2] RSPM (R 4.0.3)
#>    purrr            0.3.4      2020-04-17 [2] RSPM (R 4.0.3)
#>    R6               2.5.0      2020-10-28 [2] RSPM (R 4.0.3)
#>    Rcpp             1.0.6      2021-01-15 [2] RSPM (R 4.0.3)
#>    readr            1.4.0      2020-10-05 [2] RSPM (R 4.0.3)
#>    remotes          2.2.0      2020-07-21 [2] RSPM (R 4.0.3)
#>    renv             0.13.0     2021-02-24 [2] RSPM (R 4.0.3)
#>    rlang            0.4.10     2020-12-30 [2] RSPM (R 4.0.3)
#>    rmarkdown        2.7        2021-02-19 [2] RSPM (R 4.0.3)
#>    rprojroot        2.0.2      2020-11-15 [2] RSPM (R 4.0.3)
#>    rstudioapi       0.13       2020-11-12 [2] RSPM (R 4.0.3)
#>    sessioninfo      1.1.1      2018-11-05 [2] RSPM (R 4.0.3)
#>    snakecase        0.11.0     2019-05-25 [2] RSPM (R 4.0.3)
#>    stringi          1.5.3      2020-09-09 [2] RSPM (R 4.0.3)
#>    stringr          1.4.0      2019-02-10 [2] RSPM (R 4.0.3)
#>    testthat       * 3.0.2      2021-02-14 [2] RSPM (R 4.0.3)
#>    tibble           3.1.0      2021-02-25 [2] RSPM (R 4.0.3)
#>    tidyselect       1.1.0      2020-05-11 [2] RSPM (R 4.0.3)
#>    usethis        * 2.0.1      2021-02-10 [2] RSPM (R 4.0.3)
#>    utf8             1.1.4      2018-05-24 [2] RSPM (R 4.0.3)
#>    vctrs            0.3.6      2020-12-17 [2] RSPM (R 4.0.3)
#>    withr            2.4.1      2021-01-26 [2] RSPM (R 4.0.3)
#>    xfun             0.21       2021-02-10 [2] RSPM (R 4.0.3)
#>    yaml             2.2.1      2020-02-01 [2] RSPM (R 4.0.3)
#> 
#> [1] /tmp/RtmpIqK3Xs/temp_libpath15b4ffce3d9
#> [2] /usr/local/lib/R/site-library
#> [3] /usr/local/lib/R/library
#> 
#>  P ── Loaded and on-disk path mismatch.
```

</details>

–

Reference:

-   [2DegreesInvesting/docker](https://github.com/2DegreesInvesting/docker/tree/master/r-packages).
-   [Install Docker](https://docs.docker.com/engine/install/).
-   [Install docker-compose](https://docs.docker.com/compose/install/).

## Reproducible examples of PACTA workflows

This section shows reproducible examples of both the single- and
multiple-inputs workflows. You may use these examples in at least three
ways:

-   Read this document for an overview of which workflows are possible.
-   Run them on your local computer to track and explore their
    implementation.
-   Run them on a server as an integration test.

### Single-inputs workflow

TODO: Document inputs/outputs

> There’s a pretty important absence here… any explanation of where the
> critically important input files are or need to be. Hypothetically,
> the repo should work because it includes some default example files,
> but any analyst truly using this repo… the first thing they would want
> to do is put their input files in working\_dir – CJ

The single-inputs workflow is composed of multiple steps, each in its
own script: web\_tool\_script\_1.R, web\_tool\_script\_2.R,
web\_tool\_script\_3.R.

In R you can run each script individually with something like
`source("web_tool_script_1.R")` or run multiple scripts at once with:

``` r
# Helper from the package PACTA.analysis
source_web_tool_scripts()
#> Testing: Rscript --vanilla web_tool_script_1.R TestPortfolio_Input
#> Testing: Rscript --vanilla web_tool_script_2.R TestPortfolio_Input
#> Testing: Rscript --vanilla web_tool_script_3.R TestPortfolio_Input
```

In a terminal you can run each script individually,
e.g. “web\_tool\_script\_1.R”, with:

``` bash
Rscript --vanilla web_tool_script_1.R TestPortfolio_Input  
$ 
$ ── web_tool_script_1.R ─────────────────────────────────────────────────────────
$ Warning message:
$ In if (!is.na(total_fund_list)) { :
$   the condition has length > 1 and only the first element will be used
$ Joining, by = "holding_id"
$ Joining, by = "holding_id"
$ Joining, by = "holding_id"
```

### Multiple-inputs workflow

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
  
  invisible(path)
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
#> here() starts at /home/rstudio/PACTA_analysis
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
#> /home/rstudio/PACTA_analysis/parameter_files/ProjectParameters_GENERAL.yml

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
#> /home/rstudio/PACTA_analysis/sample_files/20_input_files/TestPortfolio_Input.csv
#> /home/rstudio/PACTA_analysis/working_dir/20_Raw_Inputs/TestPortfolio_Input.csv
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
