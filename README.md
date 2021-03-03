2021-03-03 21:13:05.

<!-- README.md is generated from README.Rmd. Please edit that file -->
# PACTA\_analysis

The goal of this repository is to assess how well a porfolio aligns with climate goals.

This documents targets internal 2DII users and developers. It provides reproducible examples of what you can achieve with the code in this repository and friends. You may use it as a guide to run your own analyses, or as an integration test to ensure code changes preserve the behavior documented here. Readers outside 2DII may instead see other related work: [transitionmonitor.com](https://platform.transitionmonitor.com/start), [r2dii.data](https://github.com/2DegreesInvesting/r2dii.data), [r2dii.match](https://github.com/2DegreesInvesting/r2dii.match), and [r2dii.analysis](https://github.com/2DegreesInvesting/r2dii.analysis).

You may want to analyze a single portfolio and investor, or multiple ones. This document details two workflows:

-   **Single-inputs workflow**: When the number of portfolios and investors is exactly one, we use a specific set of scripts optimized for this purpose. This workflow is typically used to generate interactive reports. Because this workflow is available [online](https://platform.transitionmonitor.com/start), it has been referred to as "the online workflow", and the scripts as "the web tool scripts". This is misleading because this workflow can run offline too.
-   **Multiple-outputs workflow**: When the number of portfolios and/or investors is not exactly one, we use a different set of optimized scripts. This workflow is typically used to produce static (.pdf) reports. By contrast with the so-called "online" workflow, this one has been usually referred to as "the offline workflow"; again, this is misleading.

Both workflows include three steps:

1.  Clean portfolio: Clean the input portfolio and merge in financial data, to categorize each holding and identify whether it's equity (EQ) or corporate bonds (CB).
2.  Run Analysis: Merge portfolios with asset-level data and scenarios, then group results at company, portfolio and regional level.
3.  Present results: Present the results in a clear output format.

--

Note that "`$`" and "`#>`" precede bash and R output, respectively.

### Siblings

The PACTA family includes these siblings: PACTA\_analysis, StressTestingModelDev, create\_interactive\_report, pacta-data. They are all available at <https://github.com/2DegreesInvesting> and some workflows assume they all live directly under the same parent directory. Before running a workflow, you may pull the latest commits of the relevant siblings from Github, to ensure the code you use is up to date.

## Computing environment

A functional computing environment is available in [this docker image](https://github.com/2DegreesInvesting/docker/tree/master/r-packages) (how to [install Docker](https://docs.docker.com/engine/install/)).

<details> <summary>Session information.</summary>

``` r
devtools::session_info()
#> - Session info ---------------------------------------------------------------
#>  setting  value                       
#>  version  R version 4.0.3 (2020-10-10)
#>  os       Ubuntu 18.04.5 LTS          
#>  system   x86_64, linux-gnu           
#>  ui       X11                         
#>  language (EN)                        
#>  collate  C                           
#>  ctype    C                           
#>  tz       Europe/Berlin               
#>  date     2021-03-03                  
#> 
#> - Packages -------------------------------------------------------------------
#>  ! package        * version    date       lib source        
#>    assertthat     * 0.2.1      2019-03-21 [1] RSPM (R 4.0.0)
#>    backports        1.2.0      2020-11-02 [1] RSPM (R 4.0.3)
#>    bookdown       * 0.21       2020-10-13 [1] RSPM (R 4.0.2)
#>    callr            3.5.1      2020-10-13 [1] RSPM (R 4.0.2)
#>    cellranger       1.1.0      2016-07-27 [1] RSPM (R 4.0.0)
#>    cli              2.1.0      2020-10-12 [1] RSPM (R 4.0.2)
#>    colorspace       1.4-1      2019-03-18 [1] RSPM (R 4.0.0)
#>    config         * 0.3        2018-03-27 [1] RSPM (R 4.0.0)
#>    conflicted     * 1.0.4      2019-06-21 [1] RSPM (R 4.0.0)
#>    countrycode    * 1.2.0      2020-05-22 [1] RSPM (R 4.0.0)
#>    crayon           1.3.4      2017-09-16 [1] RSPM (R 4.0.0)
#>    data.table     * 1.13.2     2020-10-19 [1] RSPM (R 4.0.3)
#>    desc             1.2.0      2018-05-01 [1] RSPM (R 4.0.0)
#>    devtools       * 2.3.2      2020-09-18 [1] RSPM (R 4.0.2)
#>    digest           0.6.27     2020-10-24 [1] RSPM (R 4.0.3)
#>    dplyr          * 1.0.2      2020-08-18 [1] RSPM (R 4.0.2)
#>    ellipsis         0.3.1      2020-05-15 [1] RSPM (R 4.0.0)
#>    evaluate         0.14       2019-05-28 [1] RSPM (R 4.0.0)
#>    fansi            0.4.1      2020-01-08 [1] RSPM (R 4.0.0)
#>    fs             * 1.5.0      2020-07-31 [1] RSPM (R 4.0.2)
#>    fst            * 0.9.4      2020-08-27 [1] RSPM (R 4.0.2)
#>    generics         0.1.0      2020-10-31 [1] RSPM (R 4.0.3)
#>    ggplot2        * 3.3.2      2020-06-19 [1] RSPM (R 4.0.1)
#>    glue           * 1.4.2      2020-08-27 [1] RSPM (R 4.0.2)
#>    gtable           0.3.0      2019-03-25 [1] RSPM (R 4.0.0)
#>    here           * 0.1        2017-05-28 [1] RSPM (R 4.0.0)
#>    hms              0.5.3      2020-01-08 [1] RSPM (R 4.0.0)
#>    htmltools        0.5.0      2020-06-16 [1] RSPM (R 4.0.1)
#>    janitor        * 2.0.1      2020-04-12 [1] RSPM (R 4.0.0)
#>    jsonlite       * 1.7.1      2020-09-07 [1] RSPM (R 4.0.2)
#>    knitr          * 1.30       2020-09-22 [1] RSPM (R 4.0.2)
#>    lattice          0.20-41    2020-04-02 [3] CRAN (R 4.0.0)
#>    lifecycle        0.2.0      2020-03-06 [1] RSPM (R 4.0.0)
#>    lubridate        1.7.9      2020-06-08 [1] RSPM (R 4.0.2)
#>    magrittr         1.5        2014-11-22 [1] RSPM (R 4.0.0)
#>    memoise          1.1.0      2017-04-21 [1] RSPM (R 4.0.0)
#>    munsell          0.5.0      2018-06-12 [1] RSPM (R 4.0.0)
#>  R PACTA.analysis * 0.0.0.9000 <NA>       [?] <NA>          
#>    pillar           1.4.6      2020-07-10 [1] RSPM (R 4.0.2)
#>    pkgbuild         1.1.0      2020-07-13 [1] RSPM (R 4.0.2)
#>    pkgconfig        2.0.3      2019-09-22 [1] RSPM (R 4.0.0)
#>    pkgload          1.1.0      2020-05-29 [1] RSPM (R 4.0.0)
#>    plyr             1.8.6      2020-03-03 [1] RSPM (R 4.0.2)
#>    prettyunits      1.1.1      2020-01-24 [1] RSPM (R 4.0.0)
#>    processx         3.4.4      2020-09-03 [1] RSPM (R 4.0.2)
#>    ps               1.4.0      2020-10-07 [1] RSPM (R 4.0.2)
#>    purrr          * 0.3.4      2020-04-17 [1] RSPM (R 4.0.0)
#>    R6               2.5.0      2020-10-28 [1] RSPM (R 4.0.3)
#>    Rcpp             1.0.5      2020-07-06 [1] RSPM (R 4.0.2)
#>    readr          * 1.4.0      2020-10-05 [1] RSPM (R 4.0.2)
#>    readxl         * 1.3.1      2019-03-13 [1] RSPM (R 4.0.2)
#>    remotes          2.2.0      2020-07-21 [1] RSPM (R 4.0.2)
#>    renv           * 0.12.1     2020-11-02 [1] RSPM (R 4.0.3)
#>    reshape2       * 1.4.4      2020-04-09 [1] RSPM (R 4.0.2)
#>    rlang          * 0.4.8      2020-10-08 [1] RSPM (R 4.0.2)
#>    rmarkdown      * 2.5        2020-10-21 [1] RSPM (R 4.0.3)
#>    rprojroot        1.3-2      2018-01-03 [1] RSPM (R 4.0.0)
#>    rstudioapi     * 0.11       2020-02-07 [1] RSPM (R 4.0.0)
#>    scales         * 1.1.1      2020-05-11 [1] RSPM (R 4.0.0)
#>    sessioninfo      1.1.1      2018-11-05 [1] RSPM (R 4.0.0)
#>    snakecase        0.11.0     2019-05-25 [1] RSPM (R 4.0.0)
#>    stringi          1.5.3      2020-09-09 [1] RSPM (R 4.0.2)
#>    stringr        * 1.4.0      2019-02-10 [1] RSPM (R 4.0.0)
#>    testthat       * 3.0.0      2020-10-31 [1] RSPM (R 4.0.3)
#>    tibble         * 3.0.4      2020-10-12 [1] RSPM (R 4.0.2)
#>    tidyr          * 1.1.2      2020-08-27 [1] RSPM (R 4.0.2)
#>    tidyselect     * 1.1.0      2020-05-11 [1] RSPM (R 4.0.0)
#>    usethis        * 1.6.3      2020-09-17 [1] RSPM (R 4.0.2)
#>    vctrs            0.3.4      2020-08-29 [1] RSPM (R 4.0.2)
#>    withr          * 2.3.0      2020-09-22 [1] RSPM (R 4.0.2)
#>    writexl        * 1.3.1      2020-08-26 [1] RSPM (R 4.0.2)
#>    xfun             0.19       2020-10-30 [1] RSPM (R 4.0.3)
#>    yaml             2.2.1      2020-02-01 [1] RSPM (R 4.0.0)
#>    zoo            * 1.8-8      2020-05-02 [1] RSPM (R 4.0.0)
#> 
#> [1] /usr/local/lib/R/site-library
#> [2] /usr/lib/R/site-library
#> [3] /usr/lib/R/library
#> 
#>  R -- Package was removed from disk.
```

</details>

As an example, this is one way to use that computing environment to produce the very document you are now reading:

-   I move to the parent directory of PACTA\_analysis and its siblings -- which in my case is "~/git/".

``` bash
cd ~/git
```

-   I run (`docker run`) an ephemeral (`--rm`), interactive (`-ti`) container of the image `2dii/r-packages`; create a number of volumes (`-v`) to access useful files in my host computer from inside the container; and set the working directory (`-w`) to PACTA\_analysis/.

<!-- -->


    docker run --rm -ti \
        -v "$(pwd)":"/root/$(basename $(pwd))" \
        -v "$HOME/.ssh":"/root/.ssh" \
        -v "$HOME/.profile":"/root/.profile" \
        -v "$HOME/.bashrc":"/root/.bashrc" \
        -v "$HOME/.Renviron":"/root/.Renviron" \
        -w "/root/$(basename $(pwd))/PACTA_analysis" \
        2dii/r-packages

-   Then I use R to produce the very file you are now reading.

``` r
R
rmarkdown::render("README.Rmd", output_format = "md_document")
```

## Installation

To use the software in this and related repositories (more on this below) you need to clone the source code from [2DII's organization on GitHub](https://github.com/2DegreesInvesting/).

The PACTA\_analysis repository contains an R package called PACTA.analysis (the name of an R package can't include "\_"). Use it from inside PACTA\_analysis with:

``` r
devtools::load_all()`
```

## Reproducible examples

This section shows reproducible examples of both the single- and multiple-inputs workflows. You may use these examples in at least three ways:

-   Read this document for an overview of which workflows are possible.
-   Run them on your local computer to track and explore their implementation.
-   Run them on a server as an integration test.

### Single-inputs workflow

TODO: Document inputs/outputs

> There's a pretty important absence here... any explanation of where the critically important input files are or need to be. Hypothetically, the repo should work because it includes some default example files, but any analyst truly using this repo... the first thing they would want to do is put their input files in working\_dir -- CJ

The single-inputs workflow is composed of multiple steps, each in its own script: web\_tool\_script\_1.R, web\_tool\_script\_2.R, web\_tool\_script\_3.R.

With R, you can run each script individually with something like `source("web_tool_script_1.R")` or run multiple scripts at once. For example, you can run scripts 1 and 2 with:

``` r
# Helper from the package PACTA.analysis
source_web_tool_scripts(1:2)
#> Testing: Rscript --vanilla web_tool_script_1.R TestPortfolio_Input
#> Testing: Rscript --vanilla web_tool_script_2.R TestPortfolio_Input
```

With the terminal, you can run each script individually. For example, you can run script 3 with:

``` bash
Rscript --vanilla web_tool_script_3.R TestPortfolio_Input  
$ 
$ -- web_tool_script_3.R ---------------------------------------------------------
$ Warning messages:
$ 1: Removed 1 rows containing missing values (geom_segment). 
$ 2: Removed 1 rows containing missing values (geom_point). 
$ 3: Removed 1 rows containing missing values (geom_point). 
$ 4: Removed 1 rows containing missing values (geom_point). 
$ 5: Removed 7 rows containing missing values (geom_point). 
$ 6: LaTeX Warning: Command \underbar  has changed.
$                Check if current package is valid.
$ LaTeX Warning: Command \underline  has changed.
$                Check if current package is valid.
$ Package Fancyhdr Warning: \fancyfoot's `E' option without twoside option is use
$ less on input line 89.
$ Package Fancyhdr Warning: \headheight is too small (12.0pt): 
$ Make it at least 55.4097pt.
$ We now make it that large for the rest of the document.
$ This may cause the page layout to be inconsistent, however.
```

### Multiple-inputs workflow

TODO
