Mauro Lepore, 2021-03-02 23:35:21.

<!-- README.md is generated from README.Rmd. Please edit that file -->
# PACTA.analysis

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

Note: Code chunks with prompts "`$`" and "`#>`" correspond to bash and R, respectively.

## Computing environment

The required computing environment is complex, yet precisely defined via a collection of [Docker images](https://github.com/2DegreesInvesting/docker/). You may experiment with your local environment but if anything fails you may want to "fix the problem" yourself. Often "the problem" is that your computing environment lacks some obscure dependency or subtle configuration and figuring this out may take hours or days. Instead you are better off using the Docker images we build for this purpose. Learning Docker takes some effort but in the long run it will save frustration and time.

[Install Docker](https://docs.docker.com/get-docker/) then the entire computing environment with:

``` bash
docker pull 2dii/r-packages
```

For an example on how to use Docker, consider how I created the document you are now reading:

-   I first moved into the parent directory of PACTA\_analysis/ and other siblings of the PACTA family.

``` bash
cd ~/git
```

-   Then I run (`docker run`) an ephemeral (`--rm`) interactive (`-ti`) container from the image `2dii/r-packages`, and created a volume (`-v`) to access everything in my local working directory (`$(pwd)`) from the `/home` directory inside the container.

``` bash
docker run --rm -ti -v $(pwd):/home 2dii/r-packages
```

-   Docker dropped me at the root directory of the container, so I moved into PACTA\_analysis/.

``` bash
cd /home/PACTA_analysis
```

-   I started R.

``` r
R
```

-   And finally I render the source file of the document you are now reading.

``` r
rmarkdown::render("README.Rmd")
```

<details> <summary>Session information</summary>

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
#>  date     2021-02-26                  
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

### System

The single-inputs workflow runs on a system defined by the Dockerfile in <https://github.com/2DegreesInvesting/docker/tree/master/system>. The multiple-inputs workflow currently lacks a formal definition of its system requirements. As a guide, see the computing environment for the single-inputs or search for relevant workflows for GitHub actions.

``` bash
ls .github/workflows
$ R-CMD-check.yaml
$ pr-commands.yaml
$ source-web-tool-scripts.yaml
```

### Siblings

PACTA\_analysis depends on other "siblings" of the PACTA family. They are all available at <https://github.com/2DegreesInvesting> and should live alongside PACTA\_analysis/, under the same parent directory. To make them easier to find, all siblings include an empty file called ".pacta"

``` bash
siblings="$(dirname $(find .. -maxdepth 2 -empty -type f -name '.pacta'))"
echo $siblings
$ ../pacta-data ../create_interactive_report ../StressTestingModelDev ../PACTA_analysis
```

Before any analysis, ensure all siblings are up to date.

``` bash
# Store
 working_directory="$pwd"

siblings="$(dirname $(find .. -maxdepth 2 -empty -type f -name '.pacta'))"
for i in $siblings; do
  cd $i && echo $i
  git log --decorate --oneline -n 1
done

# Restore
cd "$working_directory"

$ ../pacta-data
$ 13215f8 (HEAD -> master, tag: 0.0.14, origin/master) adding new peer files for Austria (#35)
$ ../create_interactive_report
$ aefc204 (HEAD -> master, tag: 0.0.14, origin/master) Merge pull request #380 from MonikaFu/375-format-scenario-names
$ ../StressTestingModelDev
$ 45f124b (HEAD -> master, tag: 0.0.14, origin/master) 211 litigation pf level (#220)
$ ../PACTA_analysis
$ a7f37cf (HEAD -> generalize-workflow) Date
```

For an easier way to work with all siblings at once from the terminal see [pacta-cli](https://github.com/2DegreesInvesting/pacta-cli).

### R packages

-   R packages used in this file:

``` r
suppressPackageStartupMessages({
  library(renv)
  library(devtools)
  library(conflicted)
})
```

-   R packages used in the directory PACTA\_analysis:

``` r
dependencies <- renv::dependencies(progress = FALSE)$Package
sort(unique(dependencies))
#>  [1] "PACTA.analysis" "R"              "assertthat"     "base"          
#>  [5] "bookdown"       "cli"            "config"         "conflicted"    
#>  [9] "countrycode"    "data.table"     "devtools"       "dplyr"         
#> [13] "fs"             "fst"            "ggplot2"        "gh"            
#> [17] "git2r"          "glue"           "here"           "janitor"       
#> [21] "jsonlite"       "knitr"          "magrittr"       "plyr"          
#> [25] "purrr"          "r2dii.utils"    "readr"          "readxl"        
#> [29] "renv"           "reshape2"       "rlang"          "rmarkdown"     
#> [33] "rstudioapi"     "scales"         "stringr"        "testthat"      
#> [37] "tibble"         "tidyr"          "tidyselect"     "tools"         
#> [41] "withr"          "writexl"        "yaml"           "zoo"
```

See also the Dockerfile in <https://github.com/2DegreesInvesting/docker/tree/master/r-packages>.

## Installation

To use the software in this and related repositories (more on this below) you need to work on developer mode. There is no "installation package"; instead you need to clone the source code [2DII's organization on GitHub](https://github.com/2DegreesInvesting/).

Notice that this repository does contain an R package.

``` r
show_package <- head(readLines("DESCRIPTION"), 3)
writeLines(show_package)
#> Package: PACTA.analysis
#> Title: Helpers for 'PACTA_analysis'
#> Version: 0.0.0.9000
```

However, it is only for convenience and must also be used in developer mode.

``` r
devtools::load_all()
#> Loading PACTA.analysis
```

## Reproducible examples

These examples show how to run each workflow. You may use them in a number of ways. For example, you may run them on your local computer to track and explore their implementation, or on a remote server as an integration test in a continuous integration pipeline.

### Single-inputs workflow

The single-inputs workflow has three steps, each in its own script.

``` bash
ls web_tool_script*
$ web_tool_script_1.R
$ web_tool_script_2.R
$ web_tool_script_3.R
```

With R, you can run each script individually with something like `source("web_tool_script_1.R")` or run multiple scripts at once with:

``` r
PACTA.analysis:::source_web_tool_scripts(1:2)
#> Testing: Rscript --vanilla web_tool_script_1.R TestPortfolio_Input
#> Testing: Rscript --vanilla web_tool_script_2.R TestPortfolio_Input
```

With the terminal, you can run each script with:

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
