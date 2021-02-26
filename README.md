Mauro Lepore, 2021-02-26 16:15:57.

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

### System

The single-inputs workflow runs on a system defined by the Dockerfile in <https://github.com/2DegreesInvesting/docker/tree/master/system>. The multiple-inputs workflow currently lacks a formal definition of its system requirements. As a guide, see the computing environment for the single-inputs or search for relevant workflows for GitHub actions.

``` bash
ls .github/workflows
$ R-CMD-check.yaml
$ pr-commands.yaml
$ source-web-tool-scripts.yaml
```

### Siblings

PACTA\_analysis depends on other "siblings" of the PACTA family. They are all available at <https://github.com/2DegreesInvesting> and should live alongside PACTA\_analysis, under the same parent directory. Before any analysis, ensure all siblings are up to date.

``` bash
# Store
 working_directory="$pwd"
```

``` bash
siblings="\
  ../pacta-data \
  ../create_interactive_report \
  ../StressTestingModelDev"
for each_sibling in $siblings; do
  cd $each_sibling && echo $each_sibling
  git log --decorate --oneline -n 1
done
$ ../pacta-data
$ 13215f8 (HEAD -> master, tag: 0.0.14, origin/master) adding new peer files for Austria (#35)
$ ../create_interactive_report
$ aefc204 (HEAD -> master, tag: 0.0.14, origin/master) Merge pull request #380 from MonikaFu/375-format-scenario-names
$ ../StressTestingModelDev
$ 45f124b (HEAD -> master, tag: 0.0.14, origin/master) 211 litigation pf level (#220)
```

``` bash
# Restore
cd "$working_directory"
```

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
#> 
#> Attaching package: 'testthat'
#> The following object is masked from 'package:devtools':
#> 
#>     test_file
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
