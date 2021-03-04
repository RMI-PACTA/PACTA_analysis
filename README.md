<!-- README.md is generated from README.Rmd. Please edit that file -->
PACTA\_analysis
===============

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
    referred to as "the online workflow", and the scripts as "the web
    tool scripts". This is misleading because this workflow can run
    offline too.
-   **Multiple-outputs workflow**: When the number of portfolios and/or
    investors is not exactly one, we use a different set of optimized
    scripts. This workflow is typically used to produce static (.pdf)
    reports. By contrast with the so-called "online" workflow, this one
    has been usually referred to as "the offline workflow"; again, this
    is misleading.

Both workflows include three steps:

1.  Clean portfolio: Clean the input portfolio and merge in financial
    data, to categorize each holding and identify whether it's equity
    (EQ) or corporate bonds (CB).
2.  Run Analysis: Merge portfolios with asset-level data and scenarios,
    then group results at company, portfolio and regional level.
3.  Present results: Present the results in a clear output format.

--

Note that "`$`" and "`#>`" precede bash and R output, respectively.

Siblings
--------

The PACTA family includes these siblings: PACTA\_analysis,
StressTestingModelDev, create\_interactive\_report, pacta-data. They are
all available at <https://github.com/2DegreesInvesting> and some
workflows assume they all live directly under the same parent directory.
Before running a workflow, you may pull the latest commits of the
relevant siblings from Github, to ensure the code you use is up to date.

Computing environment
---------------------

A functional computing environment is available in [this Docker
image](https://github.com/2DegreesInvesting/docker/tree/master/r-packages)
(how to [install Docker](https://docs.docker.com/engine/install/)). For
an example using that image let's render the document you are now
reading, using all PACTA siblings, and personal configuration files from
the home directory in the host computer (my laptop). The entire
computing environment is captured in the file "docker-compose.yml":

    version: "0.0.0.9000"
    services:
      pacta:
        image: 2dii/r-packages
        working_dir: /root/PACTA_analysis
        volumes:
          # The ssh protocol is required when 2FA is enabled (recommended)
        - ~/.ssh:/root/.ssh
          # Access GITHUB_PAT and other personal secrets
        - ~/.Renviron:/root/.Renviron
          # Avoid re-configuring git
        - ~/.gitconfig:/root/.gitconfig
          # Read/write PACTA siblings in the host from the container
        - ../PACTA_analysis:/root/PACTA_analysis
        - ../pacta-data:/root/pacta-data
        - ../create_interactive_report:/root/create_interactive_report
        - ../StressTestingModelDev:/root/StressTestingModelDev

We can now `run` an ephemeral (`--rm`) container of the `pacta` service
with `docker-compose` (how to [install
docker-compose](https://docs.docker.com/compose/install/)):

    docker-compose run --rm pacta

From the container's `working_dir`ectory we now start an `R` session and
`render()` the file you are now reading:

    R
    rmarkdown::render("README.Rmd", output_format = "md_document")

<details> <summary>Session information.</summary>

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
    #>  date     2021-03-04                  
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  ! package        * version    date       lib source        
    #>    assertthat       0.2.1      2019-03-21 [1] RSPM (R 4.0.0)
    #>    cachem           1.0.1      2021-01-21 [1] RSPM (R 4.0.3)
    #>    callr            3.5.1      2020-10-13 [1] RSPM (R 4.0.2)
    #>    cli              2.2.0      2020-11-20 [1] RSPM (R 4.0.3)
    #>    codetools        0.2-18     2020-11-04 [3] CRAN (R 4.0.3)
    #>    config           0.3.1      2020-12-17 [1] RSPM (R 4.0.3)
    #>    conflicted       1.0.4      2019-06-21 [1] RSPM (R 4.0.0)
    #>    crayon           1.3.4      2017-09-16 [1] RSPM (R 4.0.0)
    #>    desc             1.2.0      2018-05-01 [1] RSPM (R 4.0.0)
    #>    devtools         2.3.2      2020-09-18 [1] RSPM (R 4.0.2)
    #>    digest           0.6.27     2020-10-24 [1] RSPM (R 4.0.3)
    #>    dplyr            1.0.3      2021-01-15 [1] RSPM (R 4.0.3)
    #>    ellipsis         0.3.1      2020-05-15 [1] RSPM (R 4.0.0)
    #>    evaluate         0.14       2019-05-28 [1] RSPM (R 4.0.0)
    #>    fansi            0.4.2      2021-01-15 [1] RSPM (R 4.0.3)
    #>    fastmap          1.1.0      2021-01-25 [1] RSPM (R 4.0.3)
    #>    fs               1.5.0      2020-07-31 [1] RSPM (R 4.0.2)
    #>    fst              0.9.4      2020-08-27 [1] RSPM (R 4.0.3)
    #>    generics         0.1.0      2020-10-31 [1] RSPM (R 4.0.3)
    #>    glue             1.4.2      2020-08-27 [1] RSPM (R 4.0.2)
    #>    here             1.0.1      2020-12-13 [1] RSPM (R 4.0.3)
    #>    hms              1.0.0      2021-01-13 [1] RSPM (R 4.0.3)
    #>    htmltools        0.5.1.1    2021-01-22 [1] RSPM (R 4.0.3)
    #>    janitor          2.1.0      2021-01-05 [1] RSPM (R 4.0.3)
    #>    knitr            1.30       2020-09-22 [1] RSPM (R 4.0.2)
    #>    lifecycle        0.2.0      2020-03-06 [1] RSPM (R 4.0.0)
    #>    lubridate        1.7.9.2    2020-11-13 [1] RSPM (R 4.0.3)
    #>    magrittr         2.0.1      2020-11-17 [1] RSPM (R 4.0.3)
    #>    memoise          2.0.0      2021-01-26 [1] RSPM (R 4.0.3)
    #>  R PACTA.analysis * 0.0.0.9000 <NA>       [?] <NA>          
    #>    pillar           1.4.7      2020-11-20 [1] RSPM (R 4.0.3)
    #>    pkgbuild         1.2.0      2020-12-15 [1] RSPM (R 4.0.3)
    #>    pkgconfig        2.0.3      2019-09-22 [1] RSPM (R 4.0.0)
    #>    pkgload          1.1.0      2020-05-29 [1] RSPM (R 4.0.0)
    #>    prettyunits      1.1.1      2020-01-24 [1] RSPM (R 4.0.0)
    #>    processx         3.4.5      2020-11-30 [1] RSPM (R 4.0.3)
    #>    ps               1.5.0      2020-12-05 [1] RSPM (R 4.0.3)
    #>    purrr            0.3.4      2020-04-17 [1] RSPM (R 4.0.0)
    #>    R6               2.5.0      2020-10-28 [1] RSPM (R 4.0.3)
    #>    Rcpp             1.0.6      2021-01-15 [1] RSPM (R 4.0.3)
    #>    readr            1.4.0      2020-10-05 [1] RSPM (R 4.0.3)
    #>    remotes          2.2.0      2020-07-21 [1] RSPM (R 4.0.2)
    #>    renv             0.12.5     2021-01-09 [1] RSPM (R 4.0.3)
    #>    rlang            0.4.10     2020-12-30 [1] RSPM (R 4.0.3)
    #>    rmarkdown        2.6        2020-12-14 [1] RSPM (R 4.0.3)
    #>    rprojroot        2.0.2      2020-11-15 [1] RSPM (R 4.0.3)
    #>    rstudioapi       0.13       2020-11-12 [1] RSPM (R 4.0.3)
    #>    sessioninfo      1.1.1      2018-11-05 [1] RSPM (R 4.0.0)
    #>    snakecase        0.11.0     2019-05-25 [1] RSPM (R 4.0.0)
    #>    stringi          1.5.3      2020-09-09 [1] RSPM (R 4.0.2)
    #>    stringr          1.4.0      2019-02-10 [1] RSPM (R 4.0.0)
    #>    testthat       * 3.0.1      2020-12-17 [1] RSPM (R 4.0.3)
    #>    tibble           3.0.5      2021-01-15 [1] RSPM (R 4.0.3)
    #>    tidyselect       1.1.0      2020-05-11 [1] RSPM (R 4.0.0)
    #>    usethis          2.0.0      2020-12-10 [1] RSPM (R 4.0.3)
    #>    vctrs            0.3.6      2020-12-17 [1] RSPM (R 4.0.3)
    #>    withr            2.4.1      2021-01-26 [1] RSPM (R 4.0.3)
    #>    xfun             0.20       2021-01-06 [1] RSPM (R 4.0.3)
    #>    yaml             2.2.1      2020-02-01 [1] RSPM (R 4.0.0)
    #> 
    #> [1] /usr/local/lib/R/site-library
    #> [2] /usr/lib/R/site-library
    #> [3] /usr/lib/R/library
    #> 
    #>  R -- Package was removed from disk.

</details>

Reproducible examples of PACTA workflows
----------------------------------------

This section shows reproducible examples of both the single- and
multiple-inputs workflows. You may use these examples in at least three
ways:

-   Read this document for an overview of which workflows are possible.
-   Run them on your local computer to track and explore their
    implementation.
-   Run them on a server as an integration test.

### R packages

Let's start by loading the packages we use in this file:

    library(devtools, warn.conflicts = FALSE)
    #> Loading required package: usethis
    load_all()
    #> Loading PACTA.analysis

Notice we `load_all()` features in the PACTA.analysis package, which
lives inside the PACTA\_analysis repository.

### Single-inputs workflow

TODO: Document inputs/outputs

> There's a pretty important absence here... any explanation of where
> the critically important input files are or need to be.
> Hypothetically, the repo should work because it includes some default
> example files, but any analyst truly using this repo... the first
> thing they would want to do is put their input files in working\_dir
> -- CJ

The single-inputs workflow is composed of multiple steps, each in its
own script: web\_tool\_script\_1.R, web\_tool\_script\_2.R,
web\_tool\_script\_3.R.

With R, you can run each script individually with something like
`source("web_tool_script_1.R")` or run multiple scripts at once. For
example, you can run scripts 1 and 2 with:

    # Helper from the package PACTA.analysis
    source_web_tool_scripts(1:2)
    #> Testing: Rscript --vanilla web_tool_script_1.R TestPortfolio_Input
    #> Testing: Rscript --vanilla web_tool_script_2.R TestPortfolio_Input

With the terminal, you can run each script individually. For example,
you can run script 3 with:

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
    $ Package fancyhdr Warning: \fancyfoot's `E' option without twoside option is use
    $ less on input line 89.
    $ Package fancyhdr Warning: \headheight is too small (12.0pt): 
    $ (fancyhdr)                Make it at least 55.4097pt, for example:
    $ (fancyhdr)                \setlength{\headheight}{55.4097pt}.
    $ (fancyhdr)                You might also make \topmargin smaller to compensate:
    $ Package fancyhdr Warning: \headheight is too small (12.0pt): 
    $ (fancyhdr)                Make it at least 55.4097pt, for example:
    $ (fancyhdr)                \setlength{\headheight}{55.4097pt}.
    $ (fancyhdr)                You might also make \topmargin smaller to compensate:

### Multiple-inputs workflow

TODO
