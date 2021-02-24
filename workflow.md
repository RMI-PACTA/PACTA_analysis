PACTA\_analysis workflow
================
Clare
08/02/2021

## Aim

The aim of this document is to outline the workflow for running a PACTA
project.

## PACTA\_analysis Repo

The PACTA\_analysis repo contains the primary PACTA functions required
to run a portfolio alignment assessment for investors. The current
repository is built in a way that the functions work for both an online
application - ie for the P2020 projects and platform; as well as the
original application which is running a project or generating results
offline. The platform can be found at platform.transitionmonitor.com and
is maintained by Wim at Constructiva (<w.schuermann@constructiva.de> )

Whether online or offline, running an analysis has three components

1.  Portfolio Cleaning: Clean the input portfolio and merge in financial
    data, to categorise each holding and identify whether it’s equity
    (EQ) or corporate bonds (CB)
2.  Run Analysis: Merge in ald + scenarios, and group results at
    company, portfolio and regional level
3.  Results: Present the results in a clear output format

Both the online and offline scripts can be used to generate reports
offline. The difference is that the offline set of scripts is set up to
allow the generation of results for multiple portfolios and investors in
one run, whereas online is focussed on quickly generating a report for
one portfolio at once. While all steps maintain the grouping by investor
and portfolio, to allow for this, it hasn’t been optimised for running
multiple portflios as once. The online tool however has been designed
for the interactive reports, whereas the offline scripts have primarily
been used to generate the static pdf results.

### Portfolio Cleaning

This occurs in web\_tool\_script\_1.R (online) or
2\_project\_input\_analysis.R (offline).

Both start with reading in the necessary files. This includes some file
cleaning, that should be shifting to the data\_preprataion repository.

## Difference between online and offline

There are two sets of scripts that call the functions required for both
offline or online functionality.

Online: The “web\_tool\_script” set from 1 to 3 runs through the
components of the analysis Offline: The 1,2,3,4 scripts run similarly,
however have an initialisation script that should be run before each
online in order to set up project pathways and naming. This allows for
added flexibility as you may online want to be focussing on one of the
components of the analysis. Ie you’ve already generated results and just
want to create output files.

# Running a project

## Offline

This steps through the functions required to run a project using the
original scripts. The aim of this script is to define project paths and
set parameters used in the following scripts.

-   Set project name and pathways to data in the first chunk of this
    code

-   Run first chunk

-   Get portfolio file with format (Investor.Name, Portfolio.Name, ISIN,
    MarketValue, Currency, NumberofShares), saved as “.csv”;

-   Place in the project folder

-   Check and edit the parameter file to meet the project requirements

Crticial Set Up Components - Parameter file (ProjectParameters\_GENERAL)
- Portfolio file ("\*\_Input.csv") - Data files (multiple)

-   File paths to
    1.  Data location
    2.  Project location (inputs and outputs)

### Set up parameters and paths

    ## Loading PACTA.analysis
    ## Loading PACTA.analysis

    ## [1] "Project Folder Already Exists"

TO DO:

-   Improve the sourcing of files. Setting the internal flag to false to
    allow for defining this is one solution here.

-   Synchronise the parameter files with the online parameter files

### Portfolio input analysis

Aims:

-   Reads in necessary data files and cleans them

To Run:

-   You need a portfolio to be placed in the folder
    `{r} fs::path(project_location_ext, project_name, "20_Raw_Inputs")`.
    There are sample files found in this repository in the folder
    “sample\_files”

Note:

-   Fund file optional. This adds additional holdings to the portfolio
    (adds in the details of the companies owned within a fund). If the
    file for the current time stamp is there this should be read in.
    Often this file is project specific as the file size can be very
    large.

-   this is currently reading in the “cleaned” files found in the
    pacta-data repo. This workflow needs to be cleaned up so that these
    “cleaned” files can be created for any project.

<!-- -->

    ## Warning in all(currencies$exchange_rate): coercing argument of type 'double' to
    ## logical

    ## [1] "currency data temporary. todo: update"

    ## Warning in get_and_clean_fund_data(): No fund data available

    ## Warning: Missing column names filled in: 'X2' [2]

    ## Parsed with column specification:
    ## cols(
    ##   isin = col_character(),
    ##   X2 = col_logical()
    ## )

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Parsed with column specification:
    ## cols(
    ##   bics_sector = col_character(),
    ##   bics_subgroup = col_character()
    ## )

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## Parsed with column specification:
    ## cols(
    ##   Portfolio.Name = col_character(),
    ##   Investor.Name = col_character(),
    ##   ISIN = col_character(),
    ##   MarketValue = col_double(),
    ##   Currency = col_character(),
    ##   NumberofShares = col_logical()
    ## )

TO DO:

-   move cleaning to data preparation

### Run Analysis

This step merges in the ald\_scen file to the specific portfolio, and
aggregates at the different levels of granularity required for the
results - portfolio level and company level.

    ## [1] "1: Test"
    ## [1] "1: Test"
    ## [1] "Test"
    ## [1] "Test"

There are options as to where to proceed from here. From this point
there are options: /item Run the stress testing code /item Print static
graphics and the pdf report (older code) /item Print the interactive
report (html) and executive summary (pdf)

This is where there is a different in workflow between the online and
offline case. The offline workflow directs a user to the pdf code that
we’ve been using recently. There should be little stopping a user from
printing a html report, however the script to do this may need to be
created.

#### SDA Approach

The SDA approach is a methodology that is used for “Other” Sectors that
do not have clear technology roadmaps, such as Steel, Cement, and
Aviation but do have emission factor models.

This is just a test really to understand how this code works as we may
want to incorporate and not lose this work that was done by Vincent.

``` r
# msci_world <- read_rds(file.path(data_location_ext, "Indices_equity_portfolio.rda")) %>% 
#   filter(portfolio_name == "iShares MSCI World ETF")
# 
# port_all_eq_sda <- sda_portfolio_target(market = msci_world,
#                           portfolio = port_all_eq,
#                           scenario = "ETP2017_B2DS",
#                           geography = "Global",
#                           ald_sector = c("Cement","Steel", "Aviation"),
#                           start_year = start_year,
#                           target_year = 2025)
```

## Results

``` r
# source("5_interactive_report.R")
```

## QA

There is a QA script written by Jacob Kastl that should also be included
here.
