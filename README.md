
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PACTA\_analysis <a href='https://github.com/2DegreesInvesting/PACTA_analysis'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/PACTA_analysis)](https://CRAN.R-project.org/package=PACTA_analysis)
[![R-CMD-check](https://github.com/2DegreesInvesting/PACTA_analysis/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/PACTA_analysis/actions)
<!-- badges: end -->

The main goal of PACTA\_analysis is to run the [PACTA
methodology](https://2degrees-investing.org/resource/pacta/). This
document helps you to install, setup, and run PACTA from a terminal
(e.g. bash).

### 1. Install

Clone the private data and public source code from GitHub, then work
from your local clone of the source code:

``` bash
git clone git@github.com:2DegreesInvesting/pacta-data.git  # Private data!
git clone git@github.com:2DegreesInvesting/PACTA_analysis.git
cd PACTA_analysis
```

### 2. Setup

Setup directories for inputs and outputs (io):

-   Create input/ and output/ directories, these can live anywhere on
    your computer.

-   Populate the input directory with portfolio files like
    [TestPortfolio\_Input.csv](https://github.com/2DegreesInvesting/PACTA_analysis/blob/master/working_dir/20_Raw_Inputs/TestPortfolio_Input.csv),
    and parameters files like
    [TestPortfolio\_Input\_PortfolioParameters.yml](https://github.com/2DegreesInvesting/PACTA_analysis/blob/master/working_dir/10_Parameter_File/TestPortfolio_Input_PortfolioParameters.yml).

<details>

Each corresponding `<pair-name>` the portfolio and parameter files must
be named `<pair-name>_Input.csv` and
`<pair-name>_Input_PortfolioParameters.yml`, respectively. For example:

-   This pair is valid: `a_Input.csv`,
    `a_Input_PortfolioParameters.yml`.

-   This pair is invalid: `a_Input.csv`,
    `b_Input_PortfolioParameters.yml`.

In the parameter files, whatever values you give to `portfolio_name_in`
and `investor_name_in` will populate the columns `portfolio_name` and
`investor_name` of some output files. For example:

-   A parameter file:

<!-- -->

    default:
        parameters:
            portfolio_name_in: TestPortfolio_Input
            investor_name_in: Test
            peer_group: pensionfund
            language: EN
            project_code: CHPA2020

-   A few rows of some relevant output files and columns:

<!-- -->

    named list()

</details>

Edit the `docker-compose.override.yml` file to point to your
`pacta-data`, `input` and `output` folders. (Notice how the standard
`docker-compose.override.yml` currently points to my local folders):

``` bash
cat docker-compose.override.yml
version: "3.2"
services:
  app: 
    volumes: 
      - /Users/jdhoffa/Documents/github/2dii/pacta-data:/pacta-data:ro
      - /Users/jdhoffa/Desktop/pacta/input:/input:ro
      - /Users/jdhoffa/Desktop/pacta/output:/output
    working_dir: /bound
    command: /bound/bin/run-pacta
```

### 3. Run

Run PACTA via
[`docker-compose`](https://docs.docker.com/compose/install/).

``` bash
docker-compose up
```

<details>

You may interact with the PACTA container with:

``` bash
docker-compose run app bash
```

You may mount your local source code with:

``` bash
docker-compose run -v "$(pwd)":/PACTA_analysis app bash
```

These are the files used to create the Docker image and run the
container:

``` bash
cat Dockerfile
FROM rocker/r-ver:latest

RUN Rscript -e 'install.packages("remotes")'

COPY DESCRIPTION /bound/DESCRIPTION
RUN Rscript -e 'remotes::install_deps("/bound", dependencies = TRUE)'

COPY . /bound
RUN chmod -R +x /bound
```

``` bash
cat docker-compose.yml
version: "3.2"
services: 
  app:
    build: .
```

``` bash
cat docker-compose.override.yml
version: "3.2"
services:
  app: 
    volumes: 
      - /Users/jdhoffa/Documents/github/2dii/pacta-data:/pacta-data:ro
      - /Users/jdhoffa/Desktop/pacta/input:/input:ro
      - /Users/jdhoffa/Desktop/pacta/output:/output
    working_dir: /bound
    command: /bound/bin/run-pacta
```

You may remove the input/ and output/ directories and start again.

``` bash
sudo rm ../input ../output -ri
```

</details>

## Funding

This project has received funding from the [European Union LIFE
program](https://wayback.archive-it.org/12090/20210412123959/https://ec.europa.eu/easme/en/)
and the [International Climate Initiative
(IKI)](https://www.international-climate-initiative.com/en/details/project/measuring-paris-agreement-alignment-and-financial-risk-in-financial-markets-18_I_351-2982).
The Federal Ministry for the Environment, Nature Conservation and
Nuclear Safety (BMU) supports this initiative on the basis of a decision
adopted by the German Bundestag. The views expressed are the sole
responsibility of the authors and do not necessarily reflect the views
of the funders. The funders are not responsible for any use that may be
made of the information it contains.
