
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PACTA\_analysis <a href='https://github.com/2DegreesInvesting/PACTA_analysis'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->
<!-- badges: end -->

The main goal of PACTA\_analysis is to run
[PACTA](https://2degrees-investing.org/resource/pacta/).

## Usage

From a terminal (e.g. bash) running PACTA involves three steps:

1.  Clone the private data and the public source code from GitHub, and
    work from the the experimental branch where we’re “dockerizing”
    PACTA.

``` bash
git clone git@github.com:2DegreesInvesting/pacta-data.git  # Private data!
git clone git@github.com:2DegreesInvesting/PACTA_analysis -b 478-add_vanilla_pacta_dockerfile
cd PACTA_analysis
```

2.  Setup directories for inputs and outputs (io):

-   Create input/ and output/ directories, as siblings of your working
    directory

-   Populate the input directory with a portfolio file like
    [TestPortfolio\_Input.csv](https://github.com/jdhoffa/PACTA_analysis/blob/add_basic_dockerfile/working_dir/20_Raw_Inputs/TestPortfolio_Input.csv),
    and a parameters file like
    [TestPortfolio\_Input\_PortfolioParameters.yml](https://github.com/jdhoffa/PACTA_analysis/blob/add_basic_dockerfile/working_dir/20_Raw_Inputs/TestPortfolio_Input.csv)
    (here we use some test files).

``` bash
./bin/setup-io
```

3.  Run PACTA via
    [`docker-compose`](https://docs.docker.com/compose/install/).

``` bash
docker-compose up
```

<details>

You may interact with the PACTA container with:

``` bash
docker-compose run app bash
```

These are the files used to create the Docker image and run the
container:

``` bash
cat Dockerfile
FROM rocker/r-ver:4.0.2

USER root

RUN Rscript -e 'install.packages("remotes")'

COPY DESCRIPTION /bound/DESCRIPTION
RUN Rscript -e 'remotes::install_deps("/bound")'

COPY . /bound

WORKDIR /bound

CMD ["./bin/run-pacta"]
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
services: 
  app:
    volumes:
      - ../pacta-data:/pacta-data
      - ../input:/input
      - ../output:/output
```

</details>

You may remove the input/ and output/ directories and start again.

``` bash
rm ../input ../output -ri
```

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
