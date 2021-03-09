
The docker image [rocker/verse](https://hub.docker.com/r/rocker/verse)
provides most of what you need for a functional computing environment,
including system dependencies, a bash terminal and RStudio. The image
[2dii/pacta\_analysis](https://hub.docker.com/r/2dii/pacta_analysis)
extends rocker/verse to include the required R packages. Here is its
Dockerfile:

    FROM rocker/verse:latest
    MAINTAINER "Mauro Lepore" maurolepore@gmail.com

    COPY docker /docker
    RUN Rscript /docker/install.R

Finally you need access to the PACTA [siblings](#Siblings). You can
create one or more volumes mapping the location of the siblings from
your (host) computer to the container.

Here are two ways to access a full computing environment, including
PACTA siblings.

*To work from with RStudio*, ensure your working directory is
PACTA\_analysis/, then run

``` bash
docker-compose up
```

RStudio is now available from your a browser at <http://localhost:8787>,
with some personal configuration files and all siblings available at the
home directory of the container. When you are finished using this
service, run

``` bash
docker-compose down
```

The details of this environment are described in the file
docker-compose.yml:

    version: "0.0.0.9000"
    services:
      rstudio:
        image: 2dii/pacta_analysis:latest
        container_name: pacta_analysis
        ports:
          - 8787:8787
        environment:
        - PASSWORD=${RSTUDIO_PASSWORD}
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

*To work from an interactive terminal*, ensure your working directory is
the parent of all PACTA siblings, then run

``` bash
docker run --rm -ti -v "$(pwd):/root" 2dii/pacta_analysis:latest bash`
```

You are now into an ephemeral (`--rm`) docker container running an
interactive `bash` terminal (`-it`) mapping (`-v`) the parent directory
of all PACTA siblings to the /root directory. For a slightly improved
environment replace the trailing `bash` with `docker/entrypoint`.

<details>
<summary>
Information of an R session running in this computing environment.
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
#>  date     2021-03-09                  
#> 
#> ─ Packages ───────────────────────────────────────────────────────────────────
#>  ! package        * version    date       lib source        
#>    assertthat       0.2.1      2019-03-21 [1] RSPM (R 4.0.3)
#>    cachem           1.0.4      2021-02-13 [1] RSPM (R 4.0.3)
#>    callr            3.5.1      2020-10-13 [1] RSPM (R 4.0.3)
#>    cli              2.3.0      2021-01-31 [1] RSPM (R 4.0.3)
#>    codetools        0.2-18     2020-11-04 [2] CRAN (R 4.0.4)
#>    config           0.3.1      2020-12-17 [1] RSPM (R 4.0.3)
#>    conflicted       1.0.4      2019-06-21 [1] RSPM (R 4.0.0)
#>    crayon           1.4.1      2021-02-08 [1] RSPM (R 4.0.3)
#>    DBI              1.1.1      2021-01-15 [1] RSPM (R 4.0.3)
#>    desc             1.2.0      2018-05-01 [1] RSPM (R 4.0.3)
#>    devtools         2.3.2      2020-09-18 [1] RSPM (R 4.0.3)
#>    digest           0.6.27     2020-10-24 [1] RSPM (R 4.0.3)
#>    dplyr            1.0.4      2021-02-02 [1] RSPM (R 4.0.3)
#>    ellipsis         0.3.1      2020-05-15 [1] RSPM (R 4.0.3)
#>    evaluate         0.14       2019-05-28 [1] RSPM (R 4.0.3)
#>    fansi            0.4.2      2021-01-15 [1] RSPM (R 4.0.3)
#>    fastmap          1.1.0      2021-01-25 [1] RSPM (R 4.0.3)
#>    fs               1.5.0      2020-07-31 [1] RSPM (R 4.0.3)
#>    fst              0.9.4      2020-08-27 [1] RSPM (R 4.0.3)
#>    generics         0.1.0      2020-10-31 [1] RSPM (R 4.0.3)
#>    glue             1.4.2      2020-08-27 [1] RSPM (R 4.0.3)
#>    here             1.0.1      2020-12-13 [1] RSPM (R 4.0.3)
#>    hms              1.0.0      2021-01-13 [1] RSPM (R 4.0.3)
#>    htmltools        0.5.1.1    2021-01-22 [1] RSPM (R 4.0.3)
#>    janitor          2.1.0      2021-01-05 [1] RSPM (R 4.0.3)
#>    knitr            1.31       2021-01-27 [1] RSPM (R 4.0.3)
#>    lifecycle        1.0.0      2021-02-15 [1] RSPM (R 4.0.3)
#>    lubridate        1.7.9.2    2020-11-13 [1] RSPM (R 4.0.3)
#>    magrittr         2.0.1      2020-11-17 [1] RSPM (R 4.0.3)
#>    memoise          2.0.0      2021-01-26 [1] RSPM (R 4.0.3)
#>  R PACTA.analysis * 0.0.0.9000 <NA>       [?] <NA>          
#>    pillar           1.5.0      2021-02-22 [1] RSPM (R 4.0.3)
#>    pkgbuild         1.2.0      2020-12-15 [1] RSPM (R 4.0.3)
#>    pkgconfig        2.0.3      2019-09-22 [1] RSPM (R 4.0.3)
#>    pkgload          1.1.0      2020-05-29 [1] RSPM (R 4.0.3)
#>    prettyunits      1.1.1      2020-01-24 [1] RSPM (R 4.0.3)
#>    processx         3.4.5      2020-11-30 [1] RSPM (R 4.0.3)
#>    ps               1.5.0      2020-12-05 [1] RSPM (R 4.0.3)
#>    purrr            0.3.4      2020-04-17 [1] RSPM (R 4.0.3)
#>    R6               2.5.0      2020-10-28 [1] RSPM (R 4.0.3)
#>    Rcpp             1.0.6      2021-01-15 [1] RSPM (R 4.0.3)
#>    readr            1.4.0      2020-10-05 [1] RSPM (R 4.0.3)
#>    remotes          2.2.0      2020-07-21 [1] RSPM (R 4.0.3)
#>    renv             0.13.0     2021-02-24 [1] RSPM (R 4.0.3)
#>    rlang            0.4.10     2020-12-30 [1] RSPM (R 4.0.3)
#>    rmarkdown        2.7        2021-02-19 [1] RSPM (R 4.0.3)
#>    rprojroot        2.0.2      2020-11-15 [1] RSPM (R 4.0.3)
#>    rstudioapi       0.13       2020-11-12 [1] RSPM (R 4.0.3)
#>    sessioninfo      1.1.1      2018-11-05 [1] RSPM (R 4.0.3)
#>    snakecase        0.11.0     2019-05-25 [1] RSPM (R 4.0.3)
#>    stringi          1.5.3      2020-09-09 [1] RSPM (R 4.0.3)
#>    stringr          1.4.0      2019-02-10 [1] RSPM (R 4.0.3)
#>    testthat       * 3.0.2      2021-02-14 [1] RSPM (R 4.0.3)
#>    tibble           3.1.0      2021-02-25 [1] RSPM (R 4.0.3)
#>    tidyselect       1.1.0      2020-05-11 [1] RSPM (R 4.0.3)
#>    usethis          2.0.1      2021-02-10 [1] RSPM (R 4.0.3)
#>    utf8             1.1.4      2018-05-24 [1] RSPM (R 4.0.3)
#>    vctrs            0.3.6      2020-12-17 [1] RSPM (R 4.0.3)
#>    withr            2.4.1      2021-01-26 [1] RSPM (R 4.0.3)
#>    xfun             0.21       2021-02-10 [1] RSPM (R 4.0.3)
#>    yaml             2.2.1      2020-02-01 [1] RSPM (R 4.0.3)
#> 
#> [1] /usr/local/lib/R/site-library
#> [2] /usr/local/lib/R/library
#> 
#>  R ── Package was removed from disk.
```

</details>

–

Reference:

-   [2DegreesInvesting/docker](https://github.com/2DegreesInvesting/docker/tree/master/r-packages).
-   [Install Docker](https://docs.docker.com/engine/install/).
-   [Install docker-compose](https://docs.docker.com/compose/install/).
