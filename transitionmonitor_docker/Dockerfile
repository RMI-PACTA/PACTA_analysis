# using R 4.1.2 and CRAN packages available as of 2021.11.18
# https://hub.docker.com/layers/rocker/verse/4.1.2/images/sha256-1e639ca754d1963cfb259c23341fef17858b8de2766f46a16095e8807cda83f9?context=explore
# https://packagemanager.rstudio.com/client/#/repos/1/overview

FROM rocker/verse:4.1.2

ENV build_version=$image_tag

ARG CRAN
ENV CRAN=${CRAN:-https://packagemanager.rstudio.com/all/__linux__/focal/2021-11-17+Y3JhbiwyOjQ1MjYyMTU7MzM5RjM4RjI}
RUN echo "options(repos = c(CRAN='$CRAN'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

RUN tlmgr install \
    caption mathspec sectsty sourcesanspro wrapfig fancyhdr xcolor blindtext \
    titlesec lipsum amsmath iftex euenc fontspec tipa unicode-math xunicode \
    kvoptions ltxcmds kvsetkeys etoolbox auxhook bigintcalc bitset etexcmds \
    gettitlestring hycolor hyperref intcalc kvdefinekeys letltxmacro pdfescape \
    refcount rerunfilecheck stringenc uniquecounter zapfding pdftexcmds \
    infwarerr texlive-scripts geometry

RUN install2.r --skipinstalled --error \
    assertthat bookdown config conflicted countrycode crayon \
    data.table devtools dplyr DT forcats fs fst ggplot2 glue \
    highcharter here janitor jsonlite knitr plyr purrr readr readxl \
    renv reshape2 rlang rmarkdown rstudioapi scales snakecase stringr \
    testthat tibble tidyr tidyselect tidyverse usethis withr writexl yaml zoo

COPY PACTA_analysis /bound
COPY create_interactive_report /create_interactive_report
COPY pacta-data /pacta-data
COPY r2dii.climate.stress.test /r2dii.climate.stress.test
COPY r2dii.stress.test.data /r2dii.stress.test.data

RUN chmod -R a+rwX /bound
RUN chmod -R a+rwX /create_interactive_report
RUN chmod -R a+rwX /pacta-data
RUN chmod -R a+rwX /r2dii.climate.stress.test
RUN chmod -R a+rwX /r2dii.stress.test.data

RUN exit 0
