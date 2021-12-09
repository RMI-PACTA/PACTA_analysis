# using R 4.1.2, and CTAN and CRAN packages available as of 2021.11.18
# https://hub.docker.com/r/rocker/verse/tags?name=4.1.2
# https://packagemanager.rstudio.com/client/#/repos/1/overview

FROM rocker/verse:4.1.2

ARG RSTUDIO_PANDOC
ENV RSTUDIO_PANDOC='/usr/local/bin'

ARG CTAN_REPO
ENV CTAN_REPO=https://www.texlive.info/tlnet-archive/2021/11/18/tlnet

ARG CRAN
ENV CRAN=https://packagemanager.rstudio.com/all/__linux__/focal/2021-11-17+Y3JhbiwyOjQ1MjYyMTU7MzM5RjM4RjI

RUN tlmgr install \
    amsmath \
    auxhook \
    bigintcalc \
    bitset \
    blindtext \
    caption \
    etexcmds \
    etoolbox \
    euenc \
    fancyhdr \
    fontspec \
    geometry \
    gettitlestring \
    hycolor \
    hyperref \
    iftex \
    infwarerr \
    intcalc \
    kvdefinekeys \
    kvoptions \
    kvsetkeys \
    letltxmacro \
    lipsum \
    ltxcmds \
    mathspec \
    pdfescape \
    pdftexcmds \
    refcount \
    rerunfilecheck \
    sectsty \
    sourcesanspro \
    stringenc \
    texlive-scripts \
    tipa \
    titlesec \
    unicode-math \
    uniquecounter \
    wrapfig \
    xcolor \
    xunicode \
    zapfding

ARG PKG_DEPS
ENV PKG_DEPS='\
    assertr \
    assertthat \
    bookdown \
    cli \
    config \
    conflicted \
    countrycode \
    crayon \
    devtools \
    dplyr \
    DT \
    forcats \
    fs \
    fst \
    ggplot2 \
    glue \
    here \
    ISOcodes \
    janitor \
    jsonlite \
    knitr \
    magrittr \
    pkgdown \
    purrr \
    r2dii.analysis \
    r2dii.data \
    r2dii.plot \
    readr \
    readxl \
    rlang \
    rmarkdown \
    rstudioapi \
    rworldmap \
    scales \
    snakecase \
    spelling \
    stringi \
    stringr \
    testthat \
    tibble \
    tidyr \
    tidyselect \
    tidyverse \
    usethis \
    viridis \
    withr \
    writexl \
    yaml \
    zoo \
    '

RUN echo "options(repos = c(CRAN='$CRAN'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site \
    && Rscript -e " \
         install.packages('remotes'); \
         pkg_deps <- strsplit('$PKG_DEPS', '[[:space:],]+')[[1]]; \
         remotes::install_cran(pkg_deps); \
         devtools::install_github('2DegreesInvesting/r2dii.utils'); \
         "

COPY pacta-data /pacta-data
COPY r2dii.stress.test.data /r2dii.stress.test.data
COPY PACTA_analysis /bound
COPY create_interactive_report /create_interactive_report
COPY r2dii.climate.stress.test /r2dii.climate.stress.test

RUN chmod -R a+rwX /bound \
    && chmod -R a+rwX /create_interactive_report \
    && chmod -R a+rwX /pacta-data \
    && chmod -R a+rwX /r2dii.climate.stress.test \
    && chmod -R a+rwX /r2dii.stress.test.data

ARG image_tag
ENV build_version=$image_tag

RUN exit 0
