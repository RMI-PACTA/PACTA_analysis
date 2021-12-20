# using R 4.1.2, and CTAN and CRAN packages available as of 2021.11.18
# https://hub.docker.com/_/r-base
# https://www.texlive.info/tlnet-archive
# https://packagemanager.rstudio.com/client/#/repos/1/overview

FROM r-base:4.1.2

ARG SYS_DEPS="\
    git \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libtiff-dev \
    libxml2-dev \
    libxt6 \
    make \
    pandoc \
    perl \
    wget \
    zlib1g-dev \
    "
# install system dependencies
RUN apt-get update \
    && apt-get install --assume-yes --no-install-recommends $SYS_DEPS \
    && apt-get clean

ARG CRAN_REPO=https://packagemanager.rstudio.com/cran/2021-11-17+MTo2MzM5NDY2O0U0MTdGRUE5
ARG CTAN_REPO=https://www.texlive.info/tlnet-archive/2021/11/18/tlnet

ARG TEX_DEPS="\
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
    zapfding \
    "
# install LaTeX and tlmgr packages
RUN Rscript -e " \
    install.packages('tinytex', repos = '$CRAN_REPO'); \
    tinytex::install_tinytex(repository = '$CTAN_REPO'); \
    tex_deps <- strsplit(trimws(gsub('[\\\]+', '', '$TEX_DEPS')), '[[:space:]]+')[[1]]; \
    tinytex::tlmgr_install(pkgs = tex_deps); \
    "

ARG PKG_DEPS="\
    bookdown \
    cli \
    config \
    countrycode \
    devtools \
    dplyr \
    forcats \
    fs \
    fst \
    ggplot2 \
    glue \
    here \
    janitor \
    jsonlite \
    knitr \
    magrittr \
    purrr \
    readr \
    readxl \
    rmarkdown \
    scales \
    stringr \
    tibble \
    tidyr \
    writexl \
    yaml \
    zoo \
    "
RUN Rscript -e " \
      pkg_deps <- strsplit(trimws(gsub('[\\\]+', '', '$PKG_DEPS')), '[[:space:]]+')[[1]]; \
      install.packages('remotes', repos = '$CRAN_REPO'); \
      remotes::install_cran(pkg_deps, repos = '$CRAN_REPO'); \
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
