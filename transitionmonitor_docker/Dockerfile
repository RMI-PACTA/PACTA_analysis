# using R 4.1.2, and CTAN and CRAN packages available as of 2022.01.01
# https://hub.docker.com/_/r-base
# https://www.texlive.info/tlnet-archive
# https://packagemanager.rstudio.com/client/#/repos/1/overview

FROM r-base:4.1.2

# install system dependencies
ARG SYS_DEPS="\
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    openssh-client \
    wget \
    "
RUN apt-get update \
    && apt-get install -y --no-install-recommends $SYS_DEPS \
    && chmod -R a+rwX /root \
    && rm -rf /var/lib/apt/lists/*

# install TeX system and fonts
ARG TEX_APT="\
    texlive-xetex \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    lmodern \
    "
RUN apt-get update \
    && apt-get install -y --no-install-recommends $TEX_APT \
    && tlmgr init-usertree \
    && rm -rf /var/lib/apt/lists/*

# install TeX packages
ARG CTAN_REPO=https://www.texlive.info/tlnet-archive/2022/01/01/tlnet
ARG TEX_DEPS="\
    amsmath \
    blindtext \
    bookmark \
    caption \
    fancyhdr \
    geometry \
    grffile \
    hyperref \
    lipsum \
    microtype \
    parskip \
    sectsty \
    silence \
    titlesec \
    unicode-math \
    upquote \
    wrapfig \
    xcolor \
    xurl \
    "
RUN tlmgr --repository $CTAN_REPO install $TEX_DEPS

# install system dependencies for R packages
ARG R_PKG_SYS_DEPS="\
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libtiff-dev \
    libxml2-dev \
    libxt6 \
    make \
    pandoc \
    perl \
    zlib1g-dev \
    "
RUN apt-get update \
    && apt-get install -y --no-install-recommends $R_PKG_SYS_DEPS \
    && rm -rf /var/lib/apt/lists/*

# install R package dependencies
ARG CRAN_REPO=https://packagemanager.rstudio.com/all/2022-01-03+Y3JhbiwyOjQ1MjYyMTU7NTY4Qjk1ODA
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
    progress \
    purrr \
    readr \
    readxl \
    rmarkdown \
    scales \
    stringi \
    stringr \
    tibble \
    tidyr \
    vctrs \
    vroom \
    writexl \
    yaml \
    zoo \
    "
RUN Rscript -e "\
    install.packages('remotes'); \
    pkg_deps <- strsplit(trimws(gsub('[\\\]+', '', '$PKG_DEPS')), '[[:space:]]+')[[1]]; \
    remotes::install_cran(pkg_deps, repos = '$CRAN_REPO'); \
    "

# copy in PACTA repos
COPY pacta-data /pacta-data
COPY r2dii.stress.test.data /r2dii.stress.test.data
COPY r2dii.climate.stress.test /r2dii.climate.stress.test
COPY create_interactive_report /create_interactive_report
COPY PACTA_analysis /bound

# set permissions for PACTA repos
RUN chmod -R a+rwX /bound \
    && chmod -R a+rwX /create_interactive_report \
    && chmod -R a+rwX /pacta-data \
    && chmod -R a+rwX /r2dii.climate.stress.test \
    && chmod -R a+rwX /r2dii.stress.test.data

# set the build_version environment variable
ARG image_tag
ENV build_version=$image_tag

RUN exit 0
