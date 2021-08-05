FROM rocker/verse:latest

COPY DESCRIPTION /pacta/DESCRIPTION
WORKDIR /pacta

# Binary packages as of Jul 29, 2021 6:00 PM GMT-6 for Ubuntu 20.04
# https://packagemanager.rstudio.com/client/#/repos/1/overview
RUN Rscript -e "install.packages('remotes')"
RUN Rscript -e "remotes::install_deps(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/focal/4412744'))" && \
  rm /pacta -r && \
  exit 0
