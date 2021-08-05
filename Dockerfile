FROM rocker/r-ver:latest

USER root

RUN Rscript -e 'install.packages("remotes")'

COPY DESCRIPTION /PACTA_analysis/DESCRIPTION
RUN Rscript -e 'remotes::install_deps("/PACTA_analysis")'

COPY . /PACTA_analysis
