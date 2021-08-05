FROM rocker/r-ver:latest

RUN Rscript -e 'install.packages("remotes")'

COPY DESCRIPTION /r-pkg/DESCRIPTION
RUN Rscript -e 'remotes::install_deps("/r-pkg")' && rm r-pkg -r
