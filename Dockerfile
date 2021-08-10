FROM rocker/r-ver:4.0.2

USER root

RUN Rscript -e 'install.packages("remotes")'

COPY DESCRIPTION /bound/DESCRIPTION
RUN Rscript -e 'remotes::install_deps("/bound", dependencies = TRUE)'

COPY . /bound

WORKDIR /bound

CMD ["./bin/run-pacta"]
