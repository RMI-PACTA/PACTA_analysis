FROM rocker/r-ver:latest

USER root

RUN Rscript -e 'install.packages("remotes")'

COPY DESCRIPTION /PACTA_analysis/DESCRIPTION
RUN Rscript -e 'remotes::install_deps("/PACTA_analysis")'

COPY . /PACTA_analysis

WORKDIR /PACTA_analysis

RUN head -n 211 web_tool_script_2.R >> web_tool_script_2_pruned.R

RUN echo '#! /bin/bash \ 
        Rscript --vanilla web_tool_script_1.R "${1:-TestPortfolio_Input}" && \
        Rscript --vanilla web_tool_script_2_pruned.R "${1:-TestPortfolio_Input}"' >> bin/run_minimal_pacta.sh
