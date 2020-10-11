FROM maurolepore/2diirunner:1.3.0
MAINTAINER "Mauro Lepore" maurolepore@gmail.com

RUN apt-get update && apt-get install -y pandoc

# This repository has binaries not just for Windows and Mac, but also for Linux
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/bionic/latest'))" >> /root/.Rprofile \
    # update-dockerfile-packages-start
    && Rscript -e "install.packages('config')" \
    && Rscript -e "install.packages('countrycode')" \
    && Rscript -e "install.packages('cowplot')" \
    && Rscript -e "install.packages('devtools')" \
    && Rscript -e "install.packages('data.table')" \
    && Rscript -e "install.packages('dplyr')" \
    && Rscript -e "install.packages('extrafont')" \
    && Rscript -e "install.packages('fs')" \
    && Rscript -e "install.packages('fst')" \
    && Rscript -e "install.packages('ggforce')" \
    && Rscript -e "install.packages('ggmap')" \
    && Rscript -e "install.packages('ggplot2')" \
    && Rscript -e "install.packages('ggrepel')" \
    && Rscript -e "install.packages('ggthemes')" \
    && Rscript -e "install.packages('glue')" \
    && Rscript -e "install.packages('grid')" \
    && Rscript -e "install.packages('gridExtra')" \
    && Rscript -e "install.packages('here')" \
    && Rscript -e "install.packages('jsonlite')" \
    && Rscript -e "install.packages('knitr')" \
    && Rscript -e "install.packages('lme4')" \
    && Rscript -e "install.packages('matrixStats')" \
    && Rscript -e "install.packages('r2dii.utils')" \
    && Rscript -e "install.packages('RColorBrewer')" \
    && Rscript -e "install.packages('readxl')" \
    && Rscript -e "install.packages('renv')" \
    && Rscript -e "install.packages('reshape2')" \
    && Rscript -e "install.packages('rlang')" \
    && Rscript -e "install.packages('rworldmap')" \
    && Rscript -e "install.packages('scales')" \
    && Rscript -e "install.packages('sitools')" \
    && Rscript -e "install.packages('stringr')" \
    && Rscript -e "install.packages('testthat')" \
    && Rscript -e "install.packages('tidyr')" \
    && Rscript -e "install.packages('tidyselect')" \
    && Rscript -e "install.packages('tidyverse')" \
    && Rscript -e "install.packages('writexl')" \
    && Rscript -e "install.packages('xml2')" \
    # update-dockerfile-packages-end
    && echo ""
