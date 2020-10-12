FROM maurolepore/2diirunner:1.3.0
MAINTAINER "Mauro Lepore" maurolepore@gmail.com

RUN apt-get update && apt-get install -y pandoc

# This repository has binaries not just for Windows and Mac, but also for Linux
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/bionic/latest'))" >> /root/.Rprofile \
    # update-dockerfile-packages-start
    && Rscript -e "install.packages(
             c(
               "conflicted",
               "config",
               "countrycode",
               "cowplot",
               "devtools",
               "data.table",
               "dplyr",
               "extrafont",
               "fs",
               "fst",
               "ggforce",
               "ggmap",
               "ggplot2",
               "ggrepel",
               "ggthemes",
               "glue",
               "grid",
               "gridExtra",
               "here",
               "jsonlite",
               "knitr",
               "lme4",
               "matrixStats",
               "r2dii.utils",
               "RColorBrewer",
               "readxl",
               "renv",
               "reshape2",
               "rlang",
               "rworldmap",
               "scales",
               "sitools",
               "stringr",
               "testthat",
               "tidyr",
               "tidyselect",
               "tidyverse",
               "writexl",
               "xml2"
             )
           )
    # update-dockerfile-packages-end
    && echo ""

