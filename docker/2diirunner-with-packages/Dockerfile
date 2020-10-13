FROM maurolepore/2diidockerrunner:latest
MAINTAINER "Mauro Lepore" maurolepore@gmail.com

# This repository has binaries for Linux
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/bionic/latest'))" >> /root/.Rprofile \
    # update-dockerfile-packages-start
    && Rscript -e "install.packages( \
             c( \
               'conflicted', \
               'assertthat', \
               'config', \
               'countrycode', \
               'cowplot', \
               'devtools', \
               'data.table', \
               'dplyr', \
               'extrafont', \
               'fs', \
               'fst', \
               'ggforce', \
               'ggmap', \
               'ggplot2', \
               'ggrepel', \
               'ggthemes', \
               'glue', \
               'grid', \
               'gridExtra', \
               'here', \
               'jsonlite', \
               'knitr', \
               'lme4', \
               'matrixStats', \
               'r2dii.utils', \
               'RColorBrewer', \
               'readxl', \
               'renv', \
               'reshape2', \
               'rlang', \
               'rworldmap', \
               'scales', \
               'sitools', \
               'stringr', \
               'testthat', \
               'tidyr', \
               'tidyselect', \
               'tidyverse', \
               'writexl', \
               'usethis', \
               'withr', \
               'xml2' \
             ) \
           )" \
    # update-dockerfile-packages-end
    && echo ""

