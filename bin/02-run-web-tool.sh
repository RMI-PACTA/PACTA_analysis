#! /bin/bash

# Set permissions so that new files can be deleted/overwritten outside docker
umask 000

cd /bound

# Install faster, using binaries for linux from RStudio package manager
echo 'options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"))' >> ~/.Rprofile
# Rscript -e "update.packages(ask = FALSE)"
Rscript -e "install.packages(c('here','writexl','countrycode', 'renv', 'tibble', 'stringr', 'zoo', 'data.table'))"

Rscript --vanilla web_tool_script_1.R TestPortfolio_Input
Rscript --vanilla web_tool_script_2.R TestPortfolio_Input
Rscript --vanilla web_tool_script_3.R TestPortfolio_Input
