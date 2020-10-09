#! /bin/bash

# docker image import https://tool.transitionmonitor.com/downloads/2diirunner_1.3.0.tar.xz 2diirunner:1.3.0

# TODO: Change directory to PACTA_analysis/
analysis=$(pwd)
data=$(dirname $(pwd))/pacta-data
report=$(dirname $(pwd))/create_interactive_report
stresstest=$(dirname $(pwd))/StressTestingModelDev

# To commit changes to this image, remove the flag --rm
docker run --rm -ti \
  --mount type=bind,source=$analysis,target=/bound \
  --mount type=bind,source=$data,target=/pacta-data \
  --mount type=bind,source=$report,target=/create_interactive_report \
  --mount type=bind,source=$stresstest,target=/StressTestingModelDev \
  2diirunner:1.3.0 /bin/bash

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
