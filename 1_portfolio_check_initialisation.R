## Project Initialisation
rm(list=ls())

options(encoding = "UTF-8") 

library(tidyr)
library(dplyr)
library(scales)
library(reshape2)
library(tidyverse)
library(readxl)
library(tidyselect)


if (rstudioapi::isAvailable()) {
  working_location <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  working_location <- getwd()
}

working_location <- paste0(working_location, "/")
setwd(working_location)

library(r2dii.utils)

source("0_portfolio_test.R")
source("0_graphing_functions.R")
source("0_reporting_functions.R")
source("0_portfolio_input_check_functions.R")
source("0_global_functions.R")
source("0_sda_approach.R")


# Set Project Settings

# within the "project_setting.yml" config file, set the project_name, the twodii_internal switch,
# and the external data locations, if necessary.
# the project_name will determine the name of the folder that is created for this project
# Set twodii_internal to TRUE to run the analysis on an internal 2dii laptop
# This setting uses the dropbox connection for data import
# Set twodii_internal to FALSE, tu use external data locations
# Specify these data locations in the config file "project_settings.yml" in the repo

cfg <- config::get(file = "project_settings.yml")

check_valid_cfg(cfg)

project_name <- cfg$project_name
twodii_internal <- cfg$project_internal$twodii_internal
project_location_ext <- cfg$project_internal$project_location_ext
data_location_ext <- cfg$project_internal$data_location_ext


create_project_folder(project_name, twodii_internal, project_location_ext)

set_project_paths(project_name, twodii_internal, project_location_ext)

copy_files(project_name)

options(r2dii_config = paste0(par_file_path,"/AnalysisParameters.yml"))

set_global_parameters(paste0(par_file_path,"/AnalysisParameters.yml"))

analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)

