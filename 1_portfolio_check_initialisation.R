## Project Initialisation
rm(list = ls())

options(encoding = "UTF-8")

devtools::load_all()
use_r_packages()

## Project Initialisation
rm(list = ls())

if (rstudioapi::isAvailable()) {
  working_location <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  working_location <- getwd()
}

working_location <- paste0(working_location, "/")
setwd(working_location)

source("0_portfolio_test.R")
source("0_graphing_functions.R")
source("0_reporting_functions.R")
source("0_portfolio_input_check_functions.R")
source("0_global_functions.R")
source("0_sda_approach.R")

project_name <- "DFS_Project"
twodii_internal <- FALSE
# TRUE or FALSE: TRUE means that the code is running on a 2dii laptop with dropbox connection

#####################################################################
### ONLY FOR EXTERNAL PROJECTS (twodii_internal <- FALSE):
# Variables must exist for internal projects

# usethis::install_github("2DegreesInvesting/r2dii.utils")
dropbox_path <- r2dii.utils::path_dropbox_2dii()
project_location_ext <- glue::glue(dropbox_path, "/DFS")

dropbox_path_data <- r2dii.utils::dbox_port_00()
data_location_ext <- glue::glue(dropbox_path_data, "/07_AnalysisInputs/2019Q4_10042020_2020")
#####################################################################

create_project_folder(project_name, twodii_internal, project_location_ext)

set_project_paths(project_name, twodii_internal, project_location_ext)

copy_files(project_name)

options(r2dii_config = paste0(par_file_path, "/AnalysisParameters.yml"))

set_global_parameters(paste0(par_file_path, "/AnalysisParameters.yml"))

analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)
