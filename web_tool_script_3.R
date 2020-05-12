

options(encoding = "UTF-8") 

if(!require(pacman)) {install.packages(pacman)}
pacman::p_load(tidyr, dplyr, scales, reshape2, tidyverse, readxl, tidyselect, r2dii.utils,
               grid,ggplot2,ggthemes,gridExtra,scales,stringr,extrafont,knitr,RColorBrewer,
               matrixStats,rworldmap,ggmap,cowplot,ggrepel,ggforce,sitools,countrycode)

# source("0_portfolio_test.R")
source("0_graphing_functions.R")
source("0_reporting_functions.R")
# source("0_portfolio_input_check_functions.R")
source("0_global_functions.R")
source("0_web_functions.R")

if (rstudioapi::isAvailable()) {
  portfolio_name_ref <- "Portfolio3"
  working_location <- dirname(rstudioapi::getActiveDocumentContext()$path)
  working_location <- gsub("Â","",working_location)
} else {
  portfolio_name_ref = get_portfolio_name()
  working_location <- getwd()
}


working_location <- paste0(working_location, "/")


project_name <- "web_tool"
twodii_internal <- FALSE


# Change these to locations on the server. 
project_location_ext <- "C:/Users/clare/Desktop/ExternalTest"
data_location_ext <- "C:/Users/clare/Desktop/ExternalTest/r2dii_data/"

#####################################################################

# just done once
# create_project_folder(project_name, twodii_internal, project_location_ext)

# replaced with web version
# set_project_paths(project_name, twodii_internal, project_location_ext)
set_webtool_paths()

# just done once
# copy_files(project_name)

options(r2dii_config = paste0(par_file_path,"/AnalysisParameters.yml"))

set_global_parameters(paste0(par_file_path,"/AnalysisParameters.yml"))

# need to define an alternative location for data files
analysis_inputs_path <- set_analysis_inputs_path(twodii_internal, data_location_ext, dataprep_timestamp)


#########################################################################
# START REPORT PRINTING
#########################################################################

options(r2dii_config = paste0(par_file_path,"/ReportParameters.yml"))

set_report_parameters(paste0(par_file_path,"/ReportParameters.yml"))

# Set the variables for plotting
graph_values()

# List of Portfolios to print
portfolio_overview <- read_rds(paste0(proc_input_path, "/",project_name,"_overview_portfolio.rda"))
portfolio_overview$investor_name <- clean_punctuation(portfolio_overview$investor_name)
portfolio_overview$portfolio_name <- clean_punctuation(portfolio_overview$portfolio_name)
report_list <- get_report_list(portfolio_overview)


template <- readLines(paste0(getwd(),"/Templates/",templateversion,".tex")) #,encoding = "UTF-8"
translate_labels(Language)

i=1

investor_name_select <- report_list$investor_name[i]
portfolio_name_select <- report_list$portfolio_name[i]
investor_type <- report_list$Type[i]

# print(paste0(i, " of ", nrow(report_list)))

#######################
### Read in Results ###
########################
set_initial_variables()
test_list <- create_test_list()

results_call()

#########################
### REPORT GENERATION ###
#########################
report_handle <- graph_name("00",ParameterFile)
create_results_folder(project_name,investor_name_select,portfolio_name_select,report_handle)

ReportFigures()

# ReportGeneration()
