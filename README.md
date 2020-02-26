# PACTA_analysis

This set of files is the methodology for running PACTA on EQ and CB portfolios. 

To use this repository of code please follow the following instructions:

Ensure you have the required libraries installed on your computer. This includes the r2dii.utils library (https://github.com/2DegreesInvesting/r2dii.utils) 

There are four files that must be run in sequence to create results. 
1_portfolio_check_initialisation.R
2_project_input_analysis.R
3_run_analysis.R
4_report_maker.R

Within 1_portfolio_check_initialisation.R there are some lines that should be edited to make reference to local directories on your computer. 

project_name <- "TEST"
twodii_internal <- FALSE 

project_location_ext <- "/Desktop/ExternalTest"
data_location_ext <- "/Desktop/ExternalTest/Inputs"

Results files will be written to project_location_ext
Data files should be added to data_location_ext

After running 1_portfolio_check_intialisation, go to the project_location_ext folder and to 20_raw_inputs to add your own portfolio data to this file. 

Sample data files will be made available shortly. Complete data files are available under specific circumstances. Be in contact at transitionmonitor@2degrees-investing.org if you would like to find out more about this.  
