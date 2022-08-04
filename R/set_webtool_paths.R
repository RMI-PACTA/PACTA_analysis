set_webtool_paths <- function(project_root_dir = "working_dir") {
  project_location <<- file.path(working_location, project_root_dir)

  log_path <<- file.path(project_location, "00_Log_Files", portfolio_name_ref_all)
  par_file_path <<- file.path(project_location, "10_Parameter_File")
  raw_input_path <<- file.path(project_location, "20_Raw_Inputs")
  proc_input_path <<- file.path(project_location, "30_Processed_Inputs")
  results_path <<- file.path(project_location, "40_Results")
  outputs_path <<- file.path(project_location, "50_Outputs")
}
