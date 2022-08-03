set_analysis_inputs_path <- function(twodii_internal, data_location_ext, dataprep_ref = datastore_timestamp) {

  if (twodii_internal) {
    analysis_inputs_path <- path_dropbox_2dii("PortCheck", "00_Data", "07_AnalysisInputs", dataprep_ref)
    analysis_inputs_path <- file.path(analysis_inputs_path)
  } else {
    # project level setting takes precedence, portfolio level second, else what
    # set_webtool_paths() sets for data_location_ext
    if (!is.null(proj_data_location_ext)) {
      analysis_inputs_path <- proj_data_location_ext
    } else if (!is.null(port_holdings_date)) {
      analysis_inputs_path <- file.path("..", "pacta-data", port_holdings_date)
    } else {
      analysis_inputs_path <- data_location_ext
    }

  }

  return(analysis_inputs_path)
}
