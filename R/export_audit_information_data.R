export_audit_information_data <- function(audit_file_ = audit_file,
                                          portfolio_total_ = portfolio_total,
                                          folder_path = proc_input_path,
                                          project_name_ = NA) {

  # Check format
  if (isFALSE(is.character(folder_path) && length(folder_path) == 1)) {
    stop("`folder_path`` is not a string (a length one character vector).")
  }
  if (isFALSE(is.data.frame(audit_file_))) { stop("`audit_file_` is not a data.frame") }

  if ("Meta Investor" %in% audit_file_$investor_name) {
    audit_file_ <- subset(audit_file_, investor_name != "Meta Investor")
  }
  if ("Meta Investor" %in% portfolio_total_$investor_name) {
    portfolio_total_ <- subset(portfolio_total_, investor_name != "Meta Investor")
  }

  folder_path <- paste0(folder_path, "/")
  if (!is.na(project_name_)) {
    folder_path <- paste0(folder_path, project_name_, "_")
  }
  # function
  export_audit_graph_json(audit_file_, paste0(folder_path, "coveragegraph"))
  export_audit_invalid_data(portfolio_total_, paste0(folder_path, "invalidsecurities"))
  export_audit_textvar_json(portfolio_total_, file.path(folder_path, "coveragetextvar.json"))
}
