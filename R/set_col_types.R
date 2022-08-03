set_col_types <- function(grouping_variables, fixed_col_types) {

  # defines the column types based on the number of grouping_variables
  port_col_types <- paste0(paste0(rep("c", length(grouping_variables)), collapse = ""), fixed_col_types)

  return(port_col_types)
}
