path_from <- function(from = NULL, ...) {
  if (is.null(from)) return(here::here(...))
  here::here(from, ...)
}

raw_inputs <- function() {
  list("working_dir", "20_Raw_Inputs")
}

parameter_file <- function() {
  list("working_dir", "10_Parameter_File")
}
