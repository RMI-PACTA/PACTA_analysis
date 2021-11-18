#' @importFrom glue glue
#' @importFrom rlang abort warn inform %||%
#' @importFrom fs path dir_create dir_delete dir_ls
NULL

globalVariables(
  c(
    "get_portfolio_name",
    "outputs_path",
    "portfolio_name",
    "portfolio_name_ref_all",
    "set_web_parameters",
    "working_location"
  )
)
