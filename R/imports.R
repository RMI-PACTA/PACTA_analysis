#' @importFrom glue glue
#' @importFrom dplyr arrange case_when filter group_by group_vars grouped_df
#' @importFrom dplyr is_grouped_df last last left_join mutate pull rename select
#' @importFrom dplyr ungroup all_of contains distinct bind_rows
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider pivot_longer unnest
#' @importFrom rlang abort warn inform %||%
#' @importFrom fs path dir_create dir_delete dir_ls
NULL

globalVariables(
  c(
    ".data",
    ".env",
    "get_portfolio_name",
    "outputs_path",
    "portfolio_name",
    "portfolio_name_ref_all",
    "set_web_parameters",
    "working_location"
  )
)
