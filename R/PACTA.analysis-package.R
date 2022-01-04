#' @keywords internal
#' @noRd
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr contains
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr group_vars
#' @importFrom dplyr grouped_df
#' @importFrom dplyr if_else
#' @importFrom dplyr is_grouped_df
#' @importFrom dplyr last
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom fs dir_create
#' @importFrom fs dir_delete
#' @importFrom fs dir_ls
#' @importFrom fs path
#' @importFrom glue glue
#' @importFrom rlang %||%
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom rlang warn
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest
#' @importFrom wand guess_content_type
## usethis namespace: end
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
