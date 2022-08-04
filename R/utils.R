expect_no_message <- function(object, regexp = NA, ...) {
  testthat::expect_message(object = object, regexp = regexp, ...)
}

get_build_version <- function(env_var = "build_version") {
  build_version <- Sys.getenv(env_var)
  ifelse(nzchar(build_version), paste0("v", build_version), NA_character_)
}

get_build_version_msg <- function() {
  build_version <- get_build_version()
  ifelse(is.na(build_version), "", paste0(" (Docker build ", build_version, ")"))
}
