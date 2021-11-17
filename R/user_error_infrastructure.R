prepare_error_report <- function(
  report_template = file.path("inst", "rmd", "user_errors.Rmd"),
  error_file = file.path(html_directory, "user_errors.json"),
  html_directory = file.path(outputs_path, portfolio_name, "report"),
  file_name = "index.html"
  ) {
  if (file.exists(error_file)) {
    errors <- jsonlite::read_json(path = error_file)
  } else {
    stop("user errors file cannot be found.")
  }
  if (!dir.exists(html_directory)) {
    dir.create(html_directory)
  }
  output_file <- file.path(html_directory, file_name)
  rmarkdown::render(
    input = report_template,
    output_file = output_file,
    params = list(errors = errors)
  )
}

log_user_errors <- function(
  error_name,
  suggested_action = "",
  description = "",
  error_file = file.path(
    outputs_path,
    portfolio_name,
    "report",
    "user_errors.json"
    ),
  immediate = FALSE
  ) {
  # check for strings
  stopifnot(is.character(error_name))
  stopifnot(is.character(suggested_action))
  stopifnot(is.character(description))
  # check that directory exists, create if not
  if (!dir.exists(dirname(error_file))) {
    dir.create(dirname(error_file), recursive = TRUE)
  }
  # read in existing errors, or use null if none so far
  if (file.exists(error_file)) {
    errors <- jsonlite::read_json(
      path = error_file,
      simplifyVector = FALSE
    )
  } else {
    errors <- list()
  }
  # format this error, and append to the list (using c())
  this_error <- list(name  = error_name)
  if (description != "") {
    this_error["description"] <- description
  }
  if (suggested_action != "") {
    this_error["action"] <- suggested_action
  }
  errors <- c(errors, list(this_error))
  jsonlite::write_json(
    x = errors,
    path = error_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  if (immediate) {
    prepare_error_report()
  }
}

convert_user_errors_to_md <- function(
  errors,
  header_level = 3
  ) {
  name_prefix <- paste(rep("#", header_level), collapse = "")
  action_prefix <- paste(rep("#", header_level + 1), collapse = "")
  all_error_texts <- list()
  for (error in errors) {
    section_names <- names(error)
    # Prepare section header
    if ("name" %in% section_names) {
      header <- paste(name_prefix, error[["name"]])
    } else {
      header <- paste(name_prefix, "Unclassified Error")
    }
    section <- list(header = header)
    # Add description
    if ("description" %in% section_names) {
      description <- paste("**Description:**", error[["description"]])
      section <- c(section, description)
    }
    # Suggested actions
    if ("action" %in% section_names) {
      action <- paste0(
        action_prefix,
        " Suggested Action", "\n\n",
        error[["action"]]
      )
      section <- c(section, action)
    }
    error_text <- paste(section, collapse = "\n\n")
    all_error_texts <- c(all_error_texts, error_text)
  }
  all_error_texts <- paste(all_error_texts, collapse = "\n\n")
  return(all_error_texts)
}
