# Provide a programmatic interface to pass parameters when rendering this file.
# This can be used, for example, as a test locally or on GitHub actions.
rmd <- "try-web-tool.Rmd"

library(here)
library(fs)

path_from_parent <- function(...) path(path_dir(here()), ...)

parameters <- list(
  example_dataset = here('sample_files', '20_input_files', 'TestPortfolio_Input.csv'),
  project_location_ext = here(),
  data_location_ext = path_from_parent('pacta-data', '2019Q4'),
  template_location = path_from_parent('create_interactive_report')
)

rmarkdown::render(rmd, "github_document", params = parameters)

outputs <- path("working_dir", "50_Outputs")
expect_true(length(fs::dir_ls(outputs)) > 0L)
