use_r_packages <- function() {
  suppressPackageStartupMessages({
    for (pkg in required_packages_vec()) {
      library(pkg, character.only = TRUE)
    }
  })
}
