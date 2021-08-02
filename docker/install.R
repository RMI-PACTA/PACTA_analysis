# Build an image with R packages from a specific date `snapshot`:
# docker build -t 2dii/pacta_analysis:yyyy-mm-dd .

# 2021-03-04 (see https://packagemanager.rstudio.com/client/#/repos/1/overview)
snapshot <- "https://packagemanager.rstudio.com/all/__linux__/focal/1695015"
options(repos = c(CRAN = snapshot))

url <- paste0(
  "https://raw.githubusercontent.com/2DegreesInvesting/docker/",
  "extract-install.R/",
  "r-packages/install.R"
)
source(url)
