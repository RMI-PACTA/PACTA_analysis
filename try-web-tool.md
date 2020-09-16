Mauro’s first run of the web tool
================
2020-09-15

## Setup

Packages

``` r
library(testthat)
library(glue)
library(config)
library(rlang)
library(here)
library(fs)
library(tidyverse)
```

Required paths

``` r
file_name <- "TestPortfolio_Input.csv"
example_dataset <- here::here("sample_files", "20_input_files", file_name)
expected_dataset <- here::here("working_dir", "20_Raw_Inputs", file_name)
```

Required directories

``` r
# Similar to `create_portfolio_subfolders()`
create_directory_if_it_doesnt_exist <- function(directory) {
  if (!fs::dir_exists(directory)) {
    fs::dir_create(directory)
  }
  
  invisible(directory)
}

children <- c("30_Processed_Inputs", "40_Results", "50_Outputs")
(paths <- here("working_dir", children))
#> [1] "/home/mauro/git/PACTA_analysis/working_dir/30_Processed_Inputs"
#> [2] "/home/mauro/git/PACTA_analysis/working_dir/40_Results"         
#> [3] "/home/mauro/git/PACTA_analysis/working_dir/50_Outputs"

walk(paths, create_directory_if_it_doesnt_exist)
```

Required repos

``` bash
# Terminal
gh repo fork --clone 2DegreesInvesting/pacta-data
gh repo fork --clone 2DegreesInvesting/create_interactive_report
gh repo fork --clone 2DegreesInvesting/PACTA_analysis
```

Required branch: Run from inside the project PACTA\_analysis/,
particularly on the branch `current_web_functionality`.

``` bash
# Terminal
cd PACTA_analysis
```

Required working directory

``` r
here()
#> [1] "/home/mauro/git/PACTA_analysis"
```

NOTE: The branch `current_web_functionaliry` is long-lived, which makes
the workflow more complex than the standard GitHub workflow (by which
the only long-lived branch is `master`). I suggest merging
`current_web_functionality` into master, or extracting the
non-overlapping code into a separate repo.

## [Instructions by Jacob](https://bit.ly/2RCRJn7)

### 1\. Data

Here we use an example dataset, which the analysis expects in a specific
folder.

Sanity checks:

``` r
stopifnot(file_exists(example_dataset))

if (file_exists(expected_dataset)) {
  warn(glue("Removing existing file: {expected_dataset}"))
  file_delete(expected_dataset)
}
#> Warning: Removing existing file: /home/mauro/git/PACTA_analysis/working_dir/
#> 20_Raw_Inputs/TestPortfolio_Input.csv

file_copy(example_dataset, expected_dataset)

expect_true(file_exists(expected_dataset))
```

### 2\. Assign a value to `portfolio_name_ref_all`

2.  Set “TestPortfolio\_Input” as the portfolio\_name\_ref\_all in all
    three web\_tool\_scripts (lines 30, 24 and 21)

I explore what value is currently assigned to the variable
`TestPortfolio_Input`:

``` r
show_pattern_in_file <- function(file, pattern) {
  grep(pattern, readLines(file), value = TRUE)
}

(files <- dir_ls(regexp = "web_tool_script"))
#> web_tool_script_1.R web_tool_script_2.R web_tool_script_3.R
this_pattern <- "portfolio_name_ref_all.*<-"
matched <- map(files, show_pattern_in_file, pattern = this_pattern)
```

It looks like the values are already the ones we expect:

``` r
walk(matched, writeLines)
#> portfolio_name_ref_all <- c("TestPortfolio_Input")
#> portfolio_name_ref_all <- c("TestPortfolio_Input")
#> portfolio_name_ref_all <- c("TestPortfolio_Input")

script_has_this_pattern <- grepl(this_pattern, matched)
expect_true(all(script_has_this_pattern))
```

NOTE: It seems useful to make `portfolio_name_ref_all` a parameter; that
way the user can set it without changing source code.

### 3 Configuration files

3.1. Ensure this config file:

``` r
config_1 <- here(
  "working_dir",
  "10_Parameter_File",
  "TestPortfolio_Input_PortfolioParameters.yml"
)

writeLines(readLines(config_1))
#> default:
#>     parameters:
#>         portfolio_name_in: test_portfolio
#>         investor_name_in: Test

expect_true(file_exists(config_1))
```

3.1. Ensure this other configuration also file exists, and has correct
paths:

``` r
config_2 <- here("parameter_files", "WebParameters_2dii.yml")
writeLines(readLines(config_2))
#> default:
#>     paths:
#>         project_location_ext: ~/git/PACTA_analysis/
#>         data_location_ext: ~/git/pacta-data/2019Q4/
#>         template_location: ~/git/create_interactive_report/
#>     parameters:
#>         project_name: working_dir
#>         twodii_internal: FALSE
#>         new_data: FALSE
```

``` r
expect_true(file_exists(config_2))
```

``` r
config_paths <- config::get(file = config_2)$paths
str(config_paths)
#> List of 3
#>  $ project_location_ext: chr "~/git/PACTA_analysis/"
#>  $ data_location_ext   : chr "~/git/pacta-data/2019Q4/"
#>  $ template_location   : chr "~/git/create_interactive_report/"

all_paths_exist <- all(map_lgl(config_paths, dir_exists))
expect_true(all_paths_exist)
```

NOTE: It would be nice to set these paths from a more flexible
interface, like the arguments of a function or the parameters of a
parametrized rmarkdown file.

### 4\. Run scripts

  - web\_tool\_script\_1.R will produce processed input files in
    working\_dir/30\_Processed\_Inputs/

<!-- end list -->

``` r
source("web_tool_script_1.R")
#> Warning in read_file(paste0(file_location, "/fund_data.fst")): ~/git/pacta-data/
#> 2019Q4/cleaned_files/fund_data.fst does not exist
#> [1] "No Equity in portfolio"
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 30_Processed_Inputs/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 40_Results/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 50_Outputs/TestPortfolio_Input' already exists
```

**FIXME: Warning at web\_tool\_script\_1.R\#105**

``` r
Warning message:
In read_file(paste0(file_location, "/fund_data.fst")) :
  ~/git/pacta-data/2019Q4/cleaned_files/fund_data.fst does not exist
```

Not sure if this dataset is crucial, but it’s missing from my clone of
pacta-data/:

``` r
dir_ls(path("..", "pacta-data", "2019Q4", "cleaned_files"))
#> ../pacta-data/2019Q4/cleaned_files/average_sector_intensity.fst
#> ../pacta-data/2019Q4/cleaned_files/comp_fin_data.fst
#> ../pacta-data/2019Q4/cleaned_files/company_emissions.fst
#> ../pacta-data/2019Q4/cleaned_files/currencies.fst
#> ../pacta-data/2019Q4/cleaned_files/debt_fin_data.fst
#> ../pacta-data/2019Q4/cleaned_files/fin_data.fst
```

  - web\_tool\_script\_2.R will take the processed inputs from the
    previous step, calculate PACTA results and write them to
    working\_dir/40\_Results/

NOTE: I see duplicated code in the first lines of each script. For
example, you may extract all calls to `library()` into a file
packages.R; and you may also extract the repeated definitions of
`working_location` (which I think should be `working_location <-
here::here()`).

``` r
source("web_tool_script_2.R")
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 30_Processed_Inputs/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 40_Results/TestPortfolio_Input' already exists
#> Warning in dir.create(.x): '/home/mauro/git/PACTA_analysis/working_dir//
#> 50_Outputs/TestPortfolio_Input' already exists
#> [1] "1: Test"
```

  - web\_tool\_script\_3.R this will take the results from the previous
    step plus some pre-calculated data from the pacta-data repo and
    input these results into the create\_interactive\_report function.

<!-- end list -->

``` r
parent <- path_dir(here())
sibling <- path(parent, "create_interactive_report")

if (!dir_exists(sibling)) {
  abort(glue("
    PACTA_analysis/ and create_interactive_report/ must share a parent directory
    See https://git.io/JUBkN
  "))
}

expect_true(dir_exists(sibling))
```

``` r
source("web_tool_script_3.R") 
```

If all goes well, this will copy some of the css, js etc files into
working\_dir/50\_Outputs/, along with the results written into the
index.html file. It will also produce a zip archive of the results,
which is the current not-so-pretty solution to download the report and
use everything interactively when offline).

``` r
outputs <- path("working_dir", "50_Outputs")

css <- dir_ls(outputs, recurse = TRUE, regexp = "[.]css")
expect_true(length(css) > 0L)

js <- dir_ls(outputs, recurse = TRUE, regexp = "[.]js")
expect_true(length(js) > 0L)

index <- dir_ls(outputs, recurse = TRUE, regexp = "index[.]html")
expect_true(length(index) > 0L)
```

``` r
n <- 20L
writeLines(head(readLines(index), n))
#> <!DOCTYPE html>
#> <html lang="" xml:lang="">
#> <head>
#> 
#>   <meta charset="utf-8" />
#>   <meta http-equiv="X-UA-Compatible" content="IE=edge" />
#>   <title>Interactive Portfolio Report</title>
#>   <meta name="description" content="" />
#>   <meta name="generator" content="bookdown 0.19 and GitBook 2.6.7" />
#> 
#>   <meta property="og:title" content="Interactive Portfolio Report" />
#>   <meta property="og:type" content="book" />
#>   
#>   
#>   
#>   
#> 
#>   <meta name="twitter:card" content="summary" />
#>   <meta name="twitter:title" content="Interactive Portfolio Report" />
#> 
```

``` r
save_space <- FALSE
dir_tree(path(outputs, "TestPortfolio_Input"), recurse = save_space)
#> working_dir/50_Outputs/TestPortfolio_Input
#> ├── 2dii_gitbook_style.css
#> ├── TestPortfolio_Input_results.zip
#> ├── company_charts
#> ├── css
#> ├── data
#> ├── export
#> ├── img
#> ├── index.html
#> ├── js
#> ├── libs
#> └── search_index.json
```
