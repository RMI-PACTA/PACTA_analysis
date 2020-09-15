Mauro’s first run of the web tool
================
2020-09-15

## Setup

### R

``` r
library(testthat)
library(glue)
library(fs)
library(config)
library(rlang)
library(here)
library(tidyverse)
```

### Git

Fork and clone required repos:

``` bash
gh repo fork --clone 2DegreesInvesting/pacta-data
gh repo fork --clone 2DegreesInvesting/create_interactive_report
gh repo fork --clone 2DegreesInvesting/PACTA_analysis
```

Run from PACTA\_analysis/

``` bash
cd PACTA_analysis
```

``` bash
pwd
git remote -vv
$ /home/mauro/git/PACTA_analysis
$ origin    git@github.com:maurolepore/PACTA_analysis.git (fetch)
$ origin    git@github.com:maurolepore/PACTA_analysis.git (push)
$ upstream  git@github.com:2DegreesInvesting/PACTA_analysis.git (fetch)
$ upstream  git@github.com:2DegreesInvesting/PACTA_analysis.git (push)
```

Work off branch `current_web_functionality`.

``` bash
git remote show upstream | grep current_web_functionality
git branch
$     current_web_functionality  tracked
$   current_web_functionaliry
$   master
$ * mauro-wip
```

Inspect recent history:

``` bash
git log --oneline --no-merges current_web_functionaliry..HEAD
#> 8e5ed03 Cleanup
#> aa517d6 Add notes
#> 6784cbf Cleanup: Show rather than explain changes
#> c453ebf Fix working_location
#> 4ccdd59 Find a few more errors
#> 73f2032 Find error in web_tool_script_1.R: line 34
#> 809375b Remove horrible --hard reset
#> 762dcf1 Style
#> 10c80a7 Expect sibling repo
#> 78aa9c4 Try runnign web tool following Jacob's guide
```

## [Instructions by Jacob](https://bit.ly/2RCRJn7)

### 1\. Copy TestPortfolio\_Input.csv from sample\_files/20\_input\_files/ to working\_dir/20\_Raw\_Inputs/

``` r
# Ensure the source file exists
portfolio_csv <- "TestPortfolio_Input.csv"
from <- here("sample_files", "20_input_files", portfolio_csv)
stopifnot(file_exists(from))

# If it already exists, remove it with a warning
to <- here("working_dir", "20_Raw_Inputs", portfolio_csv)
if (file_exists(to)) {
  warn(glue("Removing existing file: {to}"))
  file_delete(to)
}
#> Warning: Removing existing file: /home/mauro/git/PACTA_analysis/working_dir/
#> 20_Raw_Inputs/TestPortfolio_Input.csv
```

``` r
file_copy(from, to)
```

``` r
expect_true(file_exists(to))
```

### 2\. Set “TestPortfolio\_Input” as the portfolio\_name\_ref\_all in all three web\_tool\_scripts (lines 30, 24 and 21)

``` r
show_pattern_in_file <- function(file, pattern) {
  grep(pattern, readLines(file), value = TRUE)
}
```

``` r
files <- dir_ls(regexp = "web_tool_script")
this_pattern <- "portfolio_name_ref_all.*<-.*TestPortfolio_Input"
matched <- map(files, show_pattern_in_file, pattern = this_pattern)

walk(matched, writeLines)
#> portfolio_name_ref_all <- c("TestPortfolio_Input")
#> portfolio_name_ref_all <- c("TestPortfolio_Input")
#> portfolio_name_ref_all <- c("TestPortfolio_Input")
```

``` r
script_has_this_pattern <- grepl(this_pattern, matched)
expect_true(all(script_has_this_pattern))
```

–

Note:

  - It seems useful to automate the process of running these files:
    web\_tool\_script\_1.R, web\_tool\_script\_2.R,
    web\_tool\_script\_3.R – for example, via a drake plan, or an
    rmarkdown document, or a function in a package.

  - It seems useful to parametrize these calls:
    portfolio\_name\_ref\_all \<- c(“TestPortfolio\_Input”) – for
    example, as parameters in a parametrized rmarkdown document, or as
    arguments to a function.

### 3 Configuration files

#### 3.1. Ensure that there is a config file with name “TestPortfolio\_Input\_PortfolioParameters.yml” in working\_dir/10\_Parameter\_File/ (this should already be the case, the content should not matter too much, as far as I can tell….)

``` r
config1 <- here(
  "working_dir",
  "10_Parameter_File",
  "TestPortfolio_Input_PortfolioParameters.yml"
)

writeLines(readLines(config1))
#> default:
#>     parameters:
#>         portfolio_name_in: test_portfolio
#>         investor_name_in: Test
```

``` r
expect_true(file_exists(config1))
```

#### 3.1. Set parameters

Ensure this configuration file exists, as well as all the directories
specified in the field `paths`:

``` r
config2 <- path("parameter_files", "WebParameters_2dii.yml")
writeLines(readLines(config2))
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
expect_true(file_exists(config2))
```

``` r
config2_paths <- config::get(file = config2)$paths
str(config2_paths)
#> List of 3
#>  $ project_location_ext: chr "~/git/PACTA_analysis/"
#>  $ data_location_ext   : chr "~/git/pacta-data/2019Q4/"
#>  $ template_location   : chr "~/git/create_interactive_report/"
```

``` r
expect_true(all(map_lgl(config2_paths, dir_exists)))
```

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

NOTE: A number of calls require parent directories, which should be
created if they don’t exist. You may do that with something like this:

``` r
# Compare with `create_portfolio_subfolders()`
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
