Mauro’s attempt to run the web tool
================
2020-09-15

## Setup

### R

``` r
library(testthat)
library(config)
library(here)
library(tidyverse)
```

### Git

Fork and clone 2DegreesInvesting/PACTA\_analysis:

``` bash
gh repo fork --clone 2DegreesInvesting/PACTA_analysis
```

``` bash
git remote -vv
$ origin    git@github.com:maurolepore/PACTA_analysis.git (fetch)
$ origin    git@github.com:maurolepore/PACTA_analysis.git (push)
$ upstream  git@github.com:2DegreesInvesting/PACTA_analysis.git (fetch)
$ upstream  git@github.com:2DegreesInvesting/PACTA_analysis.git (push)
```

Work on branch `current_web_functionality`.

``` bash
git remote show upstream | grep current_web_functionality
$     current_web_functionality  tracked
$     mauro-wip merges with remote current_web_functionality
```

``` bash
git remote show upstream | grep current_web_functionality
$     current_web_functionality  tracked
```

## [Instructions](https://bit.ly/2RCRJn7)

Now in order to actually run a test portfolio, you need to:

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
#>   portfolio_name_ref_all <- c("TestPortfolio_Input")
#>   portfolio_name_ref_all <- c("TestPortfolio_Input")
#>   portfolio_name_ref_all <- c("TestPortfolio_Input")
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
#> Error: At least one argument must be supplied (input file).n
```

#### Exploring the error

``` r
> traceback()
7: stop("Config file ", basename(file), " not found in current working ", 
       "directory", ifelse(use_parent, " or parent directories", 
    ...
6: config::get(file = file_path) at 0_web_functions.R#77
5: set_web_parameters(file_path = paste0(working_location, "/parameter_files/WebParameters_2dii.yml")) at web_tool_script_1.R#32
4: eval(ei, envir)
3: eval(ei, envir)
2: withVisible(eval(ei, envir))
1: source("web_tool_script_1.R", echo = TRUE)
```

The script behaves differently in an interactive versus non-interactive
session. When I knit this document, it runs in a non-interactive
session:

``` r
interactive()
#> [1] FALSE
```

This also means that the rstudio API is unavailable (but in this case
using the rstudioapi seems a suboptimal solution).

``` r
rstudioapi::isAvailable()
#> [1] FALSE
```

In a non-interactive session, we enter the `else` statement in line 34
of web\_tool\_script\_1.R with looks like this:

``` r
portfolio_name_ref_all = get_portfolio_name()
working_location <- getwd()
set_web_parameters(file_path = paste0(working_location,"/parameter_files/WebParameters_docker.yml"))
```

And `get_portfolio_name()` throws an error:

``` r
get_portfolio_name()
#> Error: At least one argument must be supplied (input file).n
```

This function calls the argument `portfolio_name_ref` that is not
defined in the function signature (e.g. there is no `x` in `f <-
function(x) { x + 1}`):

``` r
get_portfolio_name
#> function () 
#> {
#>     portfolio_name_ref = commandArgs(trailingOnly = TRUE)
#>     if (length(portfolio_name_ref) == 0) {
#>         stop("At least one argument must be supplied (input file).n", 
#>             call. = FALSE)
#>     }
#>     return(portfolio_name_ref)
#> }
#> <bytecode: 0x5614e87564d8>
```

Apparently, it’s expected to come from the global environment (which is
a bad idea); surprisingly, it is defined in an interactive session, but
not in a non-interactive session:

``` r
interactive()
#> [1] FALSE

exists("portfolio_name_ref")
#> [1] FALSE
```

–

NOTE:

I stop here. The rest fails until we successfully
`source("web_tool_script_1.R")`.

–

  - web\_tool\_script\_2.R will take the processed inputs from the
    previous step, calculate PACTA results and write them to
    working\_dir/40\_Results/

<!-- end list -->

``` r
source("web_tool_script_2.R")
#> Error: At least one argument must be supplied (input file).n
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
#> Error: At least one argument must be supplied (input file).n
```

If all goes well, this will copy some of the css, js etc files into
working\_dir/50\_Outputs/, along with the results written into the
index.html file. It will also produce a zip archive of the results,
which is the current not-so-pretty solution to download the report and
use everything interactively when offline)
