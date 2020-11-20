PACTA\_analysis
================

This repository hosts the web tool. It’s scope covers multiple public
and private repositories at <https://github.com/2DegreesInvesting>:

``` r
pacta_find()
#> [1] "PACTA_analysis"            "StressTestingModelDev"    
#> [3] "create_interactive_report" "pacta-data"
```

The repository most closely related to “PACTA” is PACTA\_analysis. The
main outputs (that feed other aspects of the web tool) come from
sourcing one specific file:

``` r
source("web_tool_script_1.R")
#> 
#> ── web_tool_script_1.R ─────────────────────────────────────────────────────────
#> Warning in cfg$parameters$portfolio_name: partial match of 'portfolio_name' to
#> 'portfolio_name_in'
#> Warning in cfg$parameters$investor_name: partial match of 'investor_name' to
#> 'investor_name_in'
```
