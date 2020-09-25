Minimal file to test rendering a file on GitHub actions
================

Packages.

``` r
library(here)
```

    ## here() starts at /home/mauro/git/PACTA_analysis

``` r
parent_path <- function(...) {
  parent <- dirname(here::here())
  file.path(parent, ...)
}

path <- parent_path("pacta-data", "2019Q4", "0_Indices_bonds_portfolio.rda")
dataset <- readRDS(path)
head(dataset)
```

    ##   investor_name                     portfolio_name scenario       allocation
    ## 1 Indices2019Q4 iShares Global Corp Bond UCITS ETF     B2DS portfolio_weight
    ## 2 Indices2019Q4 iShares Global Corp Bond UCITS ETF     B2DS portfolio_weight
    ## 3 Indices2019Q4 iShares Global Corp Bond UCITS ETF     B2DS portfolio_weight
    ## 4 Indices2019Q4 iShares Global Corp Bond UCITS ETF     B2DS portfolio_weight
    ## 5 Indices2019Q4 iShares Global Corp Bond UCITS ETF     B2DS portfolio_weight
    ## 6 Indices2019Q4 iShares Global Corp Bond UCITS ETF     B2DS portfolio_weight
    ##   equity_market scenario_geography year ald_sector technology plan_tech_prod
    ## 1  GlobalMarket             Global 2020 Automotive   Electric         406727
    ## 2  GlobalMarket             Global 2020 Automotive     Hybrid         460551
    ## 3  GlobalMarket             Global 2020 Automotive        ICE       12928876
    ## 4  GlobalMarket             Global 2020   Aviation    Freight              0
    ## 5  GlobalMarket             Global 2020   Aviation        Mix              0
    ## 6  GlobalMarket             Global 2020   Aviation      Other              0
    ##   plan_alloc_wt_tech_prod plan_carsten plan_emission_factor scen_tech_prod
    ## 1                331.8963 0.0001053739         0.0000000000         406727
    ## 2                513.0830 0.0001682063         0.0001100174         460551
    ## 3               6596.0830 0.0020399986         0.0002126086       12928876
    ## 4                  0.0000 0.0000000000                  NaN              0
    ## 5                  0.0000 0.0000000000                  NaN              0
    ## 6                  0.0000 0.0000000000                  NaN              0
    ##   scen_alloc_wt_tech_prod scen_carsten scen_emission_factor plan_sec_prod
    ## 1                331.8963 0.0001053739         0.0000000000      13796154
    ## 2                513.0830 0.0001682063         0.0001100174      13796154
    ## 3               6596.0830 0.0020399986         0.0002126086      13796154
    ## 4                  0.0000 0.0000000000                  NaN          2163
    ## 5                  0.0000 0.0000000000                  NaN          2163
    ## 6                  0.0000 0.0000000000                  NaN          2163
    ##   plan_alloc_wt_sec_prod plan_sec_carsten plan_sec_emissions_factor
    ## 1           7441.0622534     0.0023135787              0.0001960516
    ## 2           7441.0622534     0.0023135787              0.0001960516
    ## 3           7441.0622534     0.0023135787              0.0001960516
    ## 4              0.4998577     0.0005636715              0.0001354580
    ## 5              0.4998577     0.0005636715              0.0001354580
    ## 6              0.4998577     0.0005636715              0.0001354580
    ##   scen_sec_prod scen_alloc_wt_sec_prod scen_sec_carsten
    ## 1      13796154           7441.0622534     0.0023135787
    ## 2      13796154           7441.0622534     0.0023135787
    ## 3      13796154           7441.0622534     0.0023135787
    ## 4          2163              0.4998577     0.0005636715
    ## 5          2163              0.4998577     0.0005636715
    ## 6          2163              0.4998577     0.0005636715
    ##   scen_sec_emissions_factor plan_tech_share scen_tech_share
    ## 1              0.0001960516      0.04460335      0.04460335
    ## 2              0.0001960516      0.06895292      0.06895292
    ## 3              0.0001960516      0.88644373      0.88644373
    ## 4              0.0001354580      0.00000000      0.00000000
    ## 5              0.0001354580      0.00000000      0.00000000
    ## 6              0.0001354580      0.00000000      0.00000000
    ##   trajectory_deviation trajectory_alignment
    ## 1                    0                    0
    ## 2                    0                    0
    ## 3                    0                    0
    ## 4                    0                    0
    ## 5                    0                    0
    ## 6                    0                    0
