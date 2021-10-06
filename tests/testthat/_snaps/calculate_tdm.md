# outputs the expected tibble

    Code
      out
    Output
      # A tibble: 1 x 5
        technology    ald_sector tdm_tech tdm_sec reference_year
        <chr>         <chr>         <dbl>   <dbl>          <dbl>
      1 RenewablesCap Power             2       2           2020

# if only `allocation` is 'ownership_weight' outputs the expected
          tibble with a warning

    Code
      calculate_tdm(data, 2020)
    Warning <has_zero_rows>
      Filtering for "portfolio_weight" allocation, outputs 0 rows
    Output
      # A tibble: 0 x 5
      # ... with 5 variables: technology <chr>, ald_sector <chr>, tdm_tech <dbl>,
      #   tdm_sec <dbl>, reference_year <int>

