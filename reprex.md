Minimal file to test rendering a file on GitHub actions
================

``` r
tryCatch(
  message("Reproducible example rendered successfully."),
  error = function(e) {
    msg <- paste("Reproducible example failed with:\n", conditionMessage(e))
    stop(msg, call. = FALSE)
  }
)
```

    ## Reproducible example rendered successfully.
