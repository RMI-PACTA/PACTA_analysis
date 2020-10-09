library(usethis)

# Moving the .rda files directly to data/ causes an error about magic numbers
# The error does not throw if they are saved via use_data()
currencies <- readRDS("data-raw/currencies.rda")
use_data(currencies, overwrite = TRUE)

index_regions <- readRDS("data-raw/index_regions.rda")
use_data(index_regions, overwrite = TRUE)

bench_regions <- readRDS("data-raw/bench_regions.rda")
use_data(bench_regions, overwrite = TRUE)
