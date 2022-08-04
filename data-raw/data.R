library(usethis)

# Moving the .rds files directly to data/ causes an error about magic numbers
# The error does not throw if they are saved via use_data()
currencies <- readRDS("data-raw/currencies.rds")
use_data(currencies, overwrite = TRUE)

index_regions <- readRDS("data-raw/index_regions.rds")
use_data(index_regions, overwrite = TRUE)

bench_regions <- readRDS("data-raw/bench_regions.rds")
use_data(bench_regions, overwrite = TRUE)
