# Ensemble average multiple intervals

`group_channels` is a character vector applied to every interval, or a
list of per-interval channel vectors; channels excluded from an interval
do not contribute to that channel's ensemble-mean.

## Usage

``` r
ensemble_intervals(
  df_list,
  group_channels,
  metadata,
  verbose = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.
