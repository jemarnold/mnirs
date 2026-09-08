# Normalise custom interval grouping to a complete named list

Adds intervals missing from a custom `group_intervals` list as
single-interval groups, warns on duplicates, and names groups by
user-supplied names with `interval_<ids>` fallback.

## Usage

``` r
normalise_interval_groups(
  group_intervals,
  n_intervals,
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
