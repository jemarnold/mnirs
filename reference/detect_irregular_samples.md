# Report warnings for unbalanced time_channel samples

Report warnings for unbalanced time_channel samples

## Usage

``` r
detect_irregular_samples(
  x,
  time_channel,
  verbose = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- x:

  A numeric vector.

- time_channel:

  A character string naming the time or sample column. Must match a
  column name in `data` exactly.

  - If `NULL` (default), the `time_channel` metadata attribute of `data`
    is used.

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.
