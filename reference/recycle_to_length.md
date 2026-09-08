# Recycle parameter list to target length

Recycle parameter list to target length

## Usage

``` r
recycle_to_length(
  param,
  n,
  name = c("event", "group"),
  verbose = TRUE,
  env = rlang::caller_env(),
  arg = "values"
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
