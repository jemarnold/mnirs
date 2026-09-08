# apply span to resolved times and build interval_spec data frame

apply span to resolved times and build interval_spec data frame

## Usage

``` r
apply_span(
  interval_list,
  t_vec,
  span,
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
