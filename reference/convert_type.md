# Coerce column types by role: nirs numeric, event integer, others detected

Coerce column types by role: nirs numeric, event integer, others
detected

## Usage

``` r
convert_type(data, channels, verbose = TRUE, env = rlang::caller_env())
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing time series data and
  metadata.

- channels:

  A list of `time`, `event`, and `nirs` column names.

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.
