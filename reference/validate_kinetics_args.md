# Validate resolved per-channel kinetics arguments

Validates each channel's resolved argument list once, before any
fitting, so an invalid argument fails fast rather than after an
expensive fit on an earlier channel. Validation is keyed on which
arguments are present. Mutating validators are applied and written back:
[`validate_start_time()`](https://jemarnold.github.io/mnirs/reference/validate_start_time.md)
clamps `start_time`, and `align` is matched to its choices. Verbose
hints are emitted for the first channel only to avoid repeating
identical messages.

## Usage

``` r
validate_kinetics_args(
  per_channel,
  data,
  t_vec,
  verbose = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- per_channel:

  Named list of resolved argument lists, one per channel.

- data:

  A data frame of class *"mnirs"* containing time series data and
  metadata.

- t_vec:

  Numeric vector of `time_channel` values.

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

The `per_channel` list with mutating validators applied.
