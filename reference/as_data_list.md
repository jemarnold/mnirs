# Coerce `data` input to a named list of data frames

Accepts a single or grouped data frame, a list of data frames, or an
*"mnirs_kinetics"* object, whose `coefficients` are split by
`nirs_channels` into one data frame per channel (a row per interval) for
recursive analysis of coefficients.

## Usage

``` r
as_data_list(data, env = rlang::caller_env())
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing time series data and
  metadata.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.
