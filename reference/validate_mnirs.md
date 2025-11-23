# Validate `{mnirs}` parameters

Passes through manually defined parameters, or defines them from
metadata if present, and validates relevant data quality checks.

## Usage

``` r
validate_numeric(
  x,
  elements = Inf,
  range = NULL,
  inclusive = c("left", "right"),
  integer = FALSE,
  msg = ""
)

validate_mnirs_data(data, ncol = 2L)

validate_nirs_channels(data, nirs_channels, inform = TRUE)

validate_time_channel(data, time_channel)

validate_event_channel(data, event_channel, require = TRUE)

estimate_sample_rate(x)

validate_sample_rate(data, time_channel, sample_rate, inform = TRUE)
```

## Arguments

- x:

  A numeric vector.

- elements:

  A numeric value for the number of numeric elements in `arg`.

- range:

  A two-element numeric vector for the range of valid numeric values in
  `arg`.

- inclusive:

  A character vector to specify which of `left` and/or `right` boundary
  values should be included in the range, or both (the *default*), or
  excluded if `FALSE`.

- integer:

  A logical indicating whether to test `arg` as an integer using
  [`rlang::is_integerish()`](https://rlang.r-lib.org/reference/is_integerish.html),
  rather than a numeric (`FALSE` by *default*).

- msg:

  A character string detailing the `cli_abort` error message returned
  for invalid numeric values passed to `arg`.

- data:

  A data frame of class *"mnirs"* containing at least one column with
  numeric time or sample values, and one column with numeric mNIRS
  values, along with metadata.

- nirs_channels:

  A character vector of mNIRS channel names. Must match column names in
  `data` exactly. Will be taken from metadata if not defined explicitly.

- inform:

  A logical to display (the *default*) or `FALSE` to silence warnings
  and information messages used for troubleshooting.

- time_channel:

  A character string indicating the time or sample channel name. Must
  match column names in `data` exactly. Will be taken from metadata if
  not defined explicitly.

- event_channel:

  A character string indicating the event or lap channel name. Must
  match column names in `data` exactly. Will be taken from metadata if
  not defined explicitly.

- require:

  A logical to specify whether `event_channel` is required (the
  *default*) or optional (`event_channel` returned as `NULL`).

- sample_rate:

  A numeric value for the sample rate in Hz. Will be taken from metadata
  or estimated from `time_channel` if not defined explicitly.

## Value

The validated object or an error message with
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).
