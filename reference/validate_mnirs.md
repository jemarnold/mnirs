# Validate `{mnirs}` parameters

Resolve and validate *mnirs* metadata and perform basic data quality
checks.

## Usage

``` r
validate_numeric(
  x,
  elements = Inf,
  range = NULL,
  inclusive = c("left", "right"),
  integer = FALSE,
  invalid = FALSE,
  msg1 = "",
  msg2 = "."
)

validate_mnirs_data(data, ncol = 2L)

validate_nirs_channels(
  nirs_channels,
  data,
  verbose = TRUE,
  env = rlang::caller_env()
)

validate_time_channel(time_channel, data, env = rlang::caller_env())

validate_event_channel(
  event_channel,
  data,
  required = TRUE,
  env = rlang::caller_env()
)

estimate_sample_rate(x)

validate_sample_rate(data, time_channel, sample_rate, verbose = TRUE)

validate_width_span(width = NULL, span = NULL, verbose = TRUE)

validate_x_t(x, t, invalid = FALSE)
```

## Arguments

- x:

  A numeric vector.

- elements:

  An integer. Default is `Inf`. The number of numeric elements expected
  in `x`.

- range:

  A two-element numeric vector giving the valid range for `x`.

- inclusive:

  A character vector specifying which boundaries of `range` are
  included. Any of `"left"`, `"right"` (default is both). Use `FALSE` to
  exclude both endpoints.

- integer:

  Logical. Default is `FALSE`. If `TRUE`, validate `x` as integer-like
  values using
  [`rlang::is_integerish()`](https://rlang.r-lib.org/reference/is_integerish.html).
  Otherwise tested as a numeric value.

- msg1, msg2:

  A character string appended to the
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  message when numeric validation fails.

- data:

  A data frame of class *"mnirs"* containing time series data and
  metadata.

- nirs_channels:

  A character vector giving the names of mNIRS columns to operate on.
  Must match column names in `data` exactly.

  - If `NULL` (default), the `nirs_channels` metadata attribute of
    `data` is used.

- verbose:

  Logical. Default is `TRUE`. Will display or silence (if `FALSE`)
  warnings and information messages helpful for troubleshooting. A
  global default can be set via `options(mnirs.verbose = FALSE)`.

- time_channel:

  A character string giving the name of the time or sample column. Must
  match a column name in `data` exactly.

  - If `NULL` (default), the `time_channel` metadata attribute of `data`
    is used.

- event_channel:

  A character string giving the name of the event/marker column. Must
  match a column name in `data` exactly.

  - If `NULL` (default), the `event_channel` metadata attribute of
    `data` is used.

- required:

  Logical. Default is `TRUE`. `event_channel` must be present or
  detected in metadata. If `FALSE`, `event_channel` may be `NULL`.

- sample_rate:

  A numeric sample rate in Hz.

  - If `NULL` (default), the `sample_rate` metadata attribute of `data`
    will be used if detected, or the sample rate will be estimated from
    `time_channel`.

## Value

Returns the validated object (e.g. a resolved `time_channel` string), or
invisibly returns `NULL` for successful validations. On failure, an
error is thrown via
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).

## Details

`validate_mnirs()` is an internal documentation topic for a set of
validators used throughout the package. These validators:

- Prefer explicit user-supplied arguments.

- Fall back to *"mnirs"* metadata attributes when available.

- Fail fast with informative
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  messages when values are missing or invalid.
