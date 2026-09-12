# Analyse peak linear slope across NIRS channels

Internal channel-level dispatch for
`analyse_kinetics(method = "peak_slope")`. Computes the maximum local
linear slope for each `nirs_channel` within a single *"mnirs"* data
frame. See
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
for user-facing documentation.

## Usage

``` r
analyse_peak_slope(
  data,
  nirs_channels = NULL,
  time_channel = NULL,
  start_time = NULL,
  width = NULL,
  span = NULL,
  align = c("centre", "left", "right"),
  direction = c("auto", "positive", "negative"),
  end_window = Inf,
  partial = FALSE,
  na.rm = FALSE,
  verbose = TRUE,
  ...,
  env = rlang::caller_env()
)
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing time series data and
  metadata.

- nirs_channels:

  A character vector giving the names of mNIRS columns to operate on.
  Must match column names in `data` exactly.

  - If `NULL` (default), the `nirs_channels` metadata attribute of
    `data` is used.

- time_channel:

  A character string naming the time or sample column. Must match a
  column name in `data` exactly.

  - If `NULL` (default), the `time_channel` metadata attribute of `data`
    is used.

- start_time:

  A numeric value in units of `time_channel` specifying the response
  onset (effectively time = `0` of the fit). If `NULL` (*default*),
  retrieves `interval_times` from *"mnirs"* metadata, or falls back to
  `0` or the first positive time value (see *Details*).

- width:

  An integer defining the local window in number of samples around `idx`
  in which to perform the operation, according to `align`.

- span:

  A numeric value defining the local window time span around `idx` in
  which to perform the operation, according to `align`. In units of
  `time_channel` or `t`.

- align:

  Window alignment as *"centre"/"center"* (the *default*), *"left"*, or
  *"right"*. Where *"left"* is *forward looking*, and *"right"* is
  *backward looking* from the current sample.

- direction:

  A character string specifying the response direction `"positive"`, or
  `"negative"`, or detect with `"auto"` (*default*). See *Details*.

- end_window:

  A numeric value in units of `time_channel` specifying the window in
  which to look for the end of the kinetics fit; with no greater/ lesser
  values within `end_window` after the first extrema (min/max).
  `end_window = Inf` (*default*) returns the global extreme from the
  full data range (see *Details*).

  For *"biexponential"*, `end_window` bounds the fast-phase window only,
  and the default is `30` sec; the full model is then fit to the full
  data range.

- partial:

  Logical; default is `FALSE`, only returns values where a full window
  of valid (non-`NA`) samples are available. If `TRUE`, ignores `NA` and
  processes available valid samples (see *Details*).

- na.rm:

  Logical; default is `FALSE`, propagates any `NA`s to the returned
  vector. If `TRUE`, ignores `NA`s and processes available valid samples
  within the local window. May return errors or warnings. (see
  *Details*).

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- ...:

  Additional arguments.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A `data.frame` with one row per `nirs_channel` and columns
`nirs_channels`, `slope`, `intercept`, `y`, `peak_slope_time`, `idx`.
Per-channel metadata are attached as attributes:

- `"model"`: a linear regression model object via
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

- `"fitted_data"`: a named list of per-channel data frames with columns
  `window_idx` and `fitted`.

- `"diagnostics"`: a `data.frame` with one row per `nirs_channel`
  containing model fit diagnostics.

- `"channel_args"`: a `data.frame` with one row per `nirs_channel`
  recording the resolved arguments used.

## See also

[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md),
[`peak_slope()`](https://jemarnold.github.io/mnirs/reference/peak_slope.md)
