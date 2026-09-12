# Analyse fractional kinetics response time across NIRS channels

Internal channel-level dispatch for
`analyse_kinetics(method = "response_time")`. Computes the fractional
response time for each `nirs_channel` within a single *"mnirs"* data
frame. See
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
for user-facing documentation.

## Usage

``` r
analyse_response_time(
  data,
  nirs_channels = NULL,
  time_channel = NULL,
  start_time = NULL,
  response_fraction = 0.5,
  direction = c("auto", "positive", "negative"),
  end_window = Inf,
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

- response_fraction:

  **response_time**: A numeric vector in the range `[0, 1]` specifying
  the fractional response amplitude(s) to detect. Defaults to `0.5` (50%
  response, i.e. half-response time). Multiple values (e.g.
  `c(0.5, 0.632)`) return one coefficient row per fraction.

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

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- ...:

  Additional arguments passed to the underlying method function. See
  *Details*. For the [`stats::nls()`](https://rdrr.io/r/stats/nls.html)
  methods (**monoexponential, exponential_drift, biexponential,
  sigmoidal, sigmoidal_drift**), `control = list()` can be passed to
  [`stats::nls.control()`](https://rdrr.io/r/stats/nls.control.html),
  e.g. `control = list(maxiter = 200)`, applied globally to all channels
  and intervals.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A `data.frame` with one row per `nirs_channel` per `response_fraction`
and columns `nirs_channels`, `response_fraction`, `A`, `B`,
`response_time`, `response_value`, `fitted`, `idx`. Per-channel metadata
are attached as attributes:

- `"model"`: `NULL` (no parametric model is fitted).

- `"fitted_data"`: a named list of per-channel data frames with columns
  `window_idx` and `fitted`, containing the baseline, response, and
  extreme key points.

- `"diagnostics"`: a `data.frame` with one row per `nirs_channel`
  containing model fit diagnostics.

- `"channel_args"`: a `data.frame` with one row per `nirs_channel`
  recording the resolved arguments used.

## See also

[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md),
[`response_time()`](https://jemarnold.github.io/mnirs/reference/response_time.md)
