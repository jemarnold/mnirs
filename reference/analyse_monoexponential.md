# Analyse monoexponential kinetics across NIRS channels

Internal channel-level dispatch for
`analyse_kinetics(method = "monoexponential")`. Fits a monoexponential
curve to each `nirs_channel` within a single *"mnirs"* data frame via
[`fit_monoexponential()`](https://jemarnold.github.io/mnirs/reference/fit_monoexponential.md).
See
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
for user-facing documentation.

## Usage

``` r
analyse_monoexponential(
  data,
  nirs_channels = NULL,
  time_channel = NULL,
  use_TD = TRUE,
  fix = NULL,
  control = NULL,
  start_time = NULL,
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

- use_TD:

  Logical; default is `TRUE` to attempt to fit a 4-parameter
  [`SSmonoexponential()`](https://jemarnold.github.io/mnirs/reference/SSmonoexponential.md)
  model (A, B, tau, TD) with a time delay. If the 4-parameter fit fails,
  or if `use_TD = FALSE`, attempts to fit a reduced 3-parameter
  [`SSmonoexponential()`](https://jemarnold.github.io/mnirs/reference/SSmonoexponential.md)
  model (A, B, tau).

- fix:

  An *optional* named list of model parameters to hold constant during
  fitting, e.g. `fix = list(A = 0)`. Fixed parameters are excluded from
  estimation and reported at their fixed values. Applied to every
  channel, or per-channel as a list of lists keyed by channel name, e.g.
  `fix = list(smo2 = list(A = 0))`. `TD` is fixable for channels where
  `use_TD = TRUE`; a fixed `TD` disables the 3-parameter fallback.

- control:

  An *optional* [`list()`](https://rdrr.io/r/base/list.html) or
  [`stats::nls.control()`](https://rdrr.io/r/stats/nls.control.html)
  merged over each fit's internal defaults by
  [`fit_control()`](https://jemarnold.github.io/mnirs/reference/fit_control.md).
  Global to all channels.

- start_time:

  A numeric value in units of `time_channel` specifying the response
  onset (effectively time = `0` of the fit). If `NULL` (*default*),
  retrieves `interval_times` from *"mnirs"* metadata, or falls back to
  `0` or the first positive time value (see *Details*).

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

A `data.frame` with one row per `nirs_channel` and columns
`nirs_channels`, `A`, `B`, `TD`, `tau`, `k`, `MRT`, `HRT`, `MRT_fitted`,
`HRT_fitted`. Per-channel metadata are attached as attributes:

- `"model"`: an [nls](https://rdrr.io/r/stats/nls.html) model object, or
  `NULL` for channels where fitting failed.

- `"fitted_data"`: a named list of per-channel data frames with columns
  `window_idx` and `fitted`.

- `"diagnostics"`: a `data.frame` with one row per `nirs_channel`
  containing model fit diagnostics.

- `"channel_args"`: a `data.frame` with one row per `nirs_channel`
  recording the resolved arguments used.

## See also

[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md),
[`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md),
[`SSmonoexponential()`](https://jemarnold.github.io/mnirs/reference/SSmonoexponential.md)
