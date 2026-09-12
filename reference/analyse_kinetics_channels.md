# Process kinetics fits across NIRS channels

Shared per-channel skeleton for all
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
methods. For each channel, resolves the fitting window via
[`find_kinetics_idx()`](https://jemarnold.github.io/mnirs/reference/find_kinetics_idx.md),
delegates the method-specific fit to `fit_fn`, and, for methods listed
in `kinetics_fallbacks`, tests the fit with the method's `trigger`. A
channel with a reason is refit by the reduced method (recursively down
the chain) with the arguments it takes, the spec's overrides, and the
user-fixed parameters it shares; the fallback is warned about and so
recorded in the `warnings` attribute. A row where every fit in the chain
failed reports the last method tried with `NA` coefficients.

## Usage

``` r
analyse_kinetics_channels(
  data,
  nirs_channels,
  time_channel,
  per_channel,
  fit_fn,
  verbose = TRUE,
  interval_name = NA_character_,
  extra_args = list(),
  method = NULL,
  fallback = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- data:

  A single *"mnirs"* data frame.

- nirs_channels:

  Character vector of resolved channel names.

- time_channel:

  Character; resolved time column name.

- per_channel:

  Named list (one element per channel) of resolved and validated
  argument lists from
  [`resolve_channel_args()`](https://jemarnold.github.io/mnirs/reference/resolve_channel_args.md)
  and
  [`validate_kinetics_args()`](https://jemarnold.github.io/mnirs/reference/validate_kinetics_args.md).

- fit_fn:

  A channel fitter `(x, t, valid, .a, ctx)` taking the channel's full
  response `x` and time `t` elapsed from `start_time`, the
  [`find_kinetics_idx()`](https://jemarnold.github.io/mnirs/reference/find_kinetics_idx.md)
  window `valid`, the channel's resolved argument list `.a`, and a `ctx`
  list of `nirs`, `time_channel`, `interval_name`, and `env`. Returns a
  list with `coefs` (1-row data frame of method coefficients, *without*
  `interval`/`nirs_channels`), `model`, `fitted_data`
  (`window_idx`/`fitted`, indexing the original data frame rows), and
  `diag` (1-row data frame from
  [`compute_diagnostics()`](https://jemarnold.github.io/mnirs/reference/compute_diagnostics.md));
  see
  [`build_fit_results()`](https://jemarnold.github.io/mnirs/reference/build_fit_results.md).
  Fallback fitters are resolved from `kinetics_fitters`.

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- interval_name:

  Character; the interval name recorded in the `interval` column of the
  returned coefficients, `diagnostics`, and `channel_args`.

- extra_args:

  Named list of additional arguments recorded in the `channel_args`
  result attribute.

- method:

  Character; the canonical method name keying `kinetics_fallbacks`, or
  `NULL` for methods without a chain.

- fallback:

  Logical; resolve the fallback chain. `FALSE` keeps the raw fit of
  `method`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A `data.frame` of coefficients (columns `interval`, `nirs_channels`,
`model` for chained methods, and method parameters), one row per
channel, with attributes `"time_channel"` (the resolved time column
name), `"model"` and `"fitted_data"` (named lists by channel),
`"diagnostics"` and `"channel_args"` (data frames, one row per channel),
and `"warnings"` (data frame of conditions captured during fitting,
regardless of `verbose`; zero rows when none fire).

## Details

Methods with a fallback report the fitting method per row in a `model`
coefficient column and the union of the chain's coefficient columns
(`NA` where a model has no such parameter), so intervals bind regardless
of which triggers fire.
[`build_kinetics_results()`](https://jemarnold.github.io/mnirs/reference/build_kinetics_results.md)
then drops the columns of fallback models no row resolved to.
