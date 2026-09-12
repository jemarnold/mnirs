# Fit a sigmoidal-drift model to one channel

Channel fitter of
[`analyse_sigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/analyse_sigmoidal_drift.md)
(see
[`analyse_kinetics_channels()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics_channels.md)).
Self-starting
[`SSsigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/SSsigmoidal_drift.md)
of the channel `shape` via
[`stats::nls()`](https://rdrr.io/r/stats/nls.html) with
`algorithm = "port"`, seeded by
[`sigdrift_start()`](https://jemarnold.github.io/mnirs/reference/sigdrift_start.md),
with the requested `direction` enforced on `B - A` and the sign of
`slope`
([`enforce_direction()`](https://jemarnold.github.io/mnirs/reference/enforce_direction.md)).

## Usage

``` r
fit_sigmoidal_drift(x, t, valid, .a, ctx)
```

## Arguments

- x, t:

  Numeric vectors of the channel response and time elapsed from
  `start_time`.

- valid:

  The
  [`find_kinetics_idx()`](https://jemarnold.github.io/mnirs/reference/find_kinetics_idx.md)
  result for the channel.

- .a:

  The resolved argument list of the channel.

- ctx:

  The channel context list of
  [`analyse_kinetics_channels()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics_channels.md).

## Value

The `coefs`/`model`/`fitted_data`/`diag` list of
[`build_fit_results()`](https://jemarnold.github.io/mnirs/reference/build_fit_results.md),
or
[`build_na_results()`](https://jemarnold.github.io/mnirs/reference/build_na_results.md)
when the fit fails.
