# Fit a monoexponential model to one channel

Channel fitter of
[`analyse_monoexponential()`](https://jemarnold.github.io/mnirs/reference/analyse_monoexponential.md)
(see
[`analyse_kinetics_channels()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics_channels.md)),
also the fast-phase (stage 1) fit of
[`fit_biexponential()`](https://jemarnold.github.io/mnirs/reference/fit_biexponential.md)
and the fallback of
[`fit_exponential_drift()`](https://jemarnold.github.io/mnirs/reference/fit_exponential_drift.md).
Self-starting
[`SSmonoexponential()`](https://jemarnold.github.io/mnirs/reference/SSmonoexponential.md)
via [`stats::nls()`](https://rdrr.io/r/stats/nls.html); a failed
4-parameter fit falls back to the 3-parameter model
([`fit_td_fallback()`](https://jemarnold.github.io/mnirs/reference/fit_td_fallback.md)),
and the requested `direction` is enforced on `B - A`
([`enforce_direction()`](https://jemarnold.github.io/mnirs/reference/enforce_direction.md)).

## Usage

``` r
fit_monoexponential(x, t, valid, .a, ctx)
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
