# Fit a biexponential model to one channel

Channel fitter of
[`analyse_biexponential()`](https://jemarnold.github.io/mnirs/reference/analyse_biexponential.md)
(see
[`analyse_kinetics_channels()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics_channels.md)),
in two stages. Stage 1 fits the fast phase as a monoexponential on the
`end_window` window
([`fit_monoexponential()`](https://jemarnold.github.io/mnirs/reference/fit_monoexponential.md)).
Stage 2 fits the full
[`SSbiexponential()`](https://jemarnold.github.io/mnirs/reference/SSbiexponential.md)
model on the whole response via
[`stats::nls()`](https://rdrr.io/r/stats/nls.html) with
`algorithm = "port"`, `A`, `tau`, and `TD` box-bounded about their
stage-1 values by the `*_flex` half-widths and `B`, `B2`, `tau2` free,
seeded by
[`biexp_start()`](https://jemarnold.github.io/mnirs/reference/biexp_start.md)
with the fast phase held. A failed stage returns `NA`, and the fallback
chain resolves the row upstream.

## Usage

``` r
fit_biexponential(x, t, valid, .a, ctx)
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
when a stage fails.
