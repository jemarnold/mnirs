# Assemble a fitted channel result

Counterpart of
[`build_na_results()`](https://jemarnold.github.io/mnirs/reference/build_na_results.md)
for a successful fit: the `coefs`/`model`/`fitted_data`/`diag` list
expected by
[`analyse_kinetics_channels()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics_channels.md),
with fitted values and diagnostics derived from `model` on the rows in
`keep`.

## Usage

``` r
build_fit_results(
  coefs,
  model,
  x_fit,
  t_fit,
  valid,
  keep = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- coefs:

  A 1-row `data.frame` of method coefficients.

- model:

  A fitted model supporting
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html) and
  [`stats::coef()`](https://rdrr.io/r/stats/coef.html).

- x_fit, t_fit:

  Numeric vectors of the channel fit window.

- valid:

  The
  [`find_kinetics_idx()`](https://jemarnold.github.io/mnirs/reference/find_kinetics_idx.md)
  result for the channel.

- keep:

  Logical row filter of the fit window used by `model`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A named list with elements `coefs`, `model`, `fitted_data`, and `diag`.
