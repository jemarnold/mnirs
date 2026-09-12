# Build a standardised NA result for a failed channel

Returns the method-specific `coefs`/`model`/`fitted_data`/`diag` list
expected by
[`analyse_kinetics_channels()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics_channels.md)
when a model fit fails, populated with `NA`/`NULL` values.

## Usage

``` r
build_na_results(na_coefs)
```

## Arguments

- na_coefs:

  A template 1-row `data.frame` of `NA` method coefficients (*without*
  `interval`/`nirs_channels`, which are added upstream), or a character
  vector of their column names.

## Value

A named list with elements `coefs`, `model`, `fitted_data`, and `diag`.
