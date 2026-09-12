# Combine fitted and fixed coefficients into the full parameter vector

Combine fitted and fixed coefficients into the full parameter vector

## Usage

``` r
full_coefs(model, params, fix = list())
```

## Arguments

- model:

  An [nls](https://rdrr.io/r/stats/nls.html) model of the free
  parameters.

- params:

  Character vector of parameter names in model order.

- fix:

  Named list of fixed parameter values. Non-numeric elements (e.g. a
  model `shape` riding in the formula) are ignored.

## Value

A named numeric vector ordered by `params` containing the fitted
coefficients with fixed values merged in.
