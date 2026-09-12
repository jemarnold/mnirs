# Monoexponential model with gradient

Model function of
[`SSmonoexponential()`](https://jemarnold.github.io/mnirs/reference/SSmonoexponential.md):
[`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md)
plus the partial derivatives for the parameters written as bare symbols
in the call (see
[`free_params()`](https://jemarnold.github.io/mnirs/reference/free_params.md)),
so [`stats::nls()`](https://rdrr.io/r/stats/nls.html) skips
[`stats::numericDeriv()`](https://rdrr.io/r/stats/numericDeriv.html) and
a parameter fixed as a constant in the formula contributes no gradient
column.

## Usage

``` r
monoexp_model(t, A, B, tau, TD = NULL)
```

## Arguments

- t:

  A numeric vector of the predictor variable (time).

- A:

  A numeric parameter for the starting baseline of the response
  variable.

- B:

  A numeric parameter for the ending asymptote of the response variable.

- tau:

  A numeric parameter for the *time constant* (\\\tau\\) of the
  exponential response, in units of the predictor variable `t`.

- TD:

  A numeric parameter for the *time delay* before the onset of the
  exponential response, in units of the predictor variable `t`. If
  `NULL` (*default*), a 3-parameter model without time delay is used.

## Value

A numeric vector of predicted values with a `"gradient"` attribute when
any parameter is free.
