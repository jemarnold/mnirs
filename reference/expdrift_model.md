# Exponential-drift model with gradient

Model function of
[`SSexponential_drift()`](https://jemarnold.github.io/mnirs/reference/SSexponential_drift.md):
[`exponential_drift()`](https://jemarnold.github.io/mnirs/reference/exponential_drift.md)
plus the partial derivatives for the parameters written as bare symbols
in the call (see
[`free_params()`](https://jemarnold.github.io/mnirs/reference/free_params.md)),
so [`stats::nls()`](https://rdrr.io/r/stats/nls.html) skips
[`stats::numericDeriv()`](https://rdrr.io/r/stats/numericDeriv.html).
The hinge derivatives are one-sided at the drift onset.

## Usage

``` r
expdrift_model(t, A, B, tau, slope_B, drift_fraction, TD = NULL)
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

- slope_B:

  A numeric parameter for the linear drift rate `dx/dt` of the secondary
  phase, in response units per unit of the predictor variable `t`.

- drift_fraction:

  A numeric fraction of the primary amplitude `B - A` in `(0.5, 1)` at
  which the linear drift begins, where the primary response reaches
  `A + drift_fraction * (B - A)`.

- TD:

  A numeric parameter for the *time delay* before the onset of the
  exponential response, in units of the predictor variable `t`. If
  `NULL` (*default*), a 3-parameter model without time delay is used.

## Value

A numeric vector of predicted values with a `"gradient"` attribute when
any parameter is free.
