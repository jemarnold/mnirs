# Biexponential model with gradient

`biexp_core()` evaluates the curve and its partial derivatives on the
canonical parameters. `biexp_model()` is the model function of
[`SSbiexponential()`](https://jemarnold.github.io/mnirs/reference/SSbiexponential.md):
[`biexponential()`](https://jemarnold.github.io/mnirs/reference/biexponential.md)
plus the gradient for the parameters written as bare symbols in the call
(see
[`free_params()`](https://jemarnold.github.io/mnirs/reference/free_params.md)),
so [`stats::nls()`](https://rdrr.io/r/stats/nls.html) skips
[`stats::numericDeriv()`](https://rdrr.io/r/stats/numericDeriv.html).

## Usage

``` r
biexp_core(t, A, B, tau, B2, tau2, TD = NULL)

biexp_model(t, A, B, tau, B2, tau2, TD = NULL)
```

## Arguments

- t:

  A numeric vector of the predictor variable (time).

- A:

  A numeric parameter for the starting value of the response variable
  (the `t = 0` intercept).

- B:

  A numeric parameter for the asymptote of the *fast* component; the
  value the fast response alone would approach.

- tau:

  A numeric parameter for the *fast* time constant (\\\tau_1\\), in
  units of the predictor variable `t`. Dominates the initial steep
  response.

- B2:

  A numeric parameter for the asymptote of the *slow* component; the
  stable plateau the response recovers toward as `t` approaches
  infinity.

- tau2:

  A numeric parameter for the *slow* time constant (\\\tau_2\\), in
  units of the predictor variable `t`. Typically `tau2 >> tau`.

- TD:

  A numeric parameter for the *time delay* before the onset of the
  response, in units of the predictor variable `t`. If `NULL`
  (*default*), a 5-parameter model without time delay is used.

## Value

`biexp_core()`: a list of the curve `val` and the partial derivatives by
parameter name. `biexp_model()`: a numeric vector of predicted values
with a `"gradient"` attribute when any parameter is free.
