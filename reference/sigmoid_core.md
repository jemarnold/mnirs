# Sigmoid curve with gradient

`sigmoid_core()` evaluates a 4-parameter sigmoid of the given `shape`
and its partial derivatives on the canonical parameters, shared by the
`selfStart` model functions of
[`SSlogistic()`](https://jemarnold.github.io/mnirs/reference/SSlogistic.md),
[`SSgompertz()`](https://jemarnold.github.io/mnirs/reference/SSgompertz.md),
[`SSgompertz_left()`](https://jemarnold.github.io/mnirs/reference/SSgompertz.md),
and
[`SSsigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/SSsigmoidal_drift.md).
Every shape is a function `W(u)` of `u = k * (t - xmid)` with rate
`k = c * slope / (B - A)` (`c = 4` symmetric, `e` Gompertz), so with
`P = dW/du` the partials share one form. `sigmoid_model()` attaches the
gradient over the parameters written as bare symbols in `mCall` (see
[`free_params()`](https://jemarnold.github.io/mnirs/reference/free_params.md)),
so [`stats::nls()`](https://rdrr.io/r/stats/nls.html) skips
[`stats::numericDeriv()`](https://rdrr.io/r/stats/numericDeriv.html).

## Usage

``` r
sigmoid_core(t, A, B, xmid, slope, shape)

sigmoid_model(mCall, t, A, B, xmid, slope, shape)
```

## Arguments

- t:

  A numeric vector of the predictor variable (time).

- A:

  A numeric parameter for the starting asymptote of the response
  variable.

- B:

  A numeric parameter for the ending asymptote of the response variable.

- xmid:

  A numeric parameter for the time at the *inflection point* (the
  steepest point) of the curve, in units of the predictor variable `t`.

- slope:

  A numeric parameter for the response rate `dx/dt` at the inflection
  `xmid`.

- shape:

  Character; the 4-parameter sigmoidal shape. One of `"symmetric"`
  (*default*;
  [`logistic()`](https://jemarnold.github.io/mnirs/reference/logistic.md)),
  `"gompertz"`
  ([`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)),
  or `"gompertz_left"`
  ([`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)).

- mCall:

  A matched call to the model function.

## Value

`sigmoid_core()`: a list of the curve `val`, the partial derivatives by
parameter name, and the rate `k`. `sigmoid_model()`: a numeric vector of
predicted values with a `"gradient"` attribute when any parameter is
free.
