# Sigmoidal-drift model with gradient

Model function of
[`SSsigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/SSsigmoidal_drift.md):
[`sigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/sigmoidal_drift.md)
plus the partial derivatives for the parameters written as bare symbols
in the call (see
[`free_params()`](https://jemarnold.github.io/mnirs/reference/free_params.md)),
so [`stats::nls()`](https://rdrr.io/r/stats/nls.html) skips
[`stats::numericDeriv()`](https://rdrr.io/r/stats/numericDeriv.html).
The sigmoid partials come from
[`sigmoid_core()`](https://jemarnold.github.io/mnirs/reference/sigmoid_core.md);
the drift onset `xmid + u_f / k` moves with every sigmoid parameter
through the rate `k`, and the hinge derivatives are one-sided at the
onset.

## Usage

``` r
sigdrift_model(
  t,
  A,
  B,
  xmid,
  slope,
  slope_B,
  drift_fraction,
  shape = "symmetric"
)
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

- slope_B:

  A numeric parameter for the linear drift rate `dx/dt` of the secondary
  phase at the ending asymptote `B`, in response units per unit of the
  predictor variable `t`.

- drift_fraction:

  A numeric fraction of the primary amplitude `B - A` in `(0.5, 1)` at
  which the linear drift begins, where the sigmoid reaches
  `A + drift_fraction * (B - A)`.

- shape:

  Character; the 4-parameter sigmoidal shape. One of `"symmetric"`
  (*default*;
  [`logistic()`](https://jemarnold.github.io/mnirs/reference/logistic.md)),
  `"gompertz"`
  ([`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)),
  or `"gompertz_left"`
  ([`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)).

## Value

A numeric vector of predicted values with a `"gradient"` attribute when
any parameter is free.
