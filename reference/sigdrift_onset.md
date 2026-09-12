# Drift onset time of the sigmoidal-drift model

The time at which a sigmoid of the given `shape` reaches the
`drift_fraction` fraction of its amplitude, by the analytic inverse of
each shape (see
[`sigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/sigmoidal_drift.md)).
Vectorised over the numeric parameters; `shape` is a single string.

## Usage

``` r
sigdrift_onset(A, B, xmid, slope, drift_fraction, shape)
```

## Arguments

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

A numeric vector of onset times.
