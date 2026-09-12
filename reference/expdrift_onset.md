# Drift onset time of the exponential-drift model

The time at which a monoexponential response reaches the
`drift_fraction` fraction of its amplitude, by the analytic inverse
`TD - tau * log(1 - drift_fraction)` (see
[`exponential_drift()`](https://jemarnold.github.io/mnirs/reference/exponential_drift.md)).

## Usage

``` r
expdrift_onset(tau, drift_fraction, TD = NULL)
```

## Arguments

- tau:

  A numeric parameter for the *time constant* (\\\tau\\) of the
  exponential response, in units of the predictor variable `t`.

- drift_fraction:

  A numeric fraction of the primary amplitude `B - A` in `(0.5, 1)` at
  which the linear drift begins, where the primary response reaches
  `A + drift_fraction * (B - A)`.

- TD:

  A numeric parameter for the *time delay* before the onset of the
  exponential response, in units of the predictor variable `t`. If
  `NULL` (*default*), a 3-parameter model without time delay is used.

## Value

A numeric vector of onset times, `TD = 0` when `NULL`.
