# Initiate self-starting sigmoidal-drift model

`sigdrift_init()`: Returns initial values for the parameters in a
`selfStart` model. The `shape` written in the model call seeds the
matching sigmoid (`"symmetric"` when absent).

## Usage

``` r
sigdrift_init(mCall, data, LHS, ...)
```

## Arguments

- mCall:

  A matched call to the function `model`.

- data:

  A data frame with predictor `t` and the response variable.

- LHS:

  The left-hand side expression of the model formula.

- ...:

  Additional arguments, including `fixed`, a named list of user-fixed
  parameter values from
  [`init_fixed()`](https://jemarnold.github.io/mnirs/reference/init_fixed.md)
  used to seed the remaining free estimates.

## Value

`sigdrift_init()`: Initial starting estimates for parameters in the
model called by
[`SSsigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/SSsigmoidal_drift.md).
