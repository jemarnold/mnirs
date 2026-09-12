# Initiate self-starting logistic model

`logistic_init()`: Returns initial values for the parameters in a
`selfStart` model.

## Usage

``` r
logistic_init(mCall, data, LHS, ...)
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

`logistic_init()`: Initial starting estimates for parameters in the
model called by
[`SSlogistic()`](https://jemarnold.github.io/mnirs/reference/SSlogistic.md).
