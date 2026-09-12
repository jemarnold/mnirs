# Initiate self-starting exponential-drift model

`expdrift_init()`: Returns initial values for the parameters in a
`selfStart` model.

## Usage

``` r
expdrift_init(mCall, data, LHS, ...)
```

## Arguments

- mCall:

  A matched call to the function `model`.

- data:

  A data frame with time `t` and the response variable.

- LHS:

  The left-hand side expression of the model formula.

- ...:

  Additional arguments, including `fixed`, a named list of user-fixed
  parameter values from
  [`init_fixed()`](https://jemarnold.github.io/mnirs/reference/init_fixed.md)
  used to seed the remaining free estimates.

## Value

`expdrift_init()`: Initial starting estimates for parameters in the
model called by
[`SSexponential_drift()`](https://jemarnold.github.io/mnirs/reference/SSexponential_drift.md).
