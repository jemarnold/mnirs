# Initiate self-starting Gompertz model

`gompertz_init()`: Returns initial values for the parameters in a
`selfStart` model. Used by both
[`SSgompertz()`](https://jemarnold.github.io/mnirs/reference/SSgompertz.md)
and
[`SSgompertz_left()`](https://jemarnold.github.io/mnirs/reference/SSgompertz.md);
the symmetric logistic linearisation does not apply to Gompertz forms,
so initialisation is derivative-based via
[`init_inflection()`](https://jemarnold.github.io/mnirs/reference/init_inflection.md).

## Usage

``` r
gompertz_init(mCall, data, LHS, ...)
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

`gompertz_init()`: Initial starting estimates for parameters in the
model called by
[`SSgompertz()`](https://jemarnold.github.io/mnirs/reference/SSgompertz.md)
or
[`SSgompertz_left()`](https://jemarnold.github.io/mnirs/reference/SSgompertz.md).
