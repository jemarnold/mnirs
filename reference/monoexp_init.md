# Initiate self-starting monoexponential model

`monoexp_init()`: Returns initial values for the parameters in a
`selfStart` model.

## Usage

``` r
monoexp_init(mCall, data, LHS, ...)
```

## Arguments

- mCall:

  A matched call to the function `model`.

- data:

  A data frame with time `t` and the response variable.

- LHS:

  The left-hand side expression of the model formula.

- ...:

  Additional arguments.

## Value

`monoexp_init()`: Initial starting estimates for parameters in the model
called by
[`SS_monoexp3()`](https://jemarnold.github.io/mnirs/reference/SS_monoexp.md)
and
[`SS_monoexp4()`](https://jemarnold.github.io/mnirs/reference/SS_monoexp.md).
