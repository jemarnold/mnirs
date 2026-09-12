# Estimate baseline and asymptote from the first/last quintile of `x`

Shared helper used by self-start initialisers for logistic / Gompertz
model families.

## Usage

``` r
init_asymptotes(x, n = length(x))
```

## Arguments

- x:

  A numeric vector of the response variable (sorted by `t`).

- n:

  An integer length of `x`.

## Value

A list with elements `A` (starting asymptote estimate) and `B` (ending
asymptote estimate).
