# Wrap a self-start initialiser to support fixed parameters

Decorates a `selfStart` `initial` function so parameters supplied as
values in the model formula (e.g. `SSmonoexponential(t, A = 0, B, tau)`)
are excluded from the returned start vector.
[`stats::nls()`](https://rdrr.io/r/stats/nls.html) reads the free
parameters from the names of that vector, so excluded parameters are
treated as constants in the formula. Fixed values are forwarded to the
wrapped initialiser as a `fixed` list argument to seed the remaining
free estimates.

## Usage

``` r
init_fixed(init, params)
```

## Arguments

- init:

  A `selfStart` initial function `(mCall, data, LHS, ...)`.

- params:

  Character vector of the model parameter names.

## Value

A function suitable for the `initial` argument of
[`stats::selfStart()`](https://rdrr.io/r/stats/selfStart.html).
