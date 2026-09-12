# Build a self-start model formula with optional fixed parameters

Constructs `x ~ fn(t, ...)` with each free parameter as a bare symbol
and each fixed parameter substituted as its constant value.

## Usage

``` r
build_ss_formula(fn, params, fix = list(), x, t)
```

## Arguments

- fn:

  Symbol; the self-start model function.

- params:

  Character vector of parameter names in `fn` argument order.

- fix:

  Named list of fixed parameter values.

- x, t:

  Character; the response and time column names (see
  [`fit_names()`](https://jemarnold.github.io/mnirs/reference/fit_names.md)),
  so the returned model predicts on the original channel names.

## Value

A two-sided [formula](https://rdrr.io/r/stats/formula.html) on `x` and
`t`.
