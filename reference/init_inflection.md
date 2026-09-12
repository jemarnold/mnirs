# Estimate inflection point from a smoothed first derivative

Shared helper that locates the empirical inflection (peak of `|dx/dt|`
after smoothing) and returns the corresponding `xmid` and `slope`
initial values. Falls back to the half-response point and a mean-rate
slope when the derivative is degenerate.

## Usage

``` r
init_inflection(x, t, A_init, B_init)
```

## Arguments

- x:

  A numeric vector of the response variable (sorted by `t`).

- t:

  A numeric vector of the predictor variable.

- A_init:

  Estimated starting asymptote.

- B_init:

  Estimated ending asymptote.

## Value

A list with elements `idx` (integer index into `x`), `xmid` (numeric `t`
value at the inflection), and `slope` (numeric `dx/dt` at the
inflection).
