# Alias fit column names that collide with model parameters

[`stats::nls()`](https://rdrr.io/r/stats/nls.html) formula symbols must
be disjoint: a name cannot be both a data column and a parameter. The
fit data frame carries the channel names so the model predicts on them,
so a channel named after a model parameter (or `D`, the amplitude used
by
[`enforce_direction()`](https://jemarnold.github.io/mnirs/reference/enforce_direction.md))
is prefixed with `.` in the fit and in the stored model formula.
Coefficients and results keep the original names.

## Usage

``` r
fit_names(x, t, params)
```

## Arguments

- x, t:

  Character; the response and time channel names.

- params:

  Character vector of the model parameter names.

## Value

A length-2 character vector of the response and time column names to fit
on.
