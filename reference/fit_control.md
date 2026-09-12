# Merge user nls control over a fit's internal defaults

Merge user nls control over a fit's internal defaults

## Usage

``` r
fit_control(control, ...)
```

## Arguments

- control:

  User `control` list (or `NULL`) from the channel args.

- ...:

  Internal
  [`stats::nls.control()`](https://rdrr.io/r/stats/nls.control.html)
  defaults for the fit.

## Value

A control list for [`stats::nls()`](https://rdrr.io/r/stats/nls.html).
