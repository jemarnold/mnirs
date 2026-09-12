# Peak linear slope

Identify the maximum positive or negative local linear slope of a
numeric vector using rolling least-squares regression, and return the
regression parameters of the peak window. Vector-level companion to
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
with `method = "peak_slope"`.

## Usage

``` r
peak_slope(
  x,
  t = seq_along(x),
  width = NULL,
  span = NULL,
  align = c("centre", "left", "right"),
  direction = c("auto", "positive", "negative"),
  partial = FALSE,
  na.rm = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A numeric vector of the response variable.

- t:

  An *optional* numeric vector of the predictor variable (e.g. time).
  Default is `seq_along(x)`.

- width:

  An integer defining the local window in number of samples around `idx`
  in which to perform the operation, according to `align`.

- span:

  A numeric value defining the local window time span around `idx` in
  which to perform the operation, according to `align`. In units of
  `time_channel` or `t`.

- align:

  Window alignment as *"centre"/"center"* (the *default*), *"left"*, or
  *"right"*. Where *"left"* is *forward looking*, and *"right"* is
  *backward looking* from the current sample.

- direction:

  A character string specifying the response direction `"positive"`, or
  `"negative"`, or detect with `"auto"` (*default*). See *Details*.

- partial:

  Logical; default is `FALSE`, only returns values where a full window
  of valid (non-`NA`) samples are available. If `TRUE`, ignores `NA` and
  processes available valid samples (see *Details*).

- na.rm:

  Logical; default is `FALSE`, propagates any `NA`s to the returned
  vector. If `TRUE`, ignores `NA`s and processes available valid samples
  within the local window. May return errors or warnings. (see
  *Details*).

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- ...:

  Additional arguments.

## Value

A named list containing:

- `slope`:

  The peak slope value in units of `x / t`.

- `intercept`:

  The y-intercept of the peak local regression line.

- `y`:

  The predicted value of `x` at the peak slope index.

- `t`:

  The value of `t` at the peak slope index.

- `idx`:

  The integer index of the peak slope window.

- `fitted`:

  A numeric vector of predicted values spanning the peak slope window.

- `window_idx`:

  An integer vector of indices spanning the peak slope window.

- `model`:

  The [lm](https://rdrr.io/r/stats/lm.html) object fit to the peak slope
  window.

## Details

A semi-parametric approach to estimate the steepest local rate of change
of a signal. In NIRS signals this can be interpreted as the moment of
greatest mismatch between oxygen delivery and extraction. Rolling slopes
are computed by
[`rolling_slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md),
and the peak window is refit with
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) to return the
regression parameters.

### Rolling window

The local window is defined by either `width` (number of samples) or
`span` (time span in units of `t`); one of either `width` or `span` must
be specified.

- `width` with `align = "centre"` spans
  `[idx - floor((width - 1) / 2), idx + floor(width / 2)]`. Even `width`
  values bias alignment to *"left"*, placing the unequal sample forward
  of `idx`.

- `span` with `align = "centre"` spans `[t - span / 2, t + span / 2]`.

### Direction

`direction` is detected automatically by default as either *"positive"*
(upward) or *"negative"* (downward) response, from the dominant
excursion of `x` above or below its initial baseline (the median of the
earliest samples). When tied, the greater absolute rolling slope
decides. The greatest local slope in that direction is returned, and
`direction` can be overwritten manually.

### Partial windows

`partial = FALSE` (the *default*) requires the complete number of
samples specified by `width` or `span`, and returns `NA` for any window
with fewer samples. `partial = TRUE` allows computation with as few as 2
valid samples. These windows, such as at edge conditions, are more
sensitive to noise and should be used with caution.

### Missing values

`na.rm = FALSE` (the *default*) propagates any `NA` in a window to the
returned slope. `na.rm = TRUE` ignores `NA`s and computes the slope from
the remaining valid samples.

## See also

[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md),
[`rolling_slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md),
[`response_time()`](https://jemarnold.github.io/mnirs/reference/response_time.md),
[`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md)

## Examples

``` r
x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)

## peak positive slope over a 5-sample window
peak_slope(x, width = 5)
#> $slope
#> [1] 1.8
#> 
#> $intercept
#> [1] -3.6
#> 
#> $y
#> [1] 10.8
#> 
#> $t
#> [1] 8
#> 
#> $idx
#> [1] 8
#> 
#> $fitted
#> [1]  7.2  9.0 10.8 12.6 14.4
#> 
#> $window_idx
#> [1]  6  7  8  9 10
#> 
#> $model
#> 
#> Call:
#> stats::lm(formula = fit_formula, data = data.frame(x = x[window_idx], 
#>     t = t[window_idx]))
#> 
#> Coefficients:
#> (Intercept)            t  
#>        -3.6          1.8  
#> 
#> 

## peak negative slope of the reversed signal
peak_slope(rev(x), width = 5)
#> $slope
#> [1] -1.8
#> 
#> $intercept
#> [1] 21.6
#> 
#> $y
#> [1] 10.8
#> 
#> $t
#> [1] 6
#> 
#> $idx
#> [1] 6
#> 
#> $fitted
#> [1] 14.4 12.6 10.8  9.0  7.2
#> 
#> $window_idx
#> [1] 4 5 6 7 8
#> 
#> $model
#> 
#> Call:
#> stats::lm(formula = fit_formula, data = data.frame(x = x[window_idx], 
#>     t = t[window_idx]))
#> 
#> Coefficients:
#> (Intercept)            t  
#>        21.6         -1.8  
#> 
#> 
```
