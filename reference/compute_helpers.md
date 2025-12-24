# Computes rolling local values

`compute_local_windows()`: Compute a list of rolling window indices
along a time variable `t`.

`compute_local_fun()`: Compute a rolling function along `x` from a list
of rolling sample windows.

`compute_outliers()`: Computes a vector of logicals indicating local
outliers of `x` within a list of rolling sample windows `window_idx`.

`compute_valid_neighbours()`: Compute a list of rolling window indices
along `x` to either side of `NA`s.

`rolling_median()`: Compute rolling median using
[roll](https://rdrr.io/pkg/roll/man/roll-package.html) with configurable
alignment

`rolling_mean()`: Compute rolling mean using
[roll](https://rdrr.io/pkg/roll/man/roll-package.html) with configurable
alignment

`rolling_lm()`: Compute rolling linear regression slopes using
[roll](https://rdrr.io/pkg/roll/man/roll-package.html) with configurable
alignment

## Usage

``` r
compute_local_windows(
  t,
  idx = seq_along(t),
  width = NULL,
  span = NULL,
  align = c("center", "left", "right")
)

compute_local_fun(x, window_idx, fn, ...)

compute_outliers(x, window_idx, local_medians, outlier_cutoff)

compute_valid_neighbours(
  x,
  t = seq_along(x),
  width = NULL,
  span = NULL,
  verbose = TRUE
)

rolling_median(x, width, align = c("center", "left", "right"))

rolling_mean(x, width, align = c("center", "left", "right"))

rolling_lm(
  x,
  t = seq_along(x),
  width,
  align = c("center", "left", "right"),
  min_obs = width,
  verbose = TRUE,
  ...
)
```

## Arguments

- t:

  An *optional* numeric vector of the predictor variable; time or sample
  number. *Defaults* to indices of `t = seq_along(x)`.

- idx:

  A numeric vector of indices of `t` at which to calculate local
  windows. All indices of `t` by *default*, or can be used to only
  calculate for known indicies, such as invalid values of `x`.

- width:

  An integer defining the local window in number of samples around `idx`
  in which to perform the operation, according to `align`.

- span:

  A numeric value defining the local window timespan around `idx` in
  which to perform the operation, according to `align`. In units of
  `time_channel` or `t`.

- align:

  Window alignment as *"center"* (the *default*), *"left"*, or
  *"right"*. Where *"left"* is *forward looking*, and *"right"* is
  *backward looking* from the current sample.

- x:

  A numeric vector of the response variable.

- window_idx:

  A list the same or shorter length as `x` with numeric vectors for the
  sample indices of local rolling windows.

- fn:

  A function to pass through for local rolling calculation.

- ...:

  Additional arguments.

- local_medians:

  A numeric vector the same length as `x` of local median values.

- outlier_cutoff:

  An integer for the local outlier threshold, as number of standard
  deviations above and below the local median. The *default*
  `outlier_cutoff = NULL` will not replace outliers.
  `outlier_cutoff = 3` is the standard replacement threshold following
  Pearson's rule.

- verbose:

  A logical to display (the *default*) or silence (`FALSE`) warnings and
  information messages used for troubleshooting.

- min_obs:

  An integer specifying the minimum number of valid samples required to
  return a value within a window, otherwise will return `NA`.

## Value

`compute_local_windows()`: A list the same length as `idx` and the same
or shorter length as `t` with numeric vectors of sample indices of
length `width` samples or `span` units of time `t`.

`compute_local_fun()`: A numeric vector the same length as `x`.

`compute_outliers()`: A logical vector the same length as `x`.

`compute_valid_neighbours()`: A list the same length as the `NA` values
in `x` with numeric vectors of sample indices of length `width` samples
or `span` units of time `t` for valid values neighbouring split to
either side of the invalid `NA`s.

`rolling_median()`: A numeric vector of local median values, the same
length as `x`.

`rolling_mean()`: A numeric vector of local mean values, the same length
as `x`.

`rolling_lm()`: A numeric vector of local linear regression slopes, the
same length as `x`.

## Details

The local rolling window can be specified by either `width` as the
number of samples, or `span` as the timespan in units of `t`. Specifying
`width` calls [roll](https://rdrr.io/pkg/roll/man/roll-package.html)
which is often much faster than specifying `span`.

`align` defaults to *"center"* the local window around `idx` between
`[idx - floor((width-1)/2),` `idx + floor(width/2)]` when `width` is
specified. Even `width` values will bias `align` to *"left"*, with the
unequal sample forward of `idx`, effectively returning `NA` at the last
sample index. When `span` is specified, the local window is between
`[t - span/2, t + span/2]`.

Currently used functions are
`c([stats::median()], [base::mean], [slope()])`.

## Examples

``` r
x <- c(1, 2, 3, 100, 5)
t <- seq_along(x)

## a list of numeric vectors of rolling local windows along `t`
(window_idx <- mnirs:::compute_local_windows(t, width = 2, span = NULL))
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 2 3
#> 
#> [[3]]
#> [1] 3 4
#> 
#> [[4]]
#> [1] 4 5
#> 
#> [[5]]
#> [1] 5
#> 

## a numeric vector of local medians of `x`
(local_medians <- mnirs:::compute_local_fun(x, window_idx, median))
#> [1]  1.5  2.5 51.5 52.5  5.0

## a logical vector of local outliers of `x`
(is.outlier <- mnirs:::compute_outliers(x, window_idx, local_medians, outlier_cutoff = 3L))
#> [1] FALSE FALSE FALSE FALSE FALSE

## a list of numeric vectors of local windows of valid values of `x` neighbouring `NA`s.
x <- c(1, 2, 3, NA, NA, 6)
(window_idx <- mnirs:::compute_valid_neighbours(x, width = 2))
#> [[1]]
#> [1] 3 6
#> 
#> [[2]]
#> [1] 3 6
#> 

(local_medians <- mnirs:::compute_local_fun(
    x, window_idx, median, na.rm = TRUE)
)
#> [1] 4.5 4.5

x[is.na(x)] <- local_medians
x
#> [1] 1.0 2.0 3.0 4.5 4.5 6.0
```
