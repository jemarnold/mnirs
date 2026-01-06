# Find peak linear slope

Identifies the maximum positive or negative local linear slope within a
numeric vector and returns regression parameters

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
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A numeric vector of the response variable.

- t:

  An *optional* numeric vector of the predictor variable; time or sample
  number. *Defaults* to indices of `t = seq_along(x)`.

- width:

  An integer defining the local window in number of samples around `idx`
  in which to perform the operation., between
  `[idx - floor(width/2), idx + floor(width/2)]`.

- span:

  A numeric value defining the local window timespan around `idx` in
  which to perform the operation. In units of `time_channel` or `t`,
  between `[t - span/2, t + span/2]`.

- align:

  Window alignment as *"centre"/"center"* (the *default*), *"left"*, or
  *"right"*. Where *"left"* is *forward looking*, and *"right"* is
  *backward looking* from the current sample.

- direction:

  A character string to detect either the peak `"positive"` or
  `"negative"` slope, or `"auto"` detect (the *default*) based on the
  overal trend of the signal (see *Details*).

- partial:

  A logical specifying whether to perform the operation over a subset of
  available data within the local rolling window (`TRUE`), or requiring
  a complete window of valid samples (`FALSE`, by *default*). See
  *Details*.

- verbose:

  A logical to display (the *default*) or silence (`FALSE`) warnings and
  information messages used for troubleshooting.

- ...:

  Additional arguments.

## Value

A named list containing:

- `slope`:

  The peak slope value in units of `x/t`.

- `intercept`:

  The y-intercept of the peak local regression equation.

- `y`:

  The response value predicted from `x` at `t`.

- `t`:

  The time value at the index of the peak slope window.

- `idx`:

  The index position of the peak slope.

- `window_idx`:

  An integer vector of indices for the peak slope window.

## Details

Uses rolling slope calculations via the least squares formula on
complete case data. The local rolling window can be specified by either
`width` as the number of samples, or `span` as the timespan in units of
`t`.

`align` defaults to *"centre"* the local window around `idx` between
`[idx - floor((width-1)/2),` `idx + floor(width/2)]` when `width` is
specified. Even `width` values will bias `align` to *"left"*, with the
unequal sample forward of `idx`. When `span` is specified with
`align = "centre"`, the local window is between
`[t - span/2, t + span/2]`.

When `direction = "auto"`, the net slope across all of `x` is calculated
to determine the trend direction (positive or negative), then the
greatest local slope in that direction is returned. If the net slope
equals zero, will return the greatest absolute local slope. When
`direction = "positive"` or `"negative"`, returns the greatest
respective directional slope. If no positive/negative slopes exist,
returns `NA` with a warning.

The default `partial = FALSE` requires complete case data with the same
number of valid samples as specified by `width` or `span` (number of
samples is estimated for `span` from the sample rate of `t`). If fewer
than the requires valid samples are present in the local vector, `NA` is
returned.

`partial = TRUE` allows calculation over partial windows with at least
`2` valid samples, such as at edge conditions or over missing data
`NA`s. However, these slope values will be sensitive to noisy data, so
use with caution.

## Examples

``` r
x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
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
#> $window_idx
#> [1]  6  7  8  9 10
#> 

x_dec <- rev(x)
peak_slope(x_dec, width = 5)
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
#> $window_idx
#> [1] 4 5 6 7 8
#> 
```
