# Calculate rolling slope

Computes rolling linear regression slopes within a local window along a
numeric vector.

## Usage

``` r
rolling_slope(
  x,
  t = seq_along(x),
  width = NULL,
  span = NULL,
  align = c("centre", "left", "right"),
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

  An *optional* numeric vector of the predictor variable; time or sample
  number. *Defaults* to indices of `t = seq_along(x)`.

- width:

  An integer defining the local window in number of samples around `idx`
  in which to perform the operation, according to `align`.

- span:

  A numeric value defining the local window timespan around `idx` in
  which to perform the operation, according to `align`. In units of
  `time_channel` or `t`.

- align:

  Window alignment as *"centre"/"center"* (the *default*), *"left"*, or
  *"right"*. Where *"left"* is *forward looking*, and *"right"* is
  *backward looking* from the current sample.

- partial:

  A logical specifying whether to perform the operation over a subset of
  available data within the local rolling window (`TRUE`), or requiring
  a complete window of valid samples (`FALSE`, by *default*). See
  *Details*.

- na.rm:

  A logical indicating whether missing values should be ignored
  (`TRUE`). Otherwise `FALSE` (the *default*) will return `NA` when
  there are any missing data within the vector.

- verbose:

  A logical to display (the *default*) or silence (`FALSE`) warnings and
  information messages used for troubleshooting.

- ...:

  Additional arguments.

## Value

A numeric vector of rolling local slopes in units of `x/t` the same
length as `x`.

## Details

The local rolling window can be specified by either `width` as the
number of samples, or `span` as the timespan in units of `t`. Specifying
`width` tries to call
[`roll::roll_lm()`](https://rdrr.io/pkg/roll/man/roll_lm.html) if
`na.rm = TRUE` or there are no `NA`s, which is often *much* faster than
specifying `span`.

*`<CAUTION>`*, under certain edge-conditions the
[`roll::roll_lm()`](https://rdrr.io/pkg/roll/man/roll_lm.html) method
may return slightly different values than the equivalent specifying
`span`.

`align` defaults to *"centre"* the local window around `idx` between
`[idx - floor((width-1)/2),` `idx + floor(width/2)]` when `width` is
specified. Even `width` values will bias `align` to *"left"*, with the
unequal sample forward of `idx`. When `span` is specified with
`align = "centre"`, the local window is between
`[t - span/2, t + span/2]`.

`partial = TRUE` allows calculation of slope over partial windows with
at least `2` valid samples, such as at edge conditions. However, this
can return unstable results with noisy data and should not be used for
certain applications, such as peak slope detection over a vector of
noisy data.

`na.rm = TRUE` will return a valid slope value as long as there are a
minimum number of valid samples within the window (at least `2` when
`partial = TRUE`). Otherwise, a single `NA` sample in the window will
return `NA`.

## See also

[`zoo::rollapply()`](https://rdrr.io/pkg/zoo/man/rollapply.html),
[`roll::roll_lm()`](https://rdrr.io/pkg/roll/man/roll_lm.html)

## Examples

``` r
x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
rolling_slope(x, span = 3)
#>  [1] 2.0 0.5 1.0 3.0 1.0 0.5 2.5 1.0 1.5 1.5 1.0 2.0 1.0
rolling_slope(x, width = 3)
#>  [1]  NA 0.5 1.0 3.0 1.0 0.5 2.5 1.0 1.5 1.5 1.0 2.0  NA

x_na <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
rolling_slope(x_na, span = 3)
#>  [1] 2.0  NA  NA  NA 1.0 0.5 2.5  NA  NA  NA  NA  NA 1.0
rolling_slope(x_na, span = 3, partial = TRUE)
#>  [1] 2.0  NA  NA  NA 1.0 0.5 2.5  NA  NA  NA  NA  NA 1.0
rolling_slope(x_na, span = 3, partial = TRUE, na.rm = TRUE)
#>  [1] 2.0 2.0 1.0 3.0 1.0 0.5 2.5 3.0  NA  NA  NA 1.0 1.0
rolling_slope(x_na, width = 3, partial = TRUE)
#>  [1] 2.0  NA  NA  NA 1.0 0.5 2.5  NA  NA  NA  NA  NA 1.0
rolling_slope(x_na, width = 3, partial = TRUE, na.rm = TRUE)
#>  [1] 2.0 2.0 1.0 3.0 1.0 0.5 2.5 3.0  NA  NA  NA 1.0 1.0
```
