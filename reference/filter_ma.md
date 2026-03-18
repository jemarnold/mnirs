# Apply a moving average filter

Apply a simple moving average smoothing filter to vector data.
`filter_moving_average()` is an alias of `filter_ma()`.

## Usage

``` r
filter_ma(
  x,
  t = seq_along(x),
  width = NULL,
  span = NULL,
  partial = FALSE,
  na.rm = FALSE,
  verbose = TRUE,
  ...
)

filter_moving_average(
  x,
  t = seq_along(x),
  width = NULL,
  span = NULL,
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

  An *optional* numeric vector of the predictor variable (time or sample
  number). Default is `seq_along(x)`.

- width:

  An integer defining the local window in number of samples centred on
  `idx`, between `[idx - floor(width/2), idx + floor(width/2)]`.

- span:

  A numeric value defining the local window timespan around `idx` in
  units of `time_channel` or `t`, between `[t - span/2, t + span/2]`.

- partial:

  A logical specifying whether to perform the operation over a subset of
  available data within the local rolling window (`TRUE`), or requiring
  a complete window of valid samples (`FALSE`, by *default*). See
  *Details*.

- na.rm:

  A logical indicating whether missing values should be preserved and
  passed through the filter (`TRUE`). Otherwise `FALSE` (the *default*)
  will throw an error if there are any `NA`s (see *Details*).

- verbose:

  Logical. Default is `TRUE`. Will display or silence (if `FALSE`)
  warnings and information messages helpful for troubleshooting. A
  global default can be set via `options(mnirs.verbose = FALSE)`.

- ...:

  Additional arguments.

## Value

A numeric vector the same length as `x`.

## Details

Applies a centred (symmetrical) moving average filter in a local window
defined by either `width` as the number of samples around `idx` between
`[idx - floor(width/2),` `idx + floor(width/2)]`. Or by `span` as the
timespan in units of `time_channel` between `[t - span/2, t + span/2]`.

Specifying `width` is often faster than `span`.

If there are no valid values within the calculation window, will return
`NA`. A partial moving average will be calculated at the edges of the
data.

## Examples

``` r
x <- c(1, 3, 2, 5, 4, 6, 5, 7)
t <- c(0, 1, 2, 4, 5, 6, 7, 10)  ## irregular time with gaps

## width: centred window of 3 samples
filter_ma(x, width = 3)
#> [1]       NA 2.000000 3.333333 3.666667 5.000000 5.000000 6.000000       NA

## partial = TRUE fills edge values with a narrower window
filter_ma(x, width = 3, partial = TRUE)
#> [1] 2.000000 2.000000 3.333333 3.666667 5.000000 5.000000 6.000000 6.000000

## span: centred window of 2 time-units (accounts for irregular sampling)
filter_ma(x, t, span = 2)
#> [1] 2.0 2.0 2.5 4.5 5.0 5.0 5.5 7.0

## na.rm = FALSE (default): any NA in the window propagates to the result
x_na <- c(1, NA, 3, 4, 5, NA, 7, 8)
filter_ma(x_na, width = 3)
#> [1] NA NA NA  4 NA NA NA NA

## na.rm = TRUE: skip NAs within the window and return the mean of valid values
filter_ma(x_na, width = 3, partial = TRUE, na.rm = TRUE)
#> [1] 1.0 2.0 3.5 4.0 4.5 6.0 7.5 7.5
```
