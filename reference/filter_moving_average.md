# Apply a moving average filter

Apply a simple moving average smoothing filter to vector data

## Usage

``` r
filter_moving_average(
  x,
  t = seq_along(x),
  width = NULL,
  span = NULL,
  inform = TRUE
)
```

## Arguments

- x:

  A numeric vector.

- t:

  An *optional* numeric vector of time or sample number.

- width:

  An integer defining the local window in number of samples around `idx`
  in which to perform the operation., between
  `[idx - floor(width/2), idx + floor(width/2)]`.

- span:

  A numeric value defining the local window timespan around `idx` in
  which to perform the operation. In units of `time_channel` or `t`,
  between `[t - span/2, t + span/2]`.

- inform:

  A logical to display (the *default*) or `FALSE` to silence warnings
  and information messages used for troubleshooting.

## Value

A numeric vector the same length as `x`.

## Details

Applies a centred (symmetrical) moving average filter in a local window
defined by either `width` as the number of samples around `idx` between
`[idx - floor(width/2),` `idx + floor(width/2)]`. Or by `span` as the
timespan in units of `time_channel` between `[t - span/2, t + span/2]`.

If there are no valid values within the calculation window, will return
`NA`. A partial moving average will be calculated at the edges of the
data.

## See also

`zoo::rollmean()`

## Examples

``` r
## basic moving average with sample width
x <- c(1, 3, 2, 5, 4, 6, 5, 7)
filter_moving_average(x, width = 3)
#> [1] 2.000000 2.000000 3.333333 3.666667 5.000000 5.000000 6.000000 6.000000

## with explicit time vector
t <- c(0, 1, 2, 3, 4, 5, 6, 7)
filter_moving_average(x, t, width = 2)
#> [1] 2.000000 2.000000 3.333333 3.666667 5.000000 5.000000 6.000000 6.000000

## using timespan instead of sample width
filter_moving_average(x, span = 2)
#> [1] 2.000000 2.000000 3.333333 3.666667 5.000000 5.000000 6.000000 6.000000
```
