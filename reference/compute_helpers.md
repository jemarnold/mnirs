# Computes vectorised rolling local values

`compute_local_windows()`: Helper function to return a list of rolling
sample indices `idx` along a time variable `t`, defined by either
`width` in samples or `span` in units of `t`.

`compute_local_fun()`: Helper function to return a vector of local
values calculated from `x` by a function `FUN` within a list of rolling
sample windows.

`compute_outliers()`: Helper function to return a vector of logicals
indicating local outliers of `x` within a list of rolling sample windows
`window_idx`.

`compute_window_of_valid_neighbours()`: Helper function to return a list
of sample indices `idx` along valid values of `x` to either side of
`NA`s, defined by either `width` in samples or `span` in units of `t`.

## Usage

``` r
compute_local_windows(
  t,
  idx = seq_along(t),
  width = NULL,
  span = NULL,
  method = c("two-sided", "centred"),
  verbose = TRUE
)

compute_local_fun(x, window_idx, FUN)

compute_outliers(x, window_idx, local_medians, outlier_cutoff)

compute_window_of_valid_neighbours(
  x,
  t = seq_along(x),
  width = NULL,
  span = NULL,
  verbose = TRUE
)
```

## Arguments

- t:

  An *optional* numeric vector of time or sample number.

- idx:

  A numeric vector of indices of `t` at which to calculate local
  windows. All indices of `t` by *default*, or can be used to only
  calculate for known indicies, such as invalid values of `x`.

- width:

  An integer defining the window in number of samples around `idx` in
  which to detect local outliers and perform median replacement, between
  `[idx - width, idx + width]`.

- span:

  A numeric value defining window timespan around `idx` in which to
  detect local outliers and perform median replacement. In units of
  `time_channel` or `t`, between `[t - span, t + span]`.

- method:

  A character string specifying whether the local window should take a
  *"two-sided"* `width` or `span` on both sides of `idx` (the
  *default*), or *"centred"* around `idx`.

- verbose:

  A logical to return (the *default*) or silence warnings and messages
  which can be used for data error checking. Abort errors will always be
  returned.

- x:

  A numeric vector.

- window_idx:

  A list the same length as `window_idx` and the same or shorter length
  as `x` with numeric vectors for the sample indices of local rolling
  windows.

- FUN:

  A function to pass through for local rolling calculation. Current
  options are `c(median, mean)`

- local_medians:

  A numeric vector the same length as `x` of local median values.

- outlier_cutoff:

  An integer for the local outlier threshold, as number of standard
  deviations above and below the local median. The *default*
  `outlier_cutoff = NULL` will not replace outliers.
  `outlier_cutoff = 3` is the standard replacement threshold following
  Pearson's rule.

## Value

`compute_local_windows()`: A list the same length as `idx` and the same
or shorter length as `t` with numeric vectors of sample indices of
length `2 * width` samples or `2 * span` units of time `t` for
`method = ` `"two-sided"`, or `width / span` for `method = "centred"`.

`compute_local_fun()`: A numeric vector the same length as `x`.

`compute_outliers()`: A logical vector the same length as `x`.

`compute_window_of_valid_neighbours()`: A list the same length as `NA`
values in `x` with numeric vectors of sample indices of length
`2 * width` samples or `2 * span` units of time `t` for valid values
neighbouring either side of the invalid `NA`s.

## Details

`method = "two-sided"` (the *default*) is used for
[`replace_mnirs()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
functions where the user is entering `width` or `span` for both sides of
`idx`.

`method = "centred"` is used for
[`shift_mnirs()`](https://jemarnold.github.io/mnirs/reference/shift_mnirs.md)
where the user is entering `width` or `span` centred around `idx`.

## Examples

``` r
x <- c(1, 2, 3, 100, 5)
t <- seq_along(x)

## a list of numeric vectors of rolling local windows along `t`
window_idx <- mnirs:::compute_local_windows(t, width = 1, span = NULL)
window_idx
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 1 2 3
#> 
#> [[3]]
#> [1] 2 3 4
#> 
#> [[4]]
#> [1] 3 4 5
#> 
#> [[5]]
#> [1] 4 5
#> 

## a numeric vector of local medians of `x`
local_medians <- mnirs:::compute_local_fun(x, window_idx, median)
local_medians
#> [1]  1.5  2.0  3.0  5.0 52.5

## a logical vector of local outliers of `x`
is.outlier <- mnirs:::compute_outliers(x, window_idx, local_medians, outlier_cutoff = 3)
is.outlier
#> [1] FALSE FALSE FALSE  TRUE FALSE

## a list of numeric vectors of local windows of valid values of `x`
## neighbouring `NA`s.
x <- c(1, 2, 3, NA, NA, 6)
window_idx <- mnirs:::compute_window_of_valid_neighbours(x, width = 1)
window_idx
#> [[1]]
#> [1] 3 6
#> 
#> [[2]]
#> [1] 3 6
#> 

local_medians <- mnirs:::compute_local_fun(x, window_idx, median)
local_medians
#> [1] 4.5 4.5

x[is.na(x)] <- local_medians
x
#> [1] 1.0 2.0 3.0 4.5 4.5 6.0
```
