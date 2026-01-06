# Calculate rolling linear slope

`rolling_slope()`: Computes rolling linear regression slopes within a
local window along a numeric vector.

`slope()`: Calculates the linear regression slope of a numeric vector.

## Usage

``` r
rolling_slope(
  x,
  t = seq_along(x),
  width = NULL,
  span = NULL,
  align = c("centre", "left", "right"),
  partial = FALSE,
  verbose = TRUE,
  ...
)

slope(x, t = seq_along(x), ...)
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

`rolling_slope()` returns a numeric vector of rolling local slopes in
units of `x/t` the same length as `x`.

`slope()` returns a numeric slope in units of `x/t`.

## Details

See details in
[`peak_slope()`](https://jemarnold.github.io/mnirs/reference/peak_slope.md).

Additional args (`...`) accepts:

- `bypass_checks`:

  Logical; Speeds operation by bypassing validation checks. These checks
  should be performed upstream.

- `min_obs`:

  Integer; The minimum number of observations required to calculate
  `slope()`. Defined by either `width` or `span`, or equal to `2` when
  `partial = TRUE`

- `intercept`:

  Logical; When `TRUE`, `slope()` will also return a numeric intercept
  value retrievable with `attr(slope, "intercept")`.

- `window_idx`:

  Logical; When `TRUE`, `rolling_slope()` will also return a list of
  numeric window indices retrievable with
  `attr(rolling_slope, "window_idx")`.

## See also

[`zoo::rollapply()`](https://rdrr.io/pkg/zoo/man/rollapply.html)
