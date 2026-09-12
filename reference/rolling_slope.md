# Calculate rolling linear slope

`rolling_slope()`: Compute rolling linear regression slopes within a
local window along a numeric vector.

`slope()`: Calculate the linear regression slope of a numeric vector via
the least-squares formula.

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
  ...,
  env = rlang::caller_env()
)

slope(x, t = seq_along(x), na.rm = FALSE, ..., env = rlang::caller_env())
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

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

`rolling_slope()` returns a numeric vector of rolling local slopes in
units of `x / t`, the same length as `x`.

`slope()` returns a numeric slope value in units of `x / t`, or
`NA_real_` when insufficient valid observations are present.

## Details

See
[`peak_slope()`](https://jemarnold.github.io/mnirs/reference/peak_slope.md)
for details on window specification (`width`, `span`, `align`), partial
windows, and direction detection.

Additional arguments (`...`) accepted:

- `bypass_checks`:

  Logical; if `TRUE`, skips input validation. Intended for internal use
  when checks have already been performed upstream.

- `min_obs`:

  Integer; minimum number of valid observations required per window to
  return a slope. Derived from `width` or `span`, or `2L` when
  `partial = TRUE`.

- `intercept`:

  Logical; if `TRUE`, `slope()` also attaches the y-intercept as
  `attr(slope_val, "intercept")`.

- `window_idx`:

  Logical; if `TRUE`, the window bounds from
  [`compute_window_bounds()`](https://jemarnold.github.io/mnirs/reference/compute_helpers.md)
  are attached as `attr(slopes, "bounds")`.

## See also

[`peak_slope()`](https://jemarnold.github.io/mnirs/reference/peak_slope.md)
