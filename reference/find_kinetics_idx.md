# Find valid model-fitting indices up to the first extreme

Filters `x` and `t` to valid finite values, locates the first valid peak
(maximum) or trough (minimum) where `t >= 0`, and returns the integer
indices of all finite observations up to `end_window` past that extreme.

## Usage

``` r
find_kinetics_idx(
  x,
  t = seq_along(x),
  end_window = Inf,
  direction = c("auto", "positive", "negative"),
  ...,
  env = rlang::caller_env()
)
```

## Arguments

- x:

  A numeric vector of the response variable.

- t:

  An *optional* numeric vector of the predictor variable (e.g. time).
  Default is `seq_along(x)`.

- end_window:

  A numeric value in units of `time_channel` or `t` specifying the
  forward-looking window used to check for subsequent greater/lesser
  values than the candidate extreme. `end_window = Inf` (*default*)
  returns the global extreme from the full range of `x`.

- direction:

  A character string specifying the response direction `"positive"`, or
  `"negative"`, or detect with `"auto"` (*default*). See *Details*.

- ...:

  Additional arguments.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A named list with three elements:

- `direction`:

  Character; the resolved direction used – `"positive"` (peak) or
  `"negative"` (trough).

- `extreme`:

  Integer or `NULL`; the index of the first qualifying peak or trough in
  original `x` space, or `NULL` if no qualifying extreme was found
  (monotonic, horizontal, or degenerate input).

- `idx`:

  Integer vector of all valid finite indices, truncated at
  `t[extreme] + end_window`.

## Details

### Direction detection

When `direction = "auto"`, the excursions of `x` above and below its
initial baseline (the median of the earliest samples) are compared via
[`detect_direction()`](https://jemarnold.github.io/mnirs/reference/detect_direction.md).
If the upward excursion dominates, the function searches for a peak
(maximum); if the downward excursion dominates, a trough (minimum). When
the excursions tie, the direction is determined by comparing
`abs(max(x))` to `abs(min(x))`, with ties defaulting to `"positive"`.

### Negative time handling

Only samples where `t >= 0` are used for detecting the extreme, allowing
pre-baseline (negative time) data to be excluded from the search.
However, indices where `t < 0` are included in the returned vector
provided they are finite.
