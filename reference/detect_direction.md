# Detect the direction of a response signal

Resolves whether a signal responds upward (`"positive"`) or downward
(`"negative"`) by comparing the excursions of `x` above and below its
initial baseline, taken as the median of the earliest samples ordered by
`t`. The dominant excursion captures the primary response direction even
when a fast initial component partially recovers over most of the record
(e.g. biexponential drop-recovery), where a net slope would misreport
the trend. Used internally to disambiguate peak (maximum) from trough
(minimum) detection when `direction = "auto"`.

## Usage

``` r
detect_direction(
  x,
  t = seq_along(x),
  fallback = x,
  direction = c("auto", "positive", "negative")
)
```

## Arguments

- x:

  A numeric vector of the response variable.

- t:

  An *optional* numeric vector of the predictor variable (e.g. time).
  Default is `seq_along(x)`.

- fallback:

  A numeric vector (*defaults* to `x`) used to resolve direction when
  the excursions above and below baseline tie (e.g. flat or symmetric
  data). The absolute maximum and minimum of `fallback` are compared; if
  `abs(max) >= abs(min)`, `"positive"` is returned.

- direction:

  A character string specifying the response direction to detect when
  `"auto"` (*default*). When `"positive"` or `"negative"` returns
  unchanged.

## Value

A character string: `"positive"` or `"negative"`.
