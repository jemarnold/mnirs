# Rate constant of a sigmoidal shape

The rate `k` such that the sigmoid of the given `shape` is a function of
`u = k * (t - xmid)`: `4 * slope / (B - A)` for `"symmetric"`, else
`slope * e / (B - A)`. Positive for a consistent fit, where `slope` and
`B - A` share a sign.

## Usage

``` r
sigdrift_rate(A, B, slope, shape)
```

## Arguments

- A:

  A numeric parameter for the starting asymptote of the response
  variable.

- B:

  A numeric parameter for the ending asymptote of the response variable.

- slope:

  A numeric parameter for the response rate `dx/dt` at the inflection
  `xmid`.

- shape:

  Character; the 4-parameter sigmoidal shape. One of `"symmetric"`
  (*default*;
  [`logistic()`](https://jemarnold.github.io/mnirs/reference/logistic.md)),
  `"gompertz"`
  ([`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)),
  or `"gompertz_left"`
  ([`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)).

## Value

A numeric rate in units of `1 / t`.
