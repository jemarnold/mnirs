# Excursion point of the sigmoidal-drift model

The time past the inflection at which the drift rate overtakes the
decaying sigmoid rate, `|S'(t)| = |slope_B|`, floored at the drift onset
(see
[`sigdrift_onset()`](https://jemarnold.github.io/mnirs/reference/sigdrift_onset.md)):
the turning point of the curve when the phases oppose, or where the
linear trend takes over a monotonic response. A drift at least as fast
as the peak sigmoid rate `slope` takes over from the onset. Scalar
parameters only.

## Usage

``` r
sigdrift_texc(A, B, xmid, slope, slope_B, drift_fraction, shape)
```

## Arguments

- A:

  A numeric parameter for the starting asymptote of the response
  variable.

- B:

  A numeric parameter for the ending asymptote of the response variable.

- xmid:

  A numeric parameter for the time at the *inflection point* (the
  steepest point) of the curve, in units of the predictor variable `t`.

- slope:

  A numeric parameter for the response rate `dx/dt` at the inflection
  `xmid`.

- slope_B:

  A numeric parameter for the linear drift rate `dx/dt` of the secondary
  phase at the ending asymptote `B`, in response units per unit of the
  predictor variable `t`.

- drift_fraction:

  A numeric fraction of the primary amplitude `B - A` in `(0.5, 1)` at
  which the linear drift begins, where the sigmoid reaches
  `A + drift_fraction * (B - A)`.

- shape:

  Character; the 4-parameter sigmoidal shape. One of `"symmetric"`
  (*default*;
  [`logistic()`](https://jemarnold.github.io/mnirs/reference/logistic.md)),
  `"gompertz"`
  ([`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)),
  or `"gompertz_left"`
  ([`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)).

## Value

A numeric excursion time.

## Details

With `ratio = |slope_B / slope|` and `u = k * (t - xmid)` (see
[`sigdrift_rate()`](https://jemarnold.github.io/mnirs/reference/sigdrift_rate.md)),
the sigmoid rate relative to its peak is `4 * L * (1 - L)` with
`L = 1 / (1 + exp(-u))` for `"symmetric"`, solved as
`u = 2 * atanh(sqrt(1 - r))`; `exp(1 - u - exp(-u))` for `"gompertz"`;
and `exp(1 + u - exp(u))` for `"gompertz_left"`. The Gompertz forms have
no closed inverse and are solved by
[`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html) on a bracket
containing the single post-inflection root.
