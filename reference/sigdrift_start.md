# Starting estimates for the sigmoidal-drift model

Vector-level initialiser behind
[`sigdrift_init()`](https://jemarnold.github.io/mnirs/reference/sigdrift_init.md),
called directly by the kinetics worker on the fit window. The sigmoid is
seeded as for
[`SSgompertz()`](https://jemarnold.github.io/mnirs/reference/SSgompertz.md)
([`init_asymptotes()`](https://jemarnold.github.io/mnirs/reference/init_asymptotes.md),
[`init_inflection()`](https://jemarnold.github.io/mnirs/reference/init_inflection.md)),
the drift onset resolved from that seed, and the residual from the
seeded sigmoid past the onset regressed on time from the onset: the
intercept corrects the asymptote `B` and the slope is the drift. A
second pass re-seeds the sigmoid on the drift-corrected response,
correcting an inflection biased by the drift. Fewer than two points past
the onset seed a zero drift. User-fixed values are held.

## Usage

``` r
sigdrift_start(x, t, fixed = list(), shape = "symmetric")
```

## Arguments

- x:

  A numeric vector of the response variable (sorted by `t`).

- t:

  A numeric vector of the predictor variable.

- fixed:

  A named list of user-fixed parameter values.

- shape:

  Character; the 4-parameter sigmoidal shape. One of `"symmetric"`
  (*default*;
  [`logistic()`](https://jemarnold.github.io/mnirs/reference/logistic.md)),
  `"gompertz"`
  ([`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)),
  or `"gompertz_left"`
  ([`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)).

## Value

A named numeric vector of starting estimates in model order.
