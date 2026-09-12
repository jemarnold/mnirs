# Self-starting sigmoidal-drift model

Creates initial coefficient estimates for a `selfStart` wrapper around
[`sigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/sigmoidal_drift.md),
for use with [`stats::nls()`](https://rdrr.io/r/stats/nls.html): a
4-parameter sigmoid (A, B, xmid, slope) with a linear drift `slope_B` at
its ending asymptote from the onset fraction `drift_fraction`.

## Usage

``` r
SSsigmoidal_drift(t, A, B, xmid, slope, slope_B, drift_fraction, shape)
```

## Arguments

- t:

  A numeric vector of the predictor variable (time).

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

A numeric vector of predicted values the same length as the predictor
variable `t`.

## Details

### Model formula

`x ~ SSsigmoidal_drift(t, A, B, xmid, slope, slope_B, drift_fraction = 0.95, shape = "gompertz")`

`drift_fraction` should be written as a constant, and `shape` is a
string constant (`"symmetric"` when omitted); neither is estimated. The
hinge at the drift onset is not differentiable, so `algorithm = "port"`
with `control = nls.control(warnOnly = TRUE)` is recommended.

Starting estimates seed the sigmoid as for
[`SSgompertz()`](https://jemarnold.github.io/mnirs/reference/SSgompertz.md),
resolve the drift onset from that seed, and regress the residual past
the onset on time to seed `slope_B` and correct the asymptote `B`.

### Fixing parameters

Any parameter may be held constant by writing a value in place of its
name in the formula, e.g.
`x ~ SSsigmoidal_drift(t, A = 0, B, xmid, slope, slope_B, drift_fraction = 0.95)`
fixes the starting asymptote at `A = 0`. Fixed parameters are excluded
from estimation and are not returned by
[`stats::coef()`](https://rdrr.io/r/stats/coef.html).

## See also

[`sigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/sigmoidal_drift.md),
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md),
[`stats::nls()`](https://rdrr.io/r/stats/nls.html),
[`stats::selfStart()`](https://rdrr.io/r/stats/selfStart.html),
[`SSlogistic()`](https://jemarnold.github.io/mnirs/reference/SSlogistic.md),
[`SSgompertz()`](https://jemarnold.github.io/mnirs/reference/SSgompertz.md),
[`SSexponential_drift()`](https://jemarnold.github.io/mnirs/reference/SSexponential_drift.md)

## Examples

``` r
## create a Gompertz curve with late linear drift and random noise
set.seed(13)
t <- 1:120
x <- sigmoidal_drift(
    t, A = 10, B = 100, xmid = 40, slope = 4,
    slope_B = -0.4, drift_fraction = 0.95, shape = "gompertz"
) + rnorm(length(t), 0, 2)
data <- data.frame(t, x)

## fit with the drift onset held at 95% of the amplitude
model <- nls(
    x ~ SSsigmoidal_drift(
        t, A, B, xmid, slope, slope_B,
        drift_fraction = 0.95, shape = "gompertz"
    ),
    data = data,
    algorithm = "port",
    control = nls.control(warnOnly = TRUE)
)
summary(model)
#> 
#> Formula: x ~ SSsigmoidal_drift(t, A, B, xmid, slope, slope_B, drift_fraction = 0.95, 
#>     shape = "gompertz")
#> 
#> Parameters:
#>         Estimate Std. Error t value Pr(>|t|)    
#> A       10.18595    0.35178   28.95   <2e-16 ***
#> B       99.48771    0.45148  220.36   <2e-16 ***
#> xmid    40.09004    0.15194  263.86   <2e-16 ***
#> slope    4.11373    0.08178   50.30   <2e-16 ***
#> slope_B -0.37981    0.01688  -22.49   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.882 on 115 degrees of freedom
#> 
#> Algorithm "port", convergence message: relative convergence (4)
#> 
```
