# Self-starting exponential-drift model

Creates initial coefficient estimates for a `selfStart` wrapper around
[`exponential_drift()`](https://jemarnold.github.io/mnirs/reference/exponential_drift.md),
for use with [`stats::nls()`](https://rdrr.io/r/stats/nls.html).
Supports both the 5-parameter (A, B, tau, slope_B, drift_fraction) and
6-parameter forms adding a time delay TD; arity is inferred from the
formula passed to [`stats::nls()`](https://rdrr.io/r/stats/nls.html).

## Usage

``` r
SSexponential_drift(t, A, B, tau, slope_B, drift_fraction, TD)
```

## Arguments

- t:

  A numeric vector of the predictor variable (time).

- A:

  A numeric parameter for the starting baseline of the response
  variable.

- B:

  A numeric parameter for the ending asymptote of the response variable.

- tau:

  A numeric parameter for the *time constant* (\\\tau\\) of the
  exponential response, in units of the predictor variable `t`.

- slope_B:

  A numeric parameter for the linear drift rate `dx/dt` of the secondary
  phase, in response units per unit of the predictor variable `t`.

- drift_fraction:

  A numeric fraction of the primary amplitude `B - A` in `(0.5, 1)` at
  which the linear drift begins, where the primary response reaches
  `A + drift_fraction * (B - A)`.

- TD:

  A numeric parameter for the *time delay* before the onset of the
  exponential response, in units of the predictor variable `t`. If
  `NULL` (*default*), a 3-parameter model without time delay is used.

## Value

A numeric vector of predicted values the same length as the predictor
variable `t`.

## Details

### Model formulas

- 5-parameter:
  `x ~ SSexponential_drift(t, A, B, tau, slope_B, drift_fraction)`

- 6-parameter:
  `x ~ SSexponential_drift(t, A, B, tau, slope_B, drift_fraction, TD)`

The hinge at the drift onset `TD - tau * log(1 - drift_fraction)` is not
differentiable, so `algorithm = "port"` with `tau` (and `TD`) bounded
non-negative and `control = nls.control(warnOnly = TRUE)` is
recommended.

Starting estimates are profiled on a coarse grid of `tau` (and `TD`)
with `A`, `B`, and `slope_B` solved by least squares at each grid point,
keeping the residual-minimising start.

The model function returns the analytic gradient (one-sided at the
hinge) for the free parameters as a `"gradient"` attribute, so
[`stats::nls()`](https://rdrr.io/r/stats/nls.html) does not resort to
[`stats::numericDeriv()`](https://rdrr.io/r/stats/numericDeriv.html).
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) on a fitted
model carries the attribute; drop it with
[`as.vector()`](https://rdrr.io/r/base/vector.html).

### Fixing parameters

Any parameter may be held constant by writing a value in place of its
name in the formula, e.g.
`x ~ SSexponential_drift(t, A, B, tau, slope_B, drift_fraction = 0.95)`
holds the drift onset at 95% of the amplitude (`TD + 3 * tau`). Fixed
parameters are excluded from estimation and are not returned by
[`stats::coef()`](https://rdrr.io/r/stats/coef.html).

## See also

[`exponential_drift()`](https://jemarnold.github.io/mnirs/reference/exponential_drift.md),
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md),
[`stats::nls()`](https://rdrr.io/r/stats/nls.html),
[`stats::selfStart()`](https://rdrr.io/r/stats/selfStart.html),
[`SSmonoexponential()`](https://jemarnold.github.io/mnirs/reference/SSmonoexponential.md),
[`SSbiexponential()`](https://jemarnold.github.io/mnirs/reference/SSbiexponential.md)

## Examples

``` r
## create an exponential curve with late linear drift and random noise
set.seed(13)
t <- 1:180
x <- exponential_drift(
    t, A = 10, B = 100, tau = 12,
    slope_B = -0.5, drift_fraction = 0.98, TD = 15
) + rnorm(length(t), 0, 2)
data <- data.frame(t, x)

## 6-parameter fit with the drift onset held at 98% of the amplitude
model <- nls(
    x ~ SSexponential_drift(
        t, A, B, tau, slope_B, drift_fraction = 0.98, TD
    ),
    data = data,
    algorithm = "port",
    lower = c(-Inf, -Inf, 0, -Inf, 0),
    control = nls.control(warnOnly = TRUE)
)
summary(model)
#> 
#> Formula: x ~ SSexponential_drift(t, A, B, tau, slope_B, drift_fraction = 0.98, 
#>     TD)
#> 
#> Parameters:
#>          Estimate Std. Error t value Pr(>|t|)    
#> A       10.293460   0.558698   18.42   <2e-16 ***
#> B       99.640346   0.280336  355.43   <2e-16 ***
#> tau     12.009947   0.169776   70.74   <2e-16 ***
#> slope_B -0.494146   0.005696  -86.75   <2e-16 ***
#> TD      14.978469   0.162366   92.25   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 2.091 on 175 degrees of freedom
#> 
#> Algorithm "port", convergence message: both X-convergence and relative convergence (5)
#> 
```
