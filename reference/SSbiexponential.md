# Self-starting biexponential model

Creates initial coefficient estimates for a `selfStart` wrapper around
[`biexponential()`](https://jemarnold.github.io/mnirs/reference/biexponential.md),
for use with [`stats::nls()`](https://rdrr.io/r/stats/nls.html).
Supports both the 5-parameter (A, B, tau, B2, tau2) and 6-parameter
forms adding a time delay TD; arity is inferred from the formula passed
to [`stats::nls()`](https://rdrr.io/r/stats/nls.html).

## Usage

``` r
SSbiexponential(t, A, B, tau, B2, tau2, TD)
```

## Arguments

- t:

  A numeric vector of the predictor variable (time).

- A:

  A numeric parameter for the starting value of the response variable
  (the `t = 0` intercept).

- B:

  A numeric parameter for the asymptote of the *fast* component; the
  value the fast response alone would approach.

- tau:

  A numeric parameter for the *fast* time constant (\\\tau_1\\), in
  units of the predictor variable `t`. Dominates the initial steep
  response.

- B2:

  A numeric parameter for the asymptote of the *slow* component; the
  stable plateau the response recovers toward as `t` approaches
  infinity.

- tau2:

  A numeric parameter for the *slow* time constant (\\\tau_2\\), in
  units of the predictor variable `t`. Typically `tau2 >> tau`.

- TD:

  A numeric parameter for the *time delay* before the onset of the
  response, in units of the predictor variable `t`. If `NULL`
  (*default*), a 5-parameter model without time delay is used.

## Value

A numeric vector of predicted values the same length as the predictor
variable `t`.

## Details

### Model formulas

- 5-parameter: `x ~ SSbiexponential(t, A, B, tau, B2, tau2)`

- 6-parameter: `x ~ SSbiexponential(t, A, B, tau, B2, tau2, TD)`

The two phases are weakly identified when `tau` and `tau2` are close, so
`algorithm = "port"` with the time constants bounded non-negative and
`control = nls.control(warnOnly = TRUE)` is recommended.
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
instead fits the phases sequentially, holding the fast phase near a
monoexponential estimate.

The 5-parameter form is recommended for small samples or when no obvious
time delay is expected, as it converges more reliably.
[`stats::nls()`](https://rdrr.io/r/stats/nls.html) reads the free
parameters from the formula right-hand side, so omitting `TD` incurs no
degrees-of-freedom penalty.

Starting estimates are profiled on a coarse grid of `tau`, `tau2` (and
`TD`) with the amplitudes solved by least squares at each grid point,
keeping the residual-minimising start. Grid pairs with
`tau / tau2 > 0.98` are dropped as near-collinear.

The model function returns the analytic gradient for the free parameters
as a `"gradient"` attribute, so
[`stats::nls()`](https://rdrr.io/r/stats/nls.html) does not resort to
[`stats::numericDeriv()`](https://rdrr.io/r/stats/numericDeriv.html).
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) on a fitted
model carries the attribute; drop it with
[`as.vector()`](https://rdrr.io/r/base/vector.html).

### Fixing parameters

Any parameter may be held constant by writing a value in place of its
name in the formula, e.g.
`x ~ SSbiexponential(t, A, B, tau = 5, B2, tau2)` holds the fast time
constant at `5`. Fixed parameters are excluded from estimation and are
not returned by [`stats::coef()`](https://rdrr.io/r/stats/coef.html).

## See also

[`biexponential()`](https://jemarnold.github.io/mnirs/reference/biexponential.md),
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md),
[`stats::nls()`](https://rdrr.io/r/stats/nls.html),
[`stats::selfStart()`](https://rdrr.io/r/stats/selfStart.html),
[`SSmonoexponential()`](https://jemarnold.github.io/mnirs/reference/SSmonoexponential.md),
[`SSexponential_drift()`](https://jemarnold.github.io/mnirs/reference/SSexponential_drift.md)

## Examples

``` r
## create a biexponential excursion-recovery curve with random noise
set.seed(13)
t <- 0:120
x <- biexponential(t, A = 70, B = 40, tau = 5, B2 = 60, tau2 = 40) +
    rnorm(length(t), 0, 0.8)
data <- data.frame(t, x)

## 5-parameter fit
model <- nls(
    x ~ SSbiexponential(t, A, B, tau, B2, tau2),
    data = data,
    algorithm = "port",
    lower = c(-Inf, -Inf, 0, -Inf, 0),
    control = nls.control(warnOnly = TRUE)
)
summary(model)
#> 
#> Formula: x ~ SSbiexponential(t, A, B, tau, B2, tau2)
#> 
#> Parameters:
#>      Estimate Std. Error t value Pr(>|t|)    
#> A     70.4808     0.5841  120.66   <2e-16 ***
#> B     39.6287     1.0816   36.64   <2e-16 ***
#> tau    5.1728     0.3067   16.86   <2e-16 ***
#> B2    60.0822     0.3795  158.34   <2e-16 ***
#> tau2  40.2967     3.4506   11.68   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.7524 on 116 degrees of freedom
#> 
#> Algorithm "port", convergence message: relative convergence (4)
#> 

## fix the fast time constant `tau` at a known value
model_fixed <- nls(
    x ~ SSbiexponential(t, A, B, tau = 5, B2, tau2),
    data = data,
    algorithm = "port",
    lower = c(-Inf, -Inf, -Inf, 0),
    control = nls.control(warnOnly = TRUE)
)
summary(model_fixed)
#> 
#> Formula: x ~ SSbiexponential(t, A, B, tau = 5, B2, tau2)
#> 
#> Parameters:
#>      Estimate Std. Error t value Pr(>|t|)    
#> A     70.6684     0.4846  145.83   <2e-16 ***
#> B     40.1496     0.5232   76.74   <2e-16 ***
#> B2    60.2161     0.3239  185.93   <2e-16 ***
#> tau2  41.8121     2.4865   16.82   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.7503 on 117 degrees of freedom
#> 
#> Algorithm "port", convergence message: both X-convergence and relative convergence (5)
#> 
```
