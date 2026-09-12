# Self-starting Gompertz models

Creates initial coefficient estimates for `selfStart` wrappers around
[`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)
and
[`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md),
for use with [`stats::nls()`](https://rdrr.io/r/stats/nls.html). Both
wrappers use the same 4-parameter (A, B, xmid, slope) interface.

## Usage

``` r
SSgompertz(t, A, B, xmid, slope)

SSgompertz_left(t, A, B, xmid, slope)

SSgompertz_left(t, A, B, xmid, slope)
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

## Value

A numeric vector of predicted values the same length as the predictor
variable `t`.

## Details

### Model formulas

- Right-Gompertz: `x ~ SSgompertz(t, A, B, xmid, slope)`

- Left-Gompertz: `x ~ SSgompertz_left(t, A, B, xmid, slope)`

Used by
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
with `method = "sigmoidal"` and `shape = "gompertz"` or
`"gompertz_left"`. Starting estimates locate the inflection from a
smoothed first derivative. `SSgompertz()` masks
[`stats::SSgompertz()`](https://rdrr.io/r/stats/SSgompertz.html).

### Fixing parameters

Any parameter may be held constant by writing a value in place of its
name in the formula, e.g. `x ~ SSgompertz(t, A = 0, B, xmid, slope)`
fixes the starting asymptote at `A = 0`. Fixed parameters are excluded
from estimation and are not returned by
[`stats::coef()`](https://rdrr.io/r/stats/coef.html).

## See also

[`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md),
[`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md),
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md),
[`SSlogistic()`](https://jemarnold.github.io/mnirs/reference/SSlogistic.md),
[`stats::nls()`](https://rdrr.io/r/stats/nls.html),
[`stats::selfStart()`](https://rdrr.io/r/stats/selfStart.html),
[`stats::SSgompertz()`](https://rdrr.io/r/stats/SSgompertz.html)

## Examples

``` r
## create a Gompertz curve with random noise
set.seed(15)
t <- 1:60
x <- gompertz(t, A = 10, B = 100, xmid = 30, slope = 4) +
    rnorm(length(t), 0, 2)
data <- data.frame(t, x)

model <- nls(x ~ SSgompertz(t, A, B, xmid, slope), data = data)
summary(model)
#> 
#> Formula: x ~ SSgompertz(t, A, B, xmid, slope)
#> 
#> Parameters:
#>        Estimate Std. Error t value Pr(>|t|)    
#> A      10.23048    0.43131   23.72   <2e-16 ***
#> B     100.77389    0.84867  118.74   <2e-16 ***
#> xmid   29.92865    0.17117  174.85   <2e-16 ***
#> slope   4.01621    0.09135   43.96   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.811 on 56 degrees of freedom
#> 
#> Number of iterations to convergence: 4 
#> Achieved convergence tolerance: 1.076e-06
#> 

## fix the starting asymptote `A` at a known value
model_fixed <- nls(x ~ SSgompertz(t, A = 10, B, xmid, slope), data = data)
summary(model_fixed)
#> 
#> Formula: x ~ SSgompertz(t, A = 10, B, xmid, slope)
#> 
#> Parameters:
#>        Estimate Std. Error t value Pr(>|t|)    
#> B     100.85346    0.83300  121.07   <2e-16 ***
#> xmid   29.88864    0.15332  194.94   <2e-16 ***
#> slope   4.00873    0.08903   45.03   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.8 on 57 degrees of freedom
#> 
#> Number of iterations to convergence: 3 
#> Achieved convergence tolerance: 2.247e-06
#> 

## left-Gompertz
set.seed(16)
x2 <- gompertz_left(t, A = 10, B = 100, xmid = 30, slope = 4) +
    rnorm(length(t), 0, 2)
data2 <- data.frame(t, x = x2)

model_left <- nls(x ~ SSgompertz_left(t, A, B, xmid, slope), data = data2)
summary(model_left)
#> 
#> Formula: x ~ SSgompertz_left(t, A, B, xmid, slope)
#> 
#> Parameters:
#>       Estimate Std. Error t value Pr(>|t|)    
#> A       9.8781     1.0370   9.525 2.58e-13 ***
#> B     100.6108     0.4727 212.839  < 2e-16 ***
#> xmid   30.0446     0.1987 151.192  < 2e-16 ***
#> slope   3.9205     0.1002  39.127  < 2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 2.021 on 56 degrees of freedom
#> 
#> Number of iterations to convergence: 6 
#> Achieved convergence tolerance: 1.273e-06
#> 
```
