# Generalised logistic function

Calculate a 4- or 5-parameter logistic (sigmoidal) curve. The
4-parameter symmetric form is fit by
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
with `method = "sigmoidal"` and `shape = "symmetric"` (*default*), and
by [`stats::nls()`](https://rdrr.io/r/stats/nls.html) via the
self-starting wrapper
[`SSlogistic()`](https://jemarnold.github.io/mnirs/reference/SSlogistic.md).

## Usage

``` r
logistic(t, A, B, xmid, slope, asym = NULL)
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

- asym:

  A numeric parameter for the asymmetry index of the curve; the fraction
  of the amplitude `(y(xmid) - A) / (B - A)` at which the inflection
  `xmid` occurs, in `(0, 1)`. `asym = 0.5` is symmetric and equivalent
  to the 4-parameter form. If `NULL` (*default*), a symmetric
  4-parameter model is used.

## Value

A numeric vector of predicted values the same length as the predictor
variable `t`.

## Details

The 5-parameter Richards form is exported for advanced use directly with
[`stats::nls()`](https://rdrr.io/r/stats/nls.html) but is not used by
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
due to convergence instability. For asymmetric responses, prefer
[`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)
/
[`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md),
which are more stable.

### Model equations

Both forms are re-parameterised from the Richards generalised logistic
model so `xmid` is the time at inflection and `slope` is the response
rate `dx/dt` at the inflection.

- 4-parameter (symmetric):
  `A + (B - A) / (1 + exp(-4 * slope * (t - xmid) / (B - A)))`

- 5-parameter (asymmetric):
  `A + (B - A) / (1 + exp(-k * (t - xmid)))^(1 / v)` with
  `v = -log(2) / log(asym)` and `k = 2 * slope * v / ((B - A) * asym)`.

The inflection is at `t = xmid` with `dx/dt = slope` and
`y(xmid) = A + (B - A) * asym` for any `asym` in `(0, 1)`:

- `asym = 0.5` (`v = 1`) collapses to the 4-parameter form.

- `asym -> 0` gives an early-acceleration curve (inflection near `A`).

- `asym -> 1` gives a late-acceleration curve (inflection near `B`).

- `asym = 0.368` (`1/e`) approximates a right-inflection
  [`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)
  curve.

- `asym = 0.632` (`1 - 1/e`) approximates a left-inflection
  [`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md)
  curve.

## See also

[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md),
[`SSlogistic()`](https://jemarnold.github.io/mnirs/reference/SSlogistic.md),
[`gompertz()`](https://jemarnold.github.io/mnirs/reference/gompertz.md),
[`gompertz_left()`](https://jemarnold.github.io/mnirs/reference/gompertz.md),
[`sigmoidal_drift()`](https://jemarnold.github.io/mnirs/reference/sigmoidal_drift.md),
[`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md)

## Examples

``` r
## create an asymmetric logistic curve with random noise
set.seed(15)
t <- 1:60
x <- logistic(t, A = 10, B = 100, xmid = 30, slope = 4, asym = 0.3) +
    rnorm(length(t), 0, 2)
data <- data.frame(t, x)

## 5-parameter fit with the self-starting wrapper
model <- nls(x ~ SSlogistic(t, A, B, xmid, slope, asym), data = data)
summary(model)
#> 
#> Formula: x ~ SSlogistic(t, A, B, xmid, slope, asym)
#> 
#> Parameters:
#>       Estimate Std. Error t value Pr(>|t|)    
#> A      10.2987     0.5316  19.372  < 2e-16 ***
#> B     100.9565     0.9135 110.519  < 2e-16 ***
#> xmid   29.3426     2.5692  11.421 3.87e-16 ***
#> slope   3.8707     0.6245   6.198 7.69e-08 ***
#> asym    0.2743     0.1115   2.460    0.017 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.827 on 55 degrees of freedom
#> 
#> Number of iterations to convergence: 5 
#> Achieved convergence tolerance: 1.468e-06
#> 

y <- predict(model, data)

# \donttest{
    if (requireNamespace("ggplot2", quietly = TRUE)) {
        ggplot2::ggplot(data, ggplot2::aes(t, x)) +
            theme_mnirs() +
            ggplot2::geom_point() +
            ggplot2::geom_line(ggplot2::aes(y = y))
    }

# }
```
