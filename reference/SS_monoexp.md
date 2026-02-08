# Self-starting monoexponential models

`SS_monoexp3()`: Creates initial coefficient estimates for a `selfStart`
model for a 3-parameter
[`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md)
function (A, B, tau).

`SS_monoexp4()` supports a 4-parameter
[`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md)
function (A, B, tau, TD).

## Usage

``` r
SS_monoexp3(t, A, B, tau)

SS_monoexp4(t, A, B, tau, TD)
```

## Arguments

- t:

  A numeric vector of the predictor variable; time or sample number.

- A:

  A numeric parameter for the starting (baseline) value of the response
  variable.

- B:

  A numeric parameter for the ending (asymptote) value of the response
  variable.

- tau:

  A numeric parameter for the time constant `tau` (\\\tau\\) of the
  exponential curve, in units of the predictor variable `t`.

- TD:

  A numeric parameter for the time delay before the onset of exponential
  response, in units of the predictor variable `t`.

## Value

`SS_monoexp3()` and `SS_monoexp4()`: A numeric vector of predicted
values the same length as the predictor variable `t`.

## Details

For 3-parameter model: `y ~ SS_monoexp3(t, A, B, tau)`

For 4-parameter model: `y ~ SS_monoexp4(t, A, B, tau, TD)`

The 3-parameter model is recommended for small samples or when no
obvious time delay exists, as it converges more reliably.

## See also

[`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md),
[`stats::nls()`](https://rdrr.io/r/stats/nls.html),
[`stats::selfStart()`](https://rdrr.io/r/stats/selfStart.html),
[`SSasymp()`](https://rdrr.io/r/stats/SSasymp.html)

## Examples

``` r
set.seed(13)
t <- 1:60

## create an exponential curve with random noise
x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) + rnorm(length(t), 0, 3)
data <- data.frame(t, x)

(model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data))
#> Nonlinear regression model
#>   model: x ~ SS_monoexp4(t, A, B, tau, TD)
#>    data: data
#>       A       B     tau      TD 
#>  10.461 100.233   8.313  14.884 
#>  residual sum-of-squares: 455.5
#> 
#> Number of iterations to convergence: 5 
#> Achieved convergence tolerance: 7.481e-07

y <- predict(model, data)

library(ggplot2)
ggplot(data, aes(t, x)) +
    theme_mnirs() +
    geom_point() +
    geom_line(aes(y = y))
```
