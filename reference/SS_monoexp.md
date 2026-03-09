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
x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) + 
    rnorm(length(t), 0, 3)
data <- data.frame(t, x)

model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)

model
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

y
#>  [1] 10.46110 10.46110 10.46110 10.46110 10.46110 10.46110 10.46110 10.46110
#>  [9] 10.46110 10.46110 10.46110 10.46110 10.46110 10.46110 11.71031 21.74373
#> [17] 30.63994 38.52783 45.52169 51.72285 57.22115 62.09627 66.41882 70.25145
#> [25] 73.64968 76.66275 79.33431 81.70306 83.80334 85.66557 87.31673 88.78074
#> [33] 90.07881 91.22976 92.25026 93.15510 93.95737 94.66872 95.29944 95.85867
#> [41] 96.35452 96.79416 97.18398 97.52961 97.83607 98.10780 98.34872 98.56234
#> [49] 98.75175 98.91969 99.06859 99.20062 99.31768 99.42148 99.51351 99.59511
#> [57] 99.66746 99.73161 99.78849 99.83892

# \donttest{
    if (requireNamespace("ggplot2", quietly = TRUE)) {
        ggplot2::ggplot(data, ggplot2::aes(t, x)) +
            theme_mnirs() +
            ggplot2::geom_point() +
            ggplot2::geom_line(ggplot2::aes(y = y))
    }

# }
```
