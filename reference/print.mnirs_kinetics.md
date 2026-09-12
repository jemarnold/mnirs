# Methods for mnirs_kinetics objects

Generic methods for objects returned from
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md).

## Usage

``` r
# S3 method for class 'mnirs_kinetics'
print(x, ...)
```

## Arguments

- x:

  Object of class `"mnirs_kinetics"` returned from
  [`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md).

- ...:

  Additional arguments.

## Value

- `print`:

  Returns a model summary

## Examples

``` r
result <- read_mnirs(
    example_mnirs("train.red"),
    nirs_channels = c(smo2 = "SmO2"),
    time_channel = c(time = "Timestamp (seconds passed)"),
    zero_time = TRUE,
    verbose = FALSE
) |>
    resample_mnirs(method = "linear", verbose = FALSE) |>
    extract_intervals(
        group_intervals = "distinct", ## return each interval distinctly
        start = by_time(368, 1084),
        span = c(-20, 90),
        zero_time = TRUE,
        verbose = FALSE
    ) |>
    analyse_kinetics(
        method = "peak_slope",
        span = 10,
        verbose = FALSE
    )

print(result)
#> 
#> Peak Linear Response Rate
#>     Model Coefficients:
#>     interval nirs_channels  slope intercept peak_slope_time
#> 1 interval_1          smo2 0.6338     47.08            24.8
#> 2 interval_2          smo2 0.6353     41.65            33.4
#> 
#> 
```
