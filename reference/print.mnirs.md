# Methods for mnirs objects

Generic methods for objects of class `"mnirs"`.

## Usage

``` r
# S3 method for class 'mnirs'
print(x, ...)
```

## Arguments

- x:

  Object of class `"mnirs"`.

- ...:

  Additional arguments passed to
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

- `print`:

  Returns `x` without class attributes.

## Examples

``` r
x <- read_mnirs(
    example_mnirs("train.red"),
    nirs_channels = c(smo2 = "SmO2"),
    time_channel = c(time = "Timestamp (seconds passed)"),
    verbose = FALSE
) |>
    resample_mnirs(method = "linear", verbose = FALSE) |>
    extract_intervals(
        start = by_time(2452, 3168),
        span = c(-60, 120),
        verbose = FALSE
    )

print(x)
#> $interval_1
#> # A tibble: 1,801 × 2
#>     time  smo2
#>    <dbl> <dbl>
#>  1 2392   56.5
#>  2 2392.  56.5
#>  3 2392.  56.5
#>  4 2392.  56.6
#>  5 2392.  56.6
#>  6 2392.  56.6
#>  7 2393.  56.6
#>  8 2393.  56.6
#>  9 2393.  56.6
#> 10 2393.  56.6
#> # ℹ 1,791 more rows
#> 
#> $interval_2
#> # A tibble: 1,801 × 2
#>     time  smo2
#>    <dbl> <dbl>
#>  1 3108   56.7
#>  2 3108.  56.7
#>  3 3108.  56.7
#>  4 3108.  56.7
#>  5 3108.  56.8
#>  6 3108.  56.8
#>  7 3109.  56.8
#>  8 3109.  56.8
#>  9 3109.  56.8
#> 10 3109.  56.8
#> # ℹ 1,791 more rows
#> 
```
