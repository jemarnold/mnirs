# Calculate linear slope

Calculates the linear regression slope of a numeric vector.

## Usage

``` r
slope(x, t = seq_along(x), na.rm = FALSE, ...)
```

## Arguments

- x:

  A numeric vector of the response variable.

- t:

  An *optional* numeric vector of the predictor variable; time or sample
  number. *Defaults* to indices of `t = seq_along(x)`.

- na.rm:

  A logical indicating whether missing values should be ignored
  (`TRUE`). Otherwise `FALSE` (the *default*) will return `NA` when
  there are any missing data within the vector.

- ...:

  Additional arguments.

## Value

A numeric slope in units of `x/t`.

## Details

Uses the least squares formula.

## Examples

``` r
x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
mnirs:::slope(x)
#> [1] 1.434066

x_na <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
mnirs:::slope(x_na)
#> [1] NA
mnirs:::slope(x_na, na.rm = TRUE)
#> [1] 1.418874
```
