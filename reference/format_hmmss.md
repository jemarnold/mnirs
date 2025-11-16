# Format timespan data as h:mm:ss

Convert numeric timespan data to `h:mm:ss` format for pretty plotting.
Inspired by
[`ggplot2::scale_x_time()`](https://ggplot2.tidyverse.org/reference/scale_date.html).

## Usage

``` r
format_hmmss(x)
```

## Arguments

- x:

  A numeric vector.

## Value

A character vector the same length as `x`.

## Details

If all values are less than 3600 (1 hour), then format is returned as
`mm:ss`. If any value is greater than 3600, format is returned as
`h:mm:ss` with leading zeroes.

## Examples

``` r
library(ggplot2)

x = 0:120
y = sin(2 * pi * x / 15) + rnorm(length(x), 0, 0.2)

ggplot(data.frame(x, y)) +
    aes(x = x, y = y) +
    theme_mnirs() +
    scale_x_continuous(
        breaks = breaks_timespan(),
        labels = format_hmmss
    ) +
    geom_line()
```
