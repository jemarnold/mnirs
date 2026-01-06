# Breaks for timespan data

Pretty timespan breaks for plotting in units of 5, 15, 30, 60 sec, etc.
Modified from
[`scales::breaks_timespan()`](https://scales.r-lib.org/reference/breaks_timespan.html).

## Usage

``` r
breaks_timespan(unit = c("secs", "mins", "hours", "days", "weeks"), n = 5)
```

## Arguments

- unit:

  The time unit used to interpret numeric data input (*defaults* to
  *"secs"*).

- n:

  Desired number of breaks. You may get slightly more or fewer breaks
  than requested.

## Value

Returns a function for generating breaks.

## Examples

``` r
x = 0:120
y = sin(2 * pi * x / 15) + rnorm(length(x), 0, 0.2)

library(ggplot2)
data.frame(x, y) |>
    ggplot(aes(x, y)) +
    theme_mnirs() +
    scale_x_continuous(breaks = breaks_timespan()) +
    geom_line()
```
