# Plot *mnirs* objects

Create a simple plot for objects returned from
[`create_mnirs_data()`](https://jemarnold.github.io/mnirs/reference/create_mnirs_data.md).

## Usage

``` r
# S3 method for class 'mnirs'
plot(
  x,
  points = FALSE,
  time_labels = FALSE,
  n.breaks = 5,
  na.omit = FALSE,
  ...
)
```

## Arguments

- x:

  Object of class *"mnirs"* returned from
  [`create_mnirs_data()`](https://jemarnold.github.io/mnirs/reference/create_mnirs_data.md)

- points:

  Logical. Default is `FALSE`. If `TRUE` displays
  `ggplot2::geom_points()`. Otherwise only `ggplot2::geom_lines()` is
  displayed.

- time_labels:

  Logical. Default is `FALSE`. If `TRUE` displays x-axis time values
  formatted as *"hh:mm:ss"* using
  [`format_hmmss()`](https://jemarnold.github.io/mnirs/reference/format_hmmss.md).
  Otherwise, x-axis values are displayed as numeric.

- n.breaks:

  A numeric value specifying the number of breaks in both x- and y-axes.
  Default is `5`.

- na.omit:

  Logical. Default is `FALSE`. If `TRUE` omits missing (`NA`) and
  non-finite `c(Inf, -Inf, NaN)` from display.

- ...:

  Additional arguments.

## Value

A [ggplot2](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
data <- read_mnirs(
    file_path = example_mnirs("moxy_ramp"),
    nirs_channels = c(smo2_left = "SmO2 Live",
                      smo2_right = "SmO2 Live(2)"),
    time_channel = c(time = "hh:mm:ss"),
    verbose = FALSE
)

## note the options to display time values as `h:mm:ss` with 8 breaks
plot(data, time_labels = TRUE, n.breaks = 8)
```
