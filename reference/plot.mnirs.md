# Plot *mnirs* objects

Create a simple plot for objects returned from
[`create_mnirs_data()`](https://jemarnold.github.io/mnirs/reference/create_mnirs_data.md).

## Usage

``` r
# S3 method for class 'mnirs'
plot(x, ...)
```

## Arguments

- x:

  Object of class *"mnirs"* returned from
  [`create_mnirs_data()`](https://jemarnold.github.io/mnirs/reference/create_mnirs_data.md)

- ...:

  Additional arguments:

  `na.omit`

  :   A logical to omit missing (`NA`) values for better display of
      connected lines. `na.omit = FALSE` (the *default*) can be used to
      identify missing values.

  `label_time`

  :   A logical to display x-axis time values formatted as *"hh:mm:ss"*
      using
      [`scales::label_time()`](https://scales.r-lib.org/reference/label_date.html).
      `label_time = FALSE` (the *default*) will display simple numeric
      values on the x-axis.

  `n`

  :   A numeric value to define the number of breaks in both x- and
      y-axes.

## Value

A [ggplot2](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
## call an example *{mnirs}* data file
file_path <- example_mnirs("moxy_ramp")

data_table <- read_mnirs(
    file_path,
    nirs_channels = c(smo2_right = "SmO2 Live", ## identify and rename channels
                      smo2_left = "SmO2 Live(2)"),
    time_channel = c(time = "hh:mm:ss"), ## date-time format will be converted to numeric
    verbose = FALSE                      ## hide warnings & messages
)

## note the hidden plot option to display time values as `hh:mm:ss`
plot(data_table, label_time = TRUE)
```
