# Shift data range

Move the range of data channels in a data frame up or down, while
preserving the absolute amplitude/dynamic range of each channel, and the
relative scaling across channels. e.g. shift the minimum data value to
zero for all positive values, or shift the mean of the first time span
in a recording to zero.

## Usage

``` r
shift_mnirs(
  data,
  nirs_channels = list(NULL),
  time_channel = NULL,
  to = NULL,
  by = NULL,
  width = NULL,
  span = NULL,
  position = c("min", "max", "first"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing time series data and
  metadata.

- nirs_channels:

  A character vector giving the names of mNIRS columns to operate on.
  Must match column names in `data` exactly.

  - If `NULL` (default), the `nirs_channels` metadata attribute of
    `data` is used.

- time_channel:

  A character string giving the name of the time or sample column. Must
  match a column name in `data` exactly.

  - If `NULL` (default), the `time_channel` metadata attribute of `data`
    is used.

- to:

  A numeric value in units of `nirs_channels` to which the data channels
  will be shifted, e.g. shift the minimum value to zero.

- by:

  A numeric value in units of `nirs_channels` by which the data channels
  will be shifted, e.g. shift all values up by 10 units.

- width:

  An integer defining the local window in number of samples around `idx`
  in which to perform the operation., between
  `[idx - floor(width/2), idx + floor(width/2)]`.

- span:

  A numeric value defining the local window timespan around `idx` in
  which to perform the operation. In units of `time_channel` or `t`,
  between `[t - span/2, t + span/2]`.

- position:

  Indicates where the reference values will be shifted from.

  `"min"`

  :   (The *default*) will shift the minimum value(s) `to` or `by` the
      specified value.

  `"max"`

  :   Will shift the maximum value(s) `to` or `by` the specified values.

  `"first"`

  :   Will shift first value(s) `to` or `by` the specified values.

- verbose:

  Logical. Default is `TRUE`. Will display or silence (if `FALSE`)
  warnings and information messages helpful for troubleshooting. A
  global default can be set via `options(mnirs.verbose = FALSE)`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of class *"mnirs"* with metadata available with
[`attributes()`](https://rdrr.io/r/base/attributes.html).

## Details

`nirs_channels = list()` can be used to group data channels (column
names) to preserve absolute or relative scaling.

- Channels grouped together in a vector (e.g. `list(c("A", "B"))`) will
  be shifted to a common value, and the relative scaling within that
  group will be preserved.

- Channels in separate list vectors (e.g. `list("A", "B")`) will be
  shifted independently, and relative scaling between groups will be
  lost.

- A single vector of channel names (e.g. `c("A", "B")`) will group
  channels together.

- Channels (columns) in `data` not explicitly defined in `nirs_channels`
  will be passed through untouched to the output data frame.

`nirs_channels` and `time_channel` can be retrieved automatically from
`data` of class *"mnirs"* which has been processed with `{mnirs}`, if
not defined explicitly. This will default to returning all
`nirs_channels` grouped together, and should be defined explicitly for
other grouping arrangements.

Only one of either `to` or `by` and one of either `width` or `span`
should be defined. If both of either pairing are defined, `to` will be
preferred over `by`, and `width` will be preferred over `span`.

## Examples

``` r
## read example data
data <- read_mnirs(
    file_path = example_mnirs("moxy_ramp"),
    nirs_channels = c(smo2_right = "SmO2 Live",
                      smo2_left = "SmO2 Live(2)"),
    time_channel = c(time = "hh:mm:ss"),
    verbose = FALSE
) |>
    shift_mnirs(
        nirs_channels = list(smo2_right, smo2_left),
        to = 0,            ## each channel will be shifted to zero
        span = 120,        ## shift the mean of the first 120 sec
        position = "first",
        verbose = FALSE
    )

data
#> # A tibble: 2,203 × 3
#>     time smo2_right smo2_left
#>    <dbl>      <dbl>     <dbl>
#>  1 0          -1.55     2.21 
#>  2 0.400      -1.55     2.21 
#>  3 0.960      -1.55     2.21 
#>  4 1.51       -1.55     0.215
#>  5 2.06       -1.55     0.215
#>  6 2.61       -1.55     0.215
#>  7 3.16       -1.55     0.215
#>  8 3.71        1.45     1.21 
#>  9 4.26        1.45     1.21 
#> 10 4.81        1.45     1.21 
#> # ℹ 2,193 more rows

# \donttest{
    if (requireNamespace("ggplot2", quietly = TRUE)) {
        plot(data, time_labels = TRUE) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dotted")
    }

# }  
```
