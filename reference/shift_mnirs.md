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
  nirs_channels = list(),
  time_channel = NULL,
  to = NULL,
  by = NULL,
  width = NULL,
  span = NULL,
  position = c("min", "max", "first"),
  inform = TRUE
)
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing at least one column with
  numeric time or sample values, and one column with numeric mNIRS
  values, along with metadata.

- nirs_channels:

  A [`list()`](https://rdrr.io/r/base/list.html) of character vectors
  indicating the column names for data channels to be shifted (see
  *Details*).

  `list("A", "B", "C")`

  :   Will shift each channel independently, losing the relative scaling
      between channels.

  `list(c("A", "B", "C"))`

  :   Will shift all channels together, preserving the relative scaling
      between channels.

  `list(c("A", "B"), c("C", "D"))`

  :   Will shift channels `A` and `B` in one group, and channels `C` and
      `D` in another group, preserving relative scaling within, but not
      between groups.

  Must match column names in data exactly. Will be taken from metadata
  if not defined explicitly.

- time_channel:

  A character string indicating the time or sample channel name. Must
  match column names in `data` exactly. Will be taken from metadata if
  not defined explicitly.

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

- inform:

  A logical to display (the *default*) or `FALSE` to silence warnings
  and information messages used for troubleshooting.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of class *"mnirs"* with metadata available with
[`attributes()`](https://rdrr.io/r/base/attributes.html).

## Details

`nirs_channels = list()` can be used to group data channels to preserve
absolute or relative scaling.

- Channels grouped together in a list item will be shifted to a common
  value, and the relative scaling within that group will be preserved.

- Channels grouped in separate list items will be shifted independently,
  and relative scaling between groups will be lost.

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
library(ggplot2)

## read example data
data_shifted <- read_mnirs(
    file_path = example_mnirs("moxy_ramp"),
    nirs_channels = c(smo2 = "SmO2 Live"),
    time_channel = c(time = "hh:mm:ss"),
    inform = FALSE
) |>
    resample_mnirs(inform = FALSE) |>
    replace_mnirs(
        invalid_values = c(0, 100),
        outlier_cutoff = 3,
        width = 10,
        inform = FALSE
    ) |>
    filter_mnirs(na.rm = TRUE, inform = FALSE) |>
    shift_mnirs(
        to = 0,             ## NIRS values will be shifted to zero
        span = 120,         ## shift the first 120 sec of data to zero
        position = "first",
        inform = FALSE
    )

plot(data_shifted, label_time = TRUE) +
    geom_hline(yintercept = 0, linetype = "dotted")
```
