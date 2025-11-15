# Replace outliers, invalid values, and missing values

`replace_mnirs()` detects and removes local outliers and specified
invalid values in `nirs_channels` within an *"mnirs"* data frame, and
replaces missing `NA` values via interpolation methods.

`replace_invalid()` detects specified invalid values in vector data and
replaces them with the local median value or `NA`.

`replace_outliers()` detects local outliers in vector data with a Hampel
filter and replaces with the local median value or `NA`.

`replace_missing()` detects missing values in vector data and replaces
via interpolation methods.

## Usage

``` r
replace_mnirs(
  data,
  nirs_channels = NULL,
  time_channel = NULL,
  invalid_values = NULL,
  outlier_cutoff = NULL,
  width = NULL,
  span = NULL,
  method = c("linear", "median", "locf", "NA"),
  verbose = TRUE
)

replace_invalid(
  x,
  t = seq_along(x),
  invalid_values,
  width = NULL,
  span = NULL,
  method = c("median", "NA"),
  verbose = TRUE
)

replace_outliers(
  x,
  t = seq_along(x),
  outlier_cutoff = 3,
  width = NULL,
  span = NULL,
  method = c("median", "NA"),
  verbose = TRUE
)

replace_missing(
  x,
  t = seq_along(x),
  width = NULL,
  span = NULL,
  method = c("linear", "median", "locf"),
  ...
)
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing at least one column with
  numeric time or sample values, and one column with numeric mNIRS
  values, along with metadata.

- nirs_channels:

  A character vector of mNIRS channel names. Must match column names in
  `data` exactly. Will be taken from metadata if not defined explicitly.

- time_channel:

  A character string indicating the time or sample channel name. Must
  match column names in `data` exactly. Will be taken from metadata if
  not defined explicitly.

- invalid_values:

  A numeric vector of invalid values to be replaced, e.g.
  `invalid_values = c(0, 100, 102.3)`. The *default* `NULL` will not
  replace invalid values.

- outlier_cutoff:

  An integer for the local outlier threshold, as number of standard
  deviations above and below the local median. The *default*
  `outlier_cutoff = NULL` will not replace outliers.
  `outlier_cutoff = 3` is the standard replacement threshold following
  Pearson's rule.

- width:

  An integer defining the window in number of samples around `idx` in
  which to detect local outliers and perform median replacement, between
  `[idx - width, idx + width]`.

- span:

  A numeric value defining window timespan around `idx` in which to
  detect local outliers and perform median replacement. In units of
  `time_channel` or `t`, between `[t - span, t + span]`.

- method:

  A character string indicating how to handle replacement (see *Details*
  for more on each method):

  `"linear"`

  :   Replaces `NA`s via linear interpolation (the *default*) using
      [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html).

  `"median"`

  :   Replaces `NA`s with the local median of valid values within a
      centred window defined by either `width` or `span`.

  `"locf"`

  :   (*"Last observation carried forward"*). Replaces `NA`s with the
      most recent valid non-`NA` value to the left for trailing `NA`s or
      to the right for leading `NA`s, using
      [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html).

  `"NA"`

  :   Returns `NA`s without replacement.

- verbose:

  A logical to return (the *default*) or silence warnings and messages
  which can be used for data error checking. Abort errors will always be
  returned.

- x:

  A numeric vector.

- t:

  An *optional* numeric vector of time or sample number.

- ...:

  Additional arguments.

## Value

`replace_mnirs()` returns a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html) of
class *"mnirs"* with metadata available with
[`attributes()`](https://rdrr.io/r/base/attributes.html).

Vectorised `replace_*()` return a numeric vector the same length as `x`.

## Details

`replace_mnirs()` is a wrapper function expanding the vectorised
`replace_*` functions to operate on a data frame.

`nirs_channels` and `time_channel` can be retrieved automatically from
`data` of class *"mnirs"* which has been processed with `{mnirs}`, if
not defined explicitly.

Channels (columns) in `data` not explicitly defined in `nirs_channels`
will be passed through untouched to the output data frame.

`replace_invalid()` can be used to overwrite known invalid values in
exported data, such as `c(0, 100, 102.3)`.

- `<under development>`: *allow for overwriting all values greater or
  less than specified values.*

`replace_outliers()` will compute local rolling median values across
`x`, defined by either `width` number of samples, or `span` timespan in
units of `t`.

- Outliers are detected with robust median absolute deviation (MAD)
  method adapted from `pracma::hampel()`. Outliers equal to or less than
  the smallest absolute time series difference in `x` will be excluded,
  to avoid detecting negligible differences as outliers where local data
  have minimal or zero variation.

- Values of `x` outside local bounds defined by `outlier_cutoff` are
  identified as local outliers and either removed if `method = "NA"`, or
  replaced with the local median value (`method = "median"`, the
  *default*).

- This function will NOT replace `NA` values already existing in the
  `x`. They will be passed along in the returned vector. See
  `replace_missing()`.

- A high `outlier_cutoff` threshold makes the Hampel filter more
  forgiving. A low `outlier_cutoff` will declare more points to be
  outliers. `outlier_cutoff = 3` corresponds to Pearson's 3 sigma edit
  rule. `outlier_cutoff = 0` corresponds to Tukey's median filter.

`replace_missing()` will interpolate across missing values (`NA`s) as
specified by `method`.

- Leading and trailing `NA`s are replaced by *"nocb"* (*"next
  observation* *carried backward"*) and *"locf"*, respectively, for both
  `method = ` `"linear"` and `"locf"`, by applying `rule = 2` (see
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html)).

- `method = "median"` will calculate the local median of valid
  (non-`NA`) values to either side of `NA`s within a window defined by
  `width` number of samples, or a timespan defined by `span` in units of
  `t` (time). Such that sequential `NA`s will all be replaced by the
  same median value.

- If there are no valid values within `span` to one side of the `NA`
  value(s), it will be replaced with the median of the other side (i.e.
  for leading and trailing `NA`s). If there are no valid values within
  either side of `span`, the first valid sample on either side will be
  used (i.e. equivalent to `replace_missing(x, width = 1)`).

## See also

`pracma::hampel()`
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html)

## Examples

``` r
## vectorised operation
x <- c(1, 999, 3, 4, 999, 6)
replace_invalid(x, invalid_values = 999, width = 1, method = "median")
#> [1] 1 2 3 4 5 6

x_na <- replace_outliers(x, outlier_cutoff = 3, width = 1, method = "NA")
x_na
#> [1]  1 NA  3  4 NA  6

replace_missing(x_na, method = "linear")
#> [1] 1 2 3 4 5 6

## read example data
data <- read_mnirs(
    file_path = example_mnirs("moxy_ramp"),
    nirs_channels = c(smo2 = "SmO2 Live"),
    time_channel = c(time = "hh:mm:ss"),
    verbose = FALSE
)

## clean data
data_clean <- replace_mnirs(
    data,
    nirs_channels = NULL,       ## default to all nirs_channels in metadata
    time_channel = NULL,        ## default to time_channel in metadata
    invalid_values = c(0, 100), ## known invalid values in the data
    outlier_cutoff = 3,         ## recommended default value
    width = 7,                  ## local window to detect local outliers and replace missing values
    method = "linear",          ## linear interpolation over `NA`s
)

if (FALSE) { # \dontrun{
## plot original and and show where values have been replaced
plot(data, display_mmss = TRUE) +
    scale_colour_manual(
        breaks = c("smo2", "replaced"),
        values = palette_mnirs(2)
    ) +
    geom_point(
        data = data[data_clean$smo2 != data$smo2, ],
        aes(y = smo2, colour = "replaced")
    ) +
    geom_line(
        data = {
            data_clean[!is.na(data$smo2), "smo2"] <- NA
            data_clean
        },
        aes(y = smo2, colour = "replaced"), linewidth = 1
    )
} # }
```
