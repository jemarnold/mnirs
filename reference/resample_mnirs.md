# Re-sample a data frame

Up- or down-sample the number of samples in an *"mnirs"* data frame
using interpolation.

## Usage

``` r
resample_mnirs(
  data,
  time_channel = NULL,
  sample_rate = NULL,
  resample_rate = sample_rate,
  resample_time = NULL,
  method = c("linear", "locf", "NA"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing at least one column with
  numeric time or sample values, and one column with numeric mNIRS
  values, along with metadata.

- time_channel:

  A character string indicating the time or sample channel name. Must
  match column names in `data` exactly. Will be taken from metadata if
  not defined explicitly.

- sample_rate:

  A numeric value for the sample rate in Hz. Will be taken from metadata
  or estimated from `time_channel` if not defined explicitly.

- resample_rate:

  An *optional* numeric value indicating the desired output sample rate
  (in Hz) to re-sample the data frame. The *default*
  `resample_rate = sample_rate` will interpolate over missing and
  repeated samples within the bounds of the existing data rounded to the
  nearest value in Hz.

- resample_time:

  An *optional* numeric value indicating the desired sample time (in
  seconds) to re-sample the data frame.

- method:

  A character string indicating how to handle resampling (see *Details*
  for more on each method):

  `"linear"`

  :   Re-samples and replaces `NA`s via linear interpolation (the
      *default*) using
      [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html).

  `"locf"`

  :   (*"Last observation carried forward"*). Re-samples and replaces
      `NA`s with the most recent valid non-`NA` value to the left for
      trailing samples or to the right for leading samples, using
      [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html).

  `"NA"`

  :   Re-samples by matching values to their nearest value of
      `time_channel`, *without* interpolating across new samples or
      `NA`s in the original data frame.

- verbose:

  A logical to return (the *default*) or silence warnings and messages
  which can be used for data error checking. Abort errors will always be
  returned.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of class *"mnirs"* with metadata available with
[`attributes()`](https://rdrr.io/r/base/attributes.html).

## Details

This function uses
[`replace_missing()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
(based on [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html))
to interpolate across new samples in the re-sampled data range.

`time_channel` and `sample_rate` can be retrieved automatically from
`data` of class *"mnirs"* which has been processed with `{mnirs}`, if
not defined explicitly.

Otherwise, `sample_rate` will be estimated from the values in
`time_channel`. However, this may return unexpected values, and it is
safer to define `sample_rate` explicitly.

The *default* setting `resample_rate = sample_rate` will interpolate
over missing and repeated samples within the bounds of the existing data
rounded to the nearest `sample_rate`.

By *default*, `method = "linear"` or `"locf"` will interpolate across
`NA`s in the original data and any new samples between existing values
of `time_channel` (see
[`?replace_missing`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)).
Whereas `method = "NA"` will match values of numeric columns from the
original samples of `time_channel` to the new re-sampled samples,
without interpolation. Meaning `NA`s in the original data and any new
samples will be returned as `NA`.

## Examples

``` r
## read example data
data <- read_mnirs(
    file_path = example_mnirs("moxy_ramp"),
    nirs_channels = c(smo2 = "SmO2 Live"),
    time_channel = c(time = "hh:mm:ss"),
    verbose = FALSE
)
data
#> # A tibble: 2,203 × 2
#>     time  smo2
#>    <dbl> <dbl>
#>  1  0       54
#>  2  0.4     54
#>  3  0.96    54
#>  4  1.51    54
#>  5  2.06    54
#>  6  2.61    54
#>  7  3.16    54
#>  8  3.71    57
#>  9  4.26    57
#> 10  4.81    57
#> # ℹ 2,193 more rows

data_resampled <- resample_mnirs(
    data,
    time_channel = NULL,           ## taken from metadata
    sample_rate = NULL,
    # resample_rate = sample_rate, ## the default will re-sample to sample_rate
    method = "linear",             ## default linear interpolation across any new samples
    verbose = FALSE                ## will confirm the output sample rate
)

## note the altered "time" values 👇
data_resampled
#> # A tibble: 2,419 × 2
#>     time  smo2
#>    <dbl> <dbl>
#>  1   0    54  
#>  2   0.5  54  
#>  3   1    54  
#>  4   1.5  54  
#>  5   2    54  
#>  6   2.5  54  
#>  7   3    54  
#>  8   3.5  55.9
#>  9   4    57  
#> 10   4.5  57  
#> # ℹ 2,409 more rows
```
