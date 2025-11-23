# Read *mnirs* data from file

Read files exported from most commercially available mNIRS devices and
return a data frame of class *"mnirs"* with recorded time series data
and metadata.

## Usage

``` r
read_mnirs(
  file_path,
  nirs_channels,
  time_channel = NULL,
  event_channel = NULL,
  sample_rate = NULL,
  add_timestamp = FALSE,
  zero_time = FALSE,
  keep_all = FALSE,
  inform = TRUE
)
```

## Arguments

- file_path:

  The file path including extension (either *`".xlsx"`*, *`".xls"`*, or
  *`".csv"`*) to import.

- nirs_channels:

  A character vector indicating the mNIRS column names to import from
  the file. Must match column names in the data file exactly. A named
  character vector can be used to rename columns in the form:
  `c(new_name = "original_name")` (see *Details*).

- time_channel:

  An *optional* character string indicating the time or sample column
  name to import from the file. Must match column names in the data file
  exactly. A named character vector can be used to rename columns in the
  form: `c(new_name = "original_name")`. Time will be converted to
  *numeric* format (see *Details*).

- event_channel:

  An *optional* character string indicating the event or lap column name
  to import from the file. Must match column names in the data file
  exactly. A named character vector can be used to rename columns in the
  form: `c(new_name = "original_name")` (see *Details*).

- sample_rate:

  An *optional* numeric value for the exported sample rate in Hz. If not
  defined explicitly, will be estimated from the data (see *Details*).

- add_timestamp:

  `<under development>` A logical to add a *"timestamp"* column with
  date-time for each sample (class *POSIXct*), if present in the data
  file. If no absolute timestamp is detected, will instead return
  relative time as *hh:mm:ss*.

- zero_time:

  A logical to re-calculate `time_channel` to start from zero or `FALSE`
  keep the original values (the *default*).

- keep_all:

  A logical to include all columns detected from the file or `FALSE` to
  only include the explicitly specified data columns (the *default*).

- inform:

  A logical to display (the *default*) or `FALSE` to silence warnings
  and information messages used for troubleshooting.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of class *"mnirs"* with metadata available with
[`attributes()`](https://rdrr.io/r/base/attributes.html).

## Details

Channel names are matched to a single row, representing the header row
for data columns anywhere in the data file, not necessarily the top row
of the file.

Channels can be renamed in the format `c(new_name = "original_name")`,
where `*"original_name"*` should exactly match the column names found in
the file.

If there are duplicate column names in the file, the channel names will
attempt to match them in the order in which they appear. You may want to
confirm that the correct columns have been assigned to each channel as
intended.

`nirs_channels` must be defined explicitly and match column names
exactly. If `time_channel` is left blank, the function will attempt to
identify a time column automatically based on column names or values
containing time (`POSIXct`) data or time-formatted character strings
(e.g. *"hh:mm:ss"*).

`time_channel` will typically contain time values in seconds. However,
some NIRS devices (for example, *Artinis* devices recorded with
*Oxysoft*) export the sample index (i.e. integer row numbers). If
*Oxysoft* export sample rate is detected in the file metadata, a
`"time"` column will be added converting the sample indices to time
values in seconds.

When the `time_channel` is provided in date-time format, it will be
converted to numeric values and re-calculated from zero. A timestamp
column can be included with absolute date-time (e.g. *"yyyy-mm-dd
hh:mm:ss"*) if unix timestamps are present in the data file. Otherwise,
relative time (*"hh:mm:ss"*) will be returned.

If `time_channel` contains irregular sampling (i.e., non-sequential,
repeated, or unordered values) a warning will be displayed (if
`inform = TRUE`) suggesting that the user confirm the file data
manually.

`sample_rate` is required for certain `{mnirs}` functions to work
properly and can be carried forward in the data frame metadata. If it is
not defined explicitly, it will be estimated from the differences
between values in the `time_channel`. As above, in certain cases where
the `time_channel` represents sample indices rather than time values,
`sample_rate` will be inaccurately estimated to be 1 Hz. In such cases,
`sample_rate` should be defined explicitly.

Columns and rows which contain entirely missing data (`NA`) are omitted.

`inform = TRUE` will display warnings and information messages which can
be useful for troubleshooting. Errors causing abort messages will always
be displayed. Messages can be silenced globally with
`options(mnirs.inform = FALSE)`.

## Examples

``` r
## call an example mNIRS data file
file_path <- example_mnirs("moxy_ramp")

data_table <- read_mnirs(
    file_path,
    nirs_channels = c(smo2_right = "SmO2 Live", ## identify and rename channels
                      smo2_left = "SmO2 Live(2)"),
    time_channel = c(time = "hh:mm:ss"), ## date-time format will be converted to numeric
    inform = FALSE                       ## hide warnings & messages
)

data_table
#> # A tibble: 2,203 × 3
#>     time smo2_right smo2_left
#>    <dbl>      <dbl>     <dbl>
#>  1  0            54        68
#>  2  0.4          54        68
#>  3  0.96         54        68
#>  4  1.51         54        66
#>  5  2.06         54        66
#>  6  2.61         54        66
#>  7  3.16         54        66
#>  8  3.71         57        67
#>  9  4.26         57        67
#> 10  4.81         57        67
#> # ℹ 2,193 more rows
```
