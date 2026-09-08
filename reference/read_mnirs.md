# Read *mnirs* data from file

Import time-series data exported from common muscle NIRS (mNIRS) devices
and return a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
(data frame) of class `"mnirs"` with the specified signal channels and
metadata.

## Usage

``` r
read_mnirs(
  file_path,
  nirs_channels = NULL,
  time_channel = NULL,
  event_channel = NULL,
  sample_rate = NULL,
  add_timestamp = FALSE,
  zero_time = FALSE,
  keep_all = FALSE,
  verbose = TRUE
)
```

## Arguments

- file_path:

  Path of the data file to import. Supported file extensions include
  `".xls(x)"`, `".csv"`, `".txt"`, and `".ftn(2)"`.

- nirs_channels:

  A character vector of one or more column names containing mNIRS
  signals to import. Names must match the file contents exactly.

  - If `NULL` (*default*), `read_mnirs()` attempts to automatically
    detect known `nirs_channel` names from the file contents.

  - A named character vector is used to rename columns, in the form
    `c(renamed = "original_name")`.

- time_channel:

  A single character vector for the time (or sample) column name. Must
  match the file contents exactly.

  - If `NULL` (*default*), `read_mnirs()` attempts to automatically
    detect a time-like column from known device defaults, and/or
    time-formatted values.

  - A named character vector is used to rename the column, e.g.
    `c(time = "original_name")`.

- event_channel:

  An *optional* single character vector for the event or lap column
  name. Must match the file contents exactly. A named character vector
  is used to rename the column, e.g. `c(event = "original_name")`.

- sample_rate:

  An *optional* numeric sample rate in Hz. If `NULL` (*default*), the
  sample rate is estimated from `time_channel` (see *Details*).

- add_timestamp:

  Logical. Default is `FALSE`. If `TRUE` and the source data contain
  date-time (POSIXct) values, will add a `"timestamp"` column in
  addition to the specified `time_channel` as a numeric time column.

- zero_time:

  Logical. Default is `FALSE`. If `TRUE`, re-calculates numeric
  `time_channel` values to start from zero.

- keep_all:

  Logical. `FALSE` (*default*) will only keep the channels explicitly
  specified in `channels`. If `TRUE`, will keep all columns found in the
  file data table.

  - If no `channels` are specified and the NIRS device file format is
    recognised, then all columns in the file data table will be returned
    to allow exploration of the file.

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of class `"mnirs"`. Metadata are stored as attributes and can be
accessed with `attributes(data)`.

## Details

### Header detection

`read_mnirs()` searches the file for a header row containing the
requested channel names. The header row does not need to be the first
row in the file.

- If duplicate column names exist, they are made unique with a numbered
  suffix (e.g. `*_1`), and can be renamed accordingly:
  `nirs_channels = c(smo2_left = "smo2", smo2_right = "smo2_1")`.

- Unnamed columns containing data in the source file will be renamed to
  `col_n`, where `n` is the ordered column number in the file (e.g.
  `col_6`). *Artinis Oxysoft* files are an exception to this renaming
  convention. See *Artinis Oxysoft exports* below).

### Renaming channels

All `channels` can be renamed with a named character vector in the form
`c(renamed = "original_name")`. The `"original_name"` must match the
file contents header row exactly.

### Artinis Oxysoft exports

*Artinis Oxysoft* files have numbered data columns, with a "Legend"
metadata block with channel names. `read_mnirs()` can detect and rename
these channels automatically:

- `nirs_channels` names become clean lower-case column names with
  underscores (e.g. `"Rx1 - Tx1 O2Hb"` becomes `rx1_tx1_o2hb`). Channels
  can still be renamed by any of column number, cleaned name, or legend
  trace name, e.g. `nirs_channels = c(o2hb = 2)`,
  `c(o2hb = "rx1_tx1_o2hb")`, or `c(o2hb = "Rx1 - Tx1 O2Hb")`.

- `"(Sample number)"` column is renamed `sample`, and a `time` column in
  seconds is automatically derived from the export sample rate.

- `"(Event)"` column is renamed `event` and set as `event_channel`.

- *Oxysoft* exports a trailing un-numbered column containing optional
  event label text. This is renamed `labels` and returned with
  `keep_all = TRUE`, or dropped when empty. It can be selected as the
  event column explicitly with `event_channel = c(event = "labels")`.

Explicit `nirs_channels`, `time_channel`, and `event_channel` renaming
(as above) overrides automatically detected names.

### PIONIRS exports

*PIONIRS* `.ftn(2)` files are detected with `"Time"` as `time_channel`,
`"TagLabel"` as `event_channel`, and `StO2` channels as `nirs_channels`.
The `"Iteration"` sample index and numeric `"Tag"` companion columns are
returned beside `time_channel` and `event_channel` with
`keep_all = TRUE`.

### Time parsing

If `time_channel` is left as `NULL`, it can be resolved from a known
NIRS device default, or by detecting a time-like column name (e.g.
`"time"`, `"hh:mm:ss"`), or by detecting a column with date-time
formatted (POSIXct-like) values.

If `time_channel` is a date-time (POSIXct) format, it will be converted
to numeric and re-based to start from `0`, regardless of `zero_time`.

### Sample rate

If `sample_rate` is not specified, it is estimated from differences in
`time_channel`. When irregular time sampling is detected, the estimated
median `sample_rate` will be approximated as common known recording rate
(e.g. an estimated rate of `11` may be rounded to `10 Hz`).

If `time_channel` is specified as a sample index (e.g. *Artinis Oxysoft
"sample"* or *PIONIRS "Iterations"*), `sample_rate` will be
mis-estimated as `1 Hz`. `sample_rate` should be specified explicitly in
this case.

### Data cleaning

Entirely empty rows and columns are removed. Invalid values (e.g.
`c(NaN, Inf, "-")`) are standardized to `NA`. A warning is displayed
(respecting `verbose`) when irregular sampling is detected (e.g.
non-monotonic, repeated, or unequal `time_channel` values). In this
case, it is recommended to use
[`resample_mnirs()`](https://jemarnold.github.io/mnirs/reference/resample_mnirs.md)
to standardise the time grid to the desired `sample_rate`.

## Examples

``` r
read_mnirs(
    file_path = example_mnirs("moxy_ramp"), ## call an example data file
    nirs_channels = c(
        smo2_left = "SmO2 Live",            ## identify and rename channels
        smo2_right = "SmO2 Live(2)"
    ),
    time_channel = c(time = "hh:mm:ss"),    ## date-time format will be converted to numeric
    event_channel = NULL,                   ## leave blank if unused
    sample_rate = NULL,                     ## if blank, will be estimated from time_channel
    add_timestamp = FALSE,                  ## omit a date-time timestamp column
    zero_time = TRUE,                       ## recalculate time values from zero
    keep_all = FALSE,                       ## return only the specified data channels
    verbose = TRUE                          ## show warnings & messages
)
#> ! Estimated `sample_rate` = 2 Hz.
#> ℹ Define `sample_rate` explicitly to override.
#> Warning: ! Duplicate or irregular `time_channel` samples detected.
#> ℹ time = 211.59 and 1183.6.
#> ℹ Re-sample with `mnirs::resample_mnirs()`.
#> # A tibble: 2,202 × 3
#>     time smo2_left smo2_right
#>    <dbl>     <dbl>      <dbl>
#>  1 0            54         68
#>  2 0.560        54         68
#>  3 1.11         54         66
#>  4 1.66         54         66
#>  5 2.21         54         66
#>  6 2.76         54         66
#>  7 3.31         57         67
#>  8 3.86         57         67
#>  9 4.41         57         67
#> 10 4.96         57         67
#> # ℹ 2,192 more rows
```
