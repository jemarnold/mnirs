# Extract intervals from *mnirs* data

Detect and extract intervals around specified events from *"mnirs"* time
series data.

## Usage

``` r
extract_intervals(
  data,
  nirs_channels = NULL,
  time_channel = NULL,
  event_channel = NULL,
  sample_rate = NULL,
  event_times = NULL,
  event_labels = NULL,
  event_samples = NULL,
  event_groups = list("distinct", "ensemble"),
  span = list(c(-30, 180)),
  zero_time = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing time series data and
  metadata.

- nirs_channels:

  A character vector or a [`list()`](https://rdrr.io/r/base/list.html)
  of character vectors of mNIRS channel names to operate on within each
  interval (see *Details*). Names must match column names in `data`
  exactly.

  - If `NULL` (default), channels are retrieved from *"mnirs"* metadata.

  - Use multiple list items to include or exclude specific
    `nirs_channels` per interval.

- time_channel:

  A character string giving the name of the time or sample column. Must
  match a column name in `data` exactly.

  - If `NULL` (default), the `time_channel` metadata attribute of `data`
    is used.

- event_channel:

  An *optional* character string giving the name of an event/marker
  column to import. Required to specify `event_labels`. Must match
  column names in `data` exactly. Retrieved from metadata if not defined
  explicitly.

- sample_rate:

  An *optional* numeric sample rate (Hz) used to bin time values for
  ensemble-averaging. If `NULL`, will be estimated from `time_channel`
  (see *Details*).

- event_times:

  A numeric vector of `time_channel` values indicating event start times
  (see *Details*).

- event_labels:

  A character vector of strings to match in `event_channel`, indicating
  event starts. Matching is case-sensitive and must match exactly.

- event_samples:

  an integer vector of sample indices (row numbers) indicating event
  starts.

- event_groups:

  Either a character string or a
  [`list()`](https://rdrr.io/r/base/list.html) of integer vectors
  specifying how to group intervals (see *Details*).

  `"distinct"`

  :   The default. Extract each interval as an independent data frame.

  `"ensemble"`

  :   Ensemble-average each specified `nirs_channel` across all detected
      intervals, returning a single data frame.

  `list(c(1, 2), c(3, 4))`

  :   Ensemble-average each specified `nirs_channel` within each group
      and return one data frame per group.

- span:

  A [`list()`](https://rdrr.io/r/base/list.html) of two-element numeric
  vectors specifying the window around each event as `c(before, after)`,
  in units of `time_channel`.

- zero_time:

  Logical. Default is `FALSE`. If `TRUE`, re-calculates numeric
  `time_channel` values to start from zero within each interval data
  frame.

- verbose:

  Logical. Default is `TRUE`. Will display or silence (if `FALSE`)
  warnings and information messages helpful for troubleshooting. A
  global default can be set via `options(mnirs.verbose = FALSE)`.

## Value

A named [`list()`](https://rdrr.io/r/base/list.html) of
[tibbles](https://tibble.tidyverse.org/reference/tibble-package.html) of
class *"mnirs"*, with metadata available via
[`attributes()`](https://rdrr.io/r/base/attributes.html).

## Details

### Event specification

Interval events can be identified in three ways, in combination:

- `event_times`:

  Numeric time valuess in units of `time_channel`.

- `event_samples`:

  Integer sample indices (row numbers).

- `event_labels`:

  Character strings to match exactly in `event_channel`.

Events can be specified in any order, and will always be returned in the
order in which they appear in `data`.

### Per-interval `nirs_channels` for ensemble-averaging

When `event_groups = "ensemble"` or a list of numeric grouped intervals,
`nirs_channels` can be specified as a list of column names to override
ensemble-averaging across interval. For example, to exclude a bad
channel in one interval:

    nirs_channels = list(
      c("A", "B", "C"),
      c("A", "C") ## channel "B" is excluded
    )

If all grouped intervals can include all `nirs_channels`, or if
`event_groups = "distinct"`, a single `nirs_channels` character vector
can be supplied and recycled to all groups, or left as `NULL` for
channels to be taken from *"mnirs"* metadata.

### Interval time `span` windows

Each interval is defined relative to its event time in units of
`time_channel` as `[event_times + before, event_times + after]`.

- `before` is typically a negative value (window can extend before the
  event).

- `after` is typically a positive value (window can extend after the
  event).

- Both values can be either positive or negative to reference an
  interval window either completely before, or completely after the
  indicated event, respectively.

If an interval time span is partially out of bounds, available in-bounds
data are returned with a warning. Interval time spans entirely out of
bounds returns an error.

### Grouping events

`event_groups` controls whether extracted intervals are returned as
distinct data frames or ensemble-averaged.

- `"distinct"`:

  The default. Extract each interval and return a list of independent
  data frames.

- `"ensemble"`:

  Ensemble-average each specified `nirs_channel` across all detected
  intervals and return a one-item list with a single data frame.

- `list(c(1, 2), c(3, 4))`:

  Ensemble-average each specified `nirs_channel` within each group and
  return a list with one data frame for each group. Any intervals
  detected but not specified in `event_groups` are returned as distinct.

`event_groups` lists canned be named (e.g.
`list(low = c(1, 2), high = c(3, 4))`) and will pass those names to the
returned list of data frames. Otherwise, the return list will be named
`c("interval_1", "interval_2")` etc. for distinct intervals;
`"ensemble"` for ensemble-averaged; or `c("group_1_2", "group_3_4")`
etc. for custom grouping structure.

When `event_groups` is a list of numeric interval numbers, list items in
`nirs_channels` and `span` are recycled to the number of groups. If
lists are only partially specified (if there are more intervals or
groups detected than there are argument list items) The final argument
item is recycled forward as needed. Extra argument items are ignored.

## Examples

``` r
## read example data
data <- read_mnirs(
    example_mnirs("train.red"),
    nirs_channels = c(
        smo2_left = "SmO2 unfiltered",
        smo2_right = "SmO2 unfiltered"
    ),
    time_channel = c(time = "Timestamp (seconds passed)"),
    zero_time = TRUE,
    verbose = FALSE
) |>
    resample_mnirs(verbose = FALSE) ## avoid issues ensemble-averaging irregular samples

## extract intervals as a list of data frames
extract_intervals(
    data,
    nirs_channels = list(c(smo2_left, smo2_right)),
    event_times = c(368, 1093), ## specify interval events
    event_groups = "distinct",  ## return all unique intervals
    span = list(c(-20, 90)),    ## specify the event start-end timespans
    zero_time = TRUE,           ## start time from zero
    verbose = FALSE
)
#> $interval_1
#> # A tibble: 1,101 × 3
#>     time smo2_left smo2_right
#>    <dbl>     <dbl>      <dbl>
#>  1 -20        55.6       60.9
#>  2 -19.9      55.8       60.7
#>  3 -19.8      56.1       60.6
#>  4 -19.7      56.3       60.4
#>  5 -19.6      56.6       60.3
#>  6 -19.5      56.8       60.1
#>  7 -19.4      56.6       60.1
#>  8 -19.3      56.9       59.8
#>  9 -19.2      56.7       60.1
#> 10 -19.1      56.2       59.8
#> # ℹ 1,091 more rows
#> 
#> $interval_2
#> # A tibble: 1,101 × 3
#>     time smo2_left smo2_right
#>    <dbl>     <dbl>      <dbl>
#>  1 -20        56.2       57.2
#>  2 -19.9      55.7       57.4
#>  3 -19.8      55.3       57.0
#>  4 -19.7      55.3       58.6
#>  5 -19.6      55.3       58.8
#>  6 -19.5      55.3       57.9
#>  7 -19.4      55.3       59.1
#>  8 -19.3      55.3       59.0
#>  9 -19.2      55.3       57.6
#> 10 -19.1      55.7       57.4
#> # ℹ 1,091 more rows
#> 

## ensemble-average across multiple intervals
interval_list <- extract_intervals(
    data,
    nirs_channels = list(c(smo2_left, smo2_right)),
    event_times = c(368, 1093),
    event_groups = "ensemble", ## return ensemble-averaged intervals
    span = list(c(-20, 90)),
    zero_time = TRUE,
    verbose = FALSE
)

interval_list[[1L]]
#> # A tibble: 1,101 × 3
#>     time smo2_left smo2_right
#>    <dbl>     <dbl>      <dbl>
#>  1 -20        55.9       59.0
#>  2 -19.9      55.8       59.1
#>  3 -19.8      55.7       58.8
#>  4 -19.7      55.8       59.5
#>  5 -19.6      56.0       59.6
#>  6 -19.5      56.1       59.0
#>  7 -19.4      56.0       59.6
#>  8 -19.3      56.1       59.4
#>  9 -19.2      56.0       58.8
#> 10 -19.1      55.9       58.6
#> # ℹ 1,091 more rows

# \donttest{
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    plot(interval_list[[1L]], time_labels = TRUE) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted")
  }

# }
```
