# Specify interval boundaries by time, label, lap, or sample

Helper functions to define interval start or end boundaries for
[`extract_intervals()`](https://jemarnold.github.io/mnirs/reference/extract_intervals.md).

## Usage

``` r
by_time(...)

by_sample(...)

by_label(...)

by_lap(...)
```

## Arguments

- ...:

  Specify start or end boundaries.

  `by_time(...)`

  :   Numeric time values in units of `time_channel`.

  `by_label(...)`

  :   Character strings to match in `event_channel`. All matching
      occurrences are returned.

  `by_lap(...)`

  :   Integer lap numbers to match in `event_channel`. For `start`,
      resolves to the first sample of each lap. For `end`, resolves to
      the last sample.

  `by_sample(...)`

  :   Integer sample indices (row numbers).

## Value

An object of class `"mnirs_interval"` for use with the `start` and `end`
arguments of
[`extract_intervals()`](https://jemarnold.github.io/mnirs/reference/extract_intervals.md).

## Details

These helpers can be used explicitly for arguments `start`/`end`, or raw
values can be passed directly:

- Numeric → `by_time()`

- Character → `by_label()`,

- Explicit integer (e.g. `2L`) → `by_lap()`.

- Use `by_sample()` explicitly for sample indices.

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
    event_channel = c(lap = "Lap/Event"),
    zero_time = TRUE,
    verbose = FALSE
)

## start and end by time
extract_intervals(data, start = by_time(66), end = by_time(357))
#> ℹ `nirs_channels` grouped together by default.
#> $interval_1
#> # A tibble: 4,150 × 4
#>     time   lap smo2_left smo2_right
#>    <dbl> <int>     <dbl>      <dbl>
#>  1  6.04     1      67.6       70.7
#>  2  6.04     1      67.6       71.8
#>  3  6.24     1      67.6       71.4
#>  4  6.24     1      67.6       71.7
#>  5  6.30     1      67.6       71.6
#>  6  6.39     1      65.7       70.9
#>  7  6.48     1      67.4       72.0
#>  8  6.63     1      67.0       72.2
#>  9  6.69     1      67.6       70.8
#> 10  7.61     1      68.4       71.5
#> # ℹ 4,140 more rows
#> 

## start by lap
extract_intervals(data, start = by_lap(2, 4), span = 0)
#> ℹ `nirs_channels` grouped together by default.
#> $interval_1
#> # A tibble: 2,937 × 4
#>     time   lap smo2_left smo2_right
#>    <dbl> <int>     <dbl>      <dbl>
#>  1  65.9     2      68.4       71.6
#>  2  66.1     2      66.7       71.6
#>  3  66.2     2      68.1       73.8
#>  4  66.2     2      68.1       72.3
#>  5  66.3     2      68.1       72.3
#>  6  66.4     2      69.1       72.9
#>  7  66.6     2      68.8       72.9
#>  8  66.6     2      68.8       72.9
#>  9  66.8     2      68.8       73.3
#> 10  66.9     2      68.0       72.1
#> # ℹ 2,927 more rows
#> 
#> $interval_2
#> # A tibble: 2,943 × 4
#>     time   lap smo2_left smo2_right
#>    <dbl> <int>     <dbl>      <dbl>
#>  1  781.     4      69.5       70.7
#>  2  781      4      69.5       70.6
#>  3  781.     4      70.2       70.2
#>  4  781.     4      70.2       69.4
#>  5  781.     4      70.2       70.4
#>  6  781.     4      69.8       70.2
#>  7  782.     4      69.8       70.1
#>  8  782.     4      68.6       70.0
#>  9  782.     4      68.6       69.7
#> 10  782.     4      69.9       69.3
#> # ℹ 2,933 more rows
#> 

## introduce event_channel with "start" string
data$event <- NA_character_
data$event[1000] <- "start"
data <- create_mnirs_data(data, event_channel = "event")

## start by label, end by time
extract_intervals(data, start = by_label("start"), end = by_time(1500))
#> ℹ `nirs_channels` grouped together by default.
#> Warning: !  Interval 1 is partially outside data bounds.
#> ℹ Returning available data only.
#> $interval_1
#> # A tibble: 11,607 × 5
#>     time   lap smo2_left smo2_right event
#>    <dbl> <int>     <dbl>      <dbl> <chr>
#>  1  39.0     1      67.8       69.3 NA   
#>  2  39.0     1      67.8       69.1 NA   
#>  3  39.2     1      68.0       70.1 NA   
#>  4  39.2     1      68.0       69.8 NA   
#>  5  39.3     1      68.2       69.4 NA   
#>  6  39.4     1      68.2       69.7 NA   
#>  7  39.5     1      68.0       69.2 NA   
#>  8  39.6     1      68.5       69.2 NA   
#>  9  39.7     1      68.5       70.0 NA   
#> 10  39.9     1      68.5       69.8 NA   
#> # ℹ 11,597 more rows
#> 

## multiple intervals by sample index
extract_intervals(data, start = by_sample(1000, 1500), end = by_sample(2000, 2600))
#> ℹ `nirs_channels` grouped together by default.
#> $interval_1
#> # A tibble: 2,210 × 5
#>     time   lap smo2_left smo2_right event
#>    <dbl> <int>     <dbl>      <dbl> <chr>
#>  1  39.0     1      67.8       69.3 NA   
#>  2  39.0     1      67.8       69.1 NA   
#>  3  39.2     1      68.0       70.1 NA   
#>  4  39.2     1      68.0       69.8 NA   
#>  5  39.3     1      68.2       69.4 NA   
#>  6  39.4     1      68.2       69.7 NA   
#>  7  39.5     1      68.0       69.2 NA   
#>  8  39.6     1      68.5       69.2 NA   
#>  9  39.7     1      68.5       70.0 NA   
#> 10  39.9     1      68.5       69.8 NA   
#> # ℹ 2,200 more rows
#> 
#> $interval_2
#> # A tibble: 2,311 × 5
#>     time   lap smo2_left smo2_right event
#>    <dbl> <int>     <dbl>      <dbl> <chr>
#>  1  88.5     2      58.7       65.2 NA   
#>  2  88.7     2      59.7       65.0 NA   
#>  3  88.7     2      59.7       65.6 NA   
#>  4  88.9     2      58.6       66.4 NA   
#>  5  88.9     2      58.6       66.4 NA   
#>  6  89.1     2      58.9       65.3 NA   
#>  7  89.1     2      58.9       64.8 NA   
#>  8  89.2     2      58.9       65.7 NA   
#>  9  89.3     2      58.9       65.0 NA   
#> 10  89.4     2      58.8       65.1 NA   
#> # ℹ 2,301 more rows
#> 
```
