# Create an *mnirs* data frame with metadata

Manually add class `"mnirs"` and metadata to an existing data frame.

## Usage

``` r
create_mnirs_data(data, ...)
```

## Arguments

- data:

  A data frame with existing metadata (accessed with
  `attributes(data)`).

- ...:

  Additional arguments with metadata to add to the data frame. Can be
  either seperate named arguments or a list of named values.

  - nirs_device

  - nirs_channels

  - time_channel

  - event_channel

  - sample_rate

  - start_timestamp

  - interval_times

  - interval_span

  `nirs_channels`, `time_channel`, and `event_channel` accept named
  character vectors in the same form as
  [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md);
  `c(renamed = "original_name")`. Existing column names can be renamed,
  and the new names specified as `*_channel` in metadata.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of class `"mnirs"`. Metadata are stored as attributes and can be
accessed with `attributes(data)`.

## Details

Intended primarily for internal use, but can be used to inject *mnirs*
metadata into any data frame.

## Examples

``` r
data <- data.frame(
    A = 1:3,
    B = seq(10, 30, 10),
    C = seq(11, 33, 11)
)

attributes(data)
#> $names
#> [1] "A" "B" "C"
#> 
#> $class
#> [1] "data.frame"
#> 
#> $row.names
#> [1] 1 2 3
#> 

## inject metadata
nirs_data <- create_mnirs_data(
    data,
    nirs_channels = c("B", "C"),
    time_channel = "A",
    sample_rate = 1
)

attributes(nirs_data)
#> $nirs_channels
#> [1] "B" "C"
#> 
#> $time_channel
#> [1] "A"
#> 
#> $sample_rate
#> [1] 1
#> 
#> $names
#> [1] "A" "B" "C"
#> 
#> $row.names
#> [1] 1 2 3
#> 
#> $class
#> [1] "mnirs"      "tbl_df"     "tbl"        "data.frame"
#> 

## rename channels and update metadata
create_mnirs_data(
    nirs_data,
    nirs_channels = c(smo2 = "B", thb = "C"),
    time_channel = c(time = "A")
)
#> # A tibble: 3 × 3
#>    time  smo2   thb
#>   <int> <dbl> <dbl>
#> 1     1    10    11
#> 2     2    20    22
#> 3     3    30    33
```
