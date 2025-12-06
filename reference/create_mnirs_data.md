# Create an *mnirs* data frame with metadata

Manually add class *"mnirs"* and metadata to an existing data frame.

## Usage

``` r
create_mnirs_data(data, ...)
```

## Arguments

- data:

  A data frame with existing metadata (`attributes(data)`).

- ...:

  Additional arguments with metadata to add to the data frame. Can be
  either seperate named arguments or a list of named values.

  - nirs_device

  - nirs_channels

  - time_channel

  - event_channel

  - sample_rate

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of class *"mnirs"* with metadata available with
[`attributes()`](https://rdrr.io/r/base/attributes.html).

## Details

Typically will only be called internally, but can be used to inject
*mnirs* metadata into any data frame.

## Examples

``` r
df <- data.frame(
    A = 1:3,
    B = seq(10, 30, 10),
    C = seq(11, 33, 11)
)

attributes(df)
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
    df,
    nirs_channels = c("B", "C"),
    time_channel = "A",
    sample_rate = 1
)

attributes(nirs_data)
#> $class
#> [1] "mnirs"      "tbl_df"     "tbl"        "data.frame"
#> 
#> $row.names
#> [1] 1 2 3
#> 
#> $names
#> [1] "A" "B" "C"
#> 
#> $nirs_channels
#> [1] "B" "C"
#> 
#> $time_channel
#> [1] "A"
#> 
#> $sample_rate
#> [1] 1
#> 
```
