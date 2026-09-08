# Parse time_channel character or dttm to numeric seconds

Parse time_channel character or dttm to numeric seconds

## Usage

``` r
parse_time_channel(x, start_timestamp = NULL, zero_time = FALSE)
```

## Arguments

- x:

  The time column vector: numeric, character, or POSIXct.

- start_timestamp:

  Optional POSIXct from the file header, evaluated lazily only when `x`
  is not an absolute date-time series.

- zero_time:

  Logical; re-base numeric time to start from zero.

## Value

A list of `time` (numeric seconds), `timestamp` (POSIXct vector or
`NULL`), and `start_timestamp` (POSIXct or `NULL`).
