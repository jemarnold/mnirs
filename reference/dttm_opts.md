# Datetime format strings for POSIXct parsing

Time-only format must stay first:
[`parse_dttm()`](https://jemarnold.github.io/mnirs/reference/parse_dttm.md)
treats `dttm_opts[1L]` as a relative time of day and the rest as
absolute date-times.

## Usage

``` r
dttm_opts
```
