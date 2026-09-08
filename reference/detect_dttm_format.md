# Detect the first `dttm_opts` format matching a character vector

Tested on the first non-empty value only, in UTC (local time zone
parsing is slow on Windows).

## Usage

``` r
detect_dttm_format(x)
```

## Value

A format string, or `NULL` when none match.
