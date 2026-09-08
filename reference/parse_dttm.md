# Parse character date-times with one `dttm_opts` format to local POSIXct

Time-only strings are anchored to today's local midnight, matching the
Excel fraction-of-day convention.

## Usage

``` r
parse_dttm(x, fmt)
```
