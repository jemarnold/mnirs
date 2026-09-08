# Parse channel names from the Oxysoft "Legend" metadata block

Legend rows above the numeric header row map column ids to trace names.
Returns a channel list of named mappings `c(new_name = "original_col")`
plus `alias` mapping raw trace names to column ids, or `NULL` when the
legend is missing or malformed.

## Usage

``` r
parse_oxysoft_legend(raw, header_row)
```

## Arguments

- raw:

  A raw character data frame from
  [`read_file()`](https://jemarnold.github.io/mnirs/reference/read_file.md).

- header_row:

  Integer row index of the numeric data table header.
