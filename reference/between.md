# Detect if numeric values fall between a range

Vectorised inclusive within `x >= left` & `x <= right`, or exclusive
between `x > left` & `x < right`. Each side can be specified separately.

## Usage

``` r
between(x, left, right, inclusive = c("left", "right"))
```

## Arguments

- x:

  A numeric vector.

- left, right:

  Numeric boundary values for `x`

- inclusive:

  A character vector to specify which of `left` and/or `right` boundary
  values should be included in the range, or both (the default), or
  excluded if `FALSE`.

## Value

A logical vector the same length as `x`.

## Details

`inclusive = FALSE` can be used to test for positive non-zero values:
`between(x, 0, Inf, inclusive = FALSE)`.

## See also

[`dplyr::between()`](https://dplyr.tidyverse.org/reference/between.html)
