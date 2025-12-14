# Detect if numeric values fall within range of a vector

Vectorised check for `x %in% vec`, inclusive or exclusive of left and
right boundary values, specified independently.

## Usage

``` r
within(x, vec, inclusive = c("left", "right"))
```

## Arguments

- x:

  A numeric vector.

- vec:

  A numeric vector from which `left` and `right` boundary values for `x`
  will be taken.

- inclusive:

  A character vector to specify which of `left` and/or `right` boundary
  values should be included in the range, or both (the default), or
  excluded if `FALSE`.

## Value

A logical vector the same length as `x`.

## Details

`inclusive = FALSE` can be used to test for positive non-zero values:
`between(x, vec, inclusive = FALSE)`.

## See also

[`dplyr::between()`](https://dplyr.tidyverse.org/reference/between.html)
