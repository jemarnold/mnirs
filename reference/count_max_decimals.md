# Count maximum decimal places across a numeric vector

Returns the largest number of decimal places present in any finite,
non-NA element of `x`. Used internally by
[`signif_trailing()`](https://jemarnold.github.io/mnirs/reference/signif_trailing.md)
for `format = "max_digits"`.

## Usage

``` r
count_max_decimals(x)
```

## Arguments

- x:

  A numeric vector.

## Value

A single non-negative integer.
