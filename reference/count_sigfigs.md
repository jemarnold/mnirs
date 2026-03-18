# Count maximum significant figures across a numeric vector

Returns the largest number of significant figures present in any finite,
non-NA element of `x`. Used internally by
[`signif_trailing()`](https://jemarnold.github.io/mnirs/reference/signif_trailing.md)
for `format = "signif"`.

## Usage

``` r
count_sigfigs(x)
```

## Arguments

- x:

  A numeric vector.

## Value

A single positive integer (minimum 1).
