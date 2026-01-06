# Preserve and Restore NA Information Within a Vector

`preserve_na()` stores `NA` vector positions and extracts valid non-`NA`
values for later restoration with `restore_na()`.

`restore_na()` restores `NA` values to their original vector positions
after processing valid non-`NA` values returned from `preserve_na()`.

## Usage

``` r
preserve_na(x)

restore_na(y, na_info)
```

## Arguments

- x:

  A vector containing missing `NA` values.

- y:

  A vector of valid non-`NA` values returned from `preserve_na()`.

- na_info:

  A list returned from `preserve_na()`.

## Value

`preserve_na()` returns a list `na_info` with components:

- `na_info$x_valid`: A vector with `NA` values removed.

- `na_info$x_length`: A numeric value of the original input vector
  length.

- `na_info$na_idx`: A logical vector preserving `NA` positions.

`restore_na()` returns a vector `y` the same length as the original
input vector `x` with `NA` values restored to their original positions.
