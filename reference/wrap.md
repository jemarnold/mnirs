# Wrap vector elements

Rotates vector elements by moving the first `n` elements to the end.

## Usage

``` r
wrap(x, n = 0L)
```

## Arguments

- x:

  A vector.

- n:

  An integer specifying number of elements to move from start to end.
  *Default* is `0` (no wrapping). Negative values move elements from end
  to start.

## Value

A vector with all the same elements as `x`.

## Details

The function:

- Returns `x` unchanged if `n = 0`.

- Moves first `n` elements to the end of the vector.

- For negative `n`, effectively moves elements from end to start.

- If `n` is larger than `length(x)`, positions wrap around.

## Examples

``` r
x <- 1:5
mnirs:::wrap(x, 2)
#> [1] 3 4 5 1 2
mnirs:::wrap(x, 0)
#> [1] 1 2 3 4 5

## wrapping with n larger than vector length
mnirs:::wrap(x, 7)  ## same as wrap(x, 2)
#> [1] 3 4 5 1 2

## negative wrapping (move from end to start)
mnirs:::wrap(x, -1)
#> [1] 5 1 2 3 4

## works with any vector type
mnirs:::wrap(letters[1:4], 1)
#> [1] "b" "c" "d" "a"
```
