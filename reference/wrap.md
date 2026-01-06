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
