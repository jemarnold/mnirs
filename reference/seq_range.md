# Generate numeric sequence from range of a vector

Creates a numeric sequence spanning the range of input data with
specified step size and direction. Optionally rounds the range endpoints
before generating the sequence.

## Usage

``` r
seq_range(x, by = 1, direction = c("up", "down"), digits = NA)
```

## Arguments

- x:

  A numeric vector.

- by:

  A numeric step size for the sequence. *Default* is `1`.

- direction:

  A character string specifying sequence direction. Either *"up"* (the
  *default*) for ascending or *"down"* for descending sequence.

- digits:

  An integer specifying number of decimal places to round range
  endpoints, or `NA` (the *default*) for no rounding.

## Value

A numeric vector spanning the range of the input data.

## Details

The output vector will likely be a different length than the input
vector.

The function:

- Calculates the range of `x` (ignoring NA values).

- Optionally rounds the range endpoints if `digits` is specified.

- Generates a sequence with the specified step size.

- Reverses the sequence if `direction = "down"`.

## See also

[`seq()`](https://rdrr.io/r/base/seq.html),
[`range()`](https://rdrr.io/r/base/range.html)
