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

## Examples

``` r
data <- c(1.2, 3.7, 2.1, 4.9, 2.8)
mnirs:::seq_range(data)
#> [1] 1.2 2.2 3.2 4.2
mnirs:::seq_range(data, by = 0.5)
#> [1] 1.2 1.7 2.2 2.7 3.2 3.7 4.2 4.7
mnirs:::seq_range(data, by = 0.5, direction = "down")
#> [1] 4.7 4.2 3.7 3.2 2.7 2.2 1.7 1.2

## with rounding
mnirs:::seq_range(data, by = 0.2, digits = 1)
#>  [1] 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.4 3.6 3.8 4.0 4.2 4.4 4.6 4.8
```
