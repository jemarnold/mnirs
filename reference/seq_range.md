# Generate numeric sequence from range of a vector

Creates a numeric sequence spanning the range of input vector with
either a specified step size or a desired output length.

## Usage

``` r
seq_range(
  x,
  by = 1,
  length.out = NULL,
  direction = c("up", "down"),
  env = rlang::caller_env()
)
```

## Arguments

- x:

  A numeric vector.

- by:

  A numeric step size for the output sequence. *Default* is `1`. Sign
  determines order of returned vector (negative `by` returns a
  descending sequence). `direction` takes precedence over `by` sign.

- length.out:

  A positive integer giving the desired length of the sequence.
  *Default* is `NULL`. If supplied, takes precedence over `by`.

- direction:

  Order of returned vector. Either `"up"` for ascending or `"down"` for
  descending. If supplied, takes precedence over the `by` sign.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A numeric vector spanning the range of the input `x`.

## Details

The output vector will likely be a different length than the input `x`.

## See also

[`seq()`](https://rdrr.io/r/base/seq.html),
[`range()`](https://rdrr.io/r/base/range.html)
