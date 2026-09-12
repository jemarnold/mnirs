# Batched 3-parameter least squares over a grid

Solves the normal equations of a 3-column linear model at every grid
point at once, given the Gram entries `g_ij = <c_i, c_j>` and right-hand
sides `b_i = <c_i, x>` as equal-shaped arrays (one element per grid
point). Used by the self-start initialisers to profile the non-linear
parameters on a grid without a per-point decomposition.

## Usage

``` r
solve_grid3(g11, g12, g13, g22, g23, g33, b1, b2, b3, xx)
```

## Arguments

- g11, g12, g13, g22, g23, g33:

  Gram entries of the three basis columns.

- b1, b2, b3:

  Inner products of the basis columns with the response.

- xx:

  The response sum of squares `<x, x>`.

## Value

A list with the coefficient arrays `c1`, `c2`, `c3` and the residual sum
of squares `rss`, which is `Inf` where the system is singular.
