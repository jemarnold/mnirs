# Grid-profiled starting estimates for the biexponential model

Vector-level initialiser behind
[`biexp_init()`](https://jemarnold.github.io/mnirs/reference/biexp_init.md),
called directly by the kinetics worker with `tau` and `TD` held at their
stage-1 values to seed the slow phase. Profiles the time constants (and
`TD`) on a coarse grid and keeps the RSS-minimising start (cf.
[`expdrift_start()`](https://jemarnold.github.io/mnirs/reference/expdrift_start.md)).
The model is linear in `A`, `B`, and `B2` once `tau`, `tau2`, and `TD`
are held, so those are solved by least squares at every grid point at
once: the Gram entries of the bases `e1`, `e2 - e1`, `1 - e2` for every
`(tau, tau2)` pair follow from the column products of the two
exponential matrices, and
[`solve_grid3()`](https://jemarnold.github.io/mnirs/reference/solve_grid3.md)
solves the pairs in one pass. User-fixed values narrow the grids; the
amplitudes are always solved free, as this is only a seed. Pairs with
`tau / tau2 > 0.98` are dropped as their bases are near-collinear,
unless both time constants are fixed.

## Usage

``` r
biexp_start(x, t, fixed = list(), has_TD = FALSE)
```

## Arguments

- x, t:

  Numeric vectors of the response and time.

- fixed:

  A named list of user-fixed parameter values, which narrow the grids
  and constrain the free estimates.

- has_TD:

  Logical; include the time delay `TD`.

## Value

A named numeric vector of starting estimates in model order.
