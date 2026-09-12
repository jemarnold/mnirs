# Grid-profiled starting estimates for the exponential-drift model

Vector-level initialiser behind
[`expdrift_init()`](https://jemarnold.github.io/mnirs/reference/expdrift_init.md),
called directly by the kinetics worker on the fit window. Profiles `tau`
(and `TD`) on a coarse grid and keeps the RSS-minimising start (cf.
[`monoexp_start()`](https://jemarnold.github.io/mnirs/reference/monoexp_start.md)).
The model is linear in `A`, `B`, and `slope_B` once `tau` and `TD` are
held, so those are solved by least squares at every grid point at once
via
[`solve_grid3()`](https://jemarnold.github.io/mnirs/reference/solve_grid3.md).
User-fixed `tau`, `TD`, and `drift_fraction` narrow the grids; the
linear parameters are always solved free, as this is only a seed. `tau`
is capped so the drift onset stays inside the record; a grid point whose
hinge has no support is singular and skipped.

## Usage

``` r
expdrift_start(x, t, fixed = list(), has_TD = FALSE)
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
