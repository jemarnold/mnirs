# Grid-profiled starting estimates for the monoexponential model

Vector-level initialiser behind
[`monoexp_init()`](https://jemarnold.github.io/mnirs/reference/monoexp_init.md),
called directly by the kinetics worker on the fit window. Profiles `tau`
(and `TD` for the 4-parameter model) on a coarse grid and keeps the
RSS-minimising start (cf.
[`biexp_start()`](https://jemarnold.github.io/mnirs/reference/biexp_start.md)).
The model is linear in `A` and `B` once `tau` and `TD` are held, so the
asymptotes are solved by least squares at every grid point at once.
Point estimates from derivative changepoints or log-linearisation are
too sensitive to noise, overshoot, and plateau data on real NIRS
signals, and can strand nls with a singular gradient.

## Usage

``` r
monoexp_start(x, t, fixed = list(), has_TD = FALSE)
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
