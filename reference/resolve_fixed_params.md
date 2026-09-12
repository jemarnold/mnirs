# Resolve fixed parameters from a self-start model call

Classifies each self-start model parameter in a matched call as free
(written as its own bare symbol) or fixed (written as any other value,
e.g. `A = 0`). Fixed expressions are evaluated for use as initialisation
seeds; values that cannot be resolved to a finite numeric scalar return
`NULL` and seeds fall back to data-driven estimates.

## Usage

``` r
resolve_fixed_params(mCall, params, data)
```

## Arguments

- mCall:

  A matched call to the `selfStart` model.

- params:

  Character vector of the model parameter names.

- data:

  A data frame with the model variables.

## Value

A named list of fixed parameter values, empty when no parameters are
fixed.
