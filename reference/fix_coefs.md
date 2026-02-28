# Update a model object with Fixed coefficients

Re-fit a model with fixed coefficients provided as additional arguments.
Fixed coefficients are not modified when optimising for best fit.

## Usage

``` r
fix_coefs(model, data = NULL, verbose = TRUE, ...)
```

## Arguments

- model:

  An existing model object from `lm`, `nls`, `glm`, and others.

- data:

  An *optional* data frame to supply manually if original data frame is
  unavailable from a different parent environment.

- verbose:

  Logical. Default is `TRUE`. Will display or silence (if `FALSE`)
  warnings and information messages helpful for troubleshooting. A
  global default can be set via `options(mnirs.verbose = FALSE)`.

- ...:

  Named model coefficients to fix.

## Value

An updated model object with remaining free coefficients.

## Details

If no fixed coefficients are supplied, or if a coefficient does not
exist in the model, the model will be returned unchanged (with a
warning).

The function cannot update if all model coefficients are supplied as
fixed, and will abort.
