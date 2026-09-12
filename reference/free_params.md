# Free parameters of a self-start model call

A parameter written as a bare symbol in the model call is free (fitted
by [`stats::nls()`](https://rdrr.io/r/stats/nls.html)); one written as a
constant or expression is fixed. Used by the model functions to return
gradient columns for the free parameters only, in call order, as
[`stats::nls()`](https://rdrr.io/r/stats/nls.html) indexes the
`"gradient"` attribute by position.

## Usage

``` r
free_params(mCall, params)
```

## Arguments

- mCall:

  A matched call to the model function.

- params:

  Character vector of the model parameter names.

## Value

A character vector; the subset of `params` that are free.
