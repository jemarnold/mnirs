# Accept or reject a non-converged port fit

[`stats::nls()`](https://rdrr.io/r/stats/nls.html) with
`algorithm = "port"` and `warnOnly = TRUE` returns a model whose stop
certificate failed. It is kept with a warning when `ok` holds and its
coefficients are finite; otherwise it is reported as an error and
dropped. The port stop code is reported in prose either way.

## Usage

``` r
accept_port_fit(model, on_error, ok = TRUE)
```

## Arguments

- model:

  An [nls](https://rdrr.io/r/stats/nls.html) model or `NULL`.

- on_error:

  A reporting function; see
  [`fit_td_fallback()`](https://jemarnold.github.io/mnirs/reference/fit_td_fallback.md).

- ok:

  Logical; a further acceptance condition, e.g. an RSS no worse than the
  starting estimates. Evaluated only for a non-converged fit.

## Value

`model` or `NULL`.
