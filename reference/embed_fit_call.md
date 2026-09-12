# Make an nls model call self-contained

[`stats::nls()`](https://rdrr.io/r/stats/nls.html) stores its call
arguments as the expressions it was called with (`formula`, `.data`,
`start[free]`), resolvable only in the fitting frame. Evaluating them in
that frame and storing the values lets
[`stats::update()`](https://rdrr.io/r/stats/update.html),
[`stats::profile()`](https://rdrr.io/r/stats/profile.html), and
`insight::get_data()`.

## Usage

``` r
embed_fit_call(model, env = parent.frame())
```

## Arguments

- model:

  An [nls](https://rdrr.io/r/stats/nls.html) model.

- env:

  The fitting frame the call arguments resolve in; *default* the caller
  of this function.

## Value

`model` with every `call` argument replaced by its value.
