# Warn on a failed or non-converged kinetics model fit

Shared warning for the nls-based kinetics workers. Error conditions
report a failed fit with an optional hint that the reduced
(`n_params - 1`) model is attempted next; warning-class conditions
report a fit accepted despite non-convergence.

## Usage

``` r
warn_fit_failed(
  fn,
  e,
  .nirs,
  interval_name,
  n_params = NULL,
  retry = FALSE,
  env = rlang::caller_env()
)
```

## Arguments

- fn:

  Symbol or character; the model fn named in the message.

- e:

  The captured condition object.

- .nirs:

  Character; the channel name.

- interval_name:

  Character; the interval label.

- n_params:

  Integer or `NULL`; parameter count prefixed to the fn name.

- retry:

  Logical; hint that the reduced model fit is attempted next.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

`invisible(NULL)`, invoked for its warning side effect.
