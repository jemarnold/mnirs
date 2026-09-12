# Enforce the requested direction on a converged parametric fit

Direction is the sign of the primary amplitude `D = B - A` (`B - A` for
the biexponential), where the primary asymptote follows `A` in model
parameter order. A fit is satisfied when `D` and every free parameter
lie inside the refit box (`D` sign-constrained; `lower`/ `upper` for the
rest, e.g. a sigmoid slope sign floor) and is returned unchanged.
Otherwise the model is refit on `D` via `nls(algorithm = "port")` with
`D` sign-bounded and its magnitude floored strictly above zero (sigmoid
models divide by `D`), then re-expressed in the original
parameterisation from that optimum so the returned model reports
consistent coefficient names. A refit that fails, pins a sign-floored
coefficient (degenerate flat fit), or loses the requested sign on
re-expression warns and returns `NULL`. Parameters in `fix` are held
constant: a fixed `A` or `B` is substituted into the amplitude
reparameterisation; with both fixed the amplitude sign is predetermined
and no refit is possible.

## Usage

``` r
enforce_direction(
  model,
  coefs,
  fit_data,
  direction,
  amp_fn,
  fn = sub("^(SS)?", "SS", as.character(amp_fn)),
  lower = NULL,
  upper = NULL,
  floor_params = NULL,
  fix = list(),
  control = NULL,
  .nirs,
  interval_name,
  env = rlang::caller_env()
)
```

## Arguments

- model:

  A converged [nls](https://rdrr.io/r/stats/nls.html) model object.

- coefs:

  Named numeric coefficient vector in model parameter order with fixed
  values merged in (see
  [`full_coefs()`](https://jemarnold.github.io/mnirs/reference/full_coefs.md)).

- fit_data:

  Data frame with the response in the first column and time in the
  second; the refit formula is built on those names.

- direction:

  Character; resolved `"positive"` or `"negative"`.

- amp_fn:

  Symbol; model fn taking `t` and the model parameters as named
  arguments. A self-start fn returning a `"gradient"` attribute (free
  symbols, asymptotes first) makes both refits analytic; a plain fn
  falls through to
  [`stats::numericDeriv()`](https://rdrr.io/r/stats/numericDeriv.html).

- fn:

  Character; the self-start fn named in the warning (*default*
  `SS<amp_fn>`, or `amp_fn` itself when already prefixed).

- lower, upper:

  Named numeric bounds for free parameters other than the asymptotes.
  Sign-floor bounds should be data-scaled small values (not
  `.Machine$double.eps`) so pinned-floor degeneracy is detectable.

- floor_params:

  Character; names of refit coefficients subject to the pinned-floor
  degeneracy check. `NULL` (*default*) checks every finite bound;
  restrict when other bounds are structural (e.g. the biexponential
  time-constant bounds).

- fix:

  Named list of user-fixed parameter values.

- control:

  User
  [`stats::nls.control()`](https://rdrr.io/r/stats/nls.control.html)
  list merged over the refit defaults by
  [`fit_control()`](https://jemarnold.github.io/mnirs/reference/fit_control.md).

- .nirs:

  Character; the channel name.

- interval_name:

  Character; the interval label.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A named list `list(model, coefs)` with `coefs` a named numeric vector in
`(A, B, ...)` space including fixed values, or `NULL` when the direction
cannot be satisfied (caller returns
[`build_na_results()`](https://jemarnold.github.io/mnirs/reference/build_na_results.md)).
