# Fit a self-start model with time-delay fallback

Shared attempt skeleton for the nls-based kinetics workers. The TD model
is flat at `A` before `TD`, so the pre-onset baseline anchors `A`; the
reduced model has no such region and diverges at `t < 0`, so it is fit
from `start_time` onward. An under-determined attempt is rejected before
it reaches `fitter`, and a failed TD fit falls back to the reduced model
without `TD` unless `TD` is user-fixed. Every failure is reported
through
[`warn_fit_failed()`](https://jemarnold.github.io/mnirs/reference/warn_fit_failed.md).

## Usage

``` r
fit_td_fallback(x_fit, t_fit, params, .a, fitter, fn, ctx)
```

## Arguments

- x_fit, t_fit:

  Numeric vectors of the channel fit window.

- params:

  Character vector of parameter names in model order, including `TD`
  when the channel fits the TD model.

- .a:

  The channel's resolved argument list (`use_TD`, `fix`).

- fitter:

  A function `(.data, .params, on_error)` fitting `.params` to a data
  frame with the response and time columns named per
  [`fit_names()`](https://jemarnold.github.io/mnirs/reference/fit_names.md)
  and returning an [nls](https://rdrr.io/r/stats/nls.html) model or
  `NULL`. `on_error(e)` reports the condition `e` and returns `NULL`, so
  it doubles as a [`tryCatch()`](https://rdrr.io/r/base/conditions.html)
  error handler.

- fn:

  Symbol; the self-start fn named in the warning.

- ctx:

  The channel context list of
  [`analyse_kinetics_channels()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics_channels.md).

## Value

A list with `model` (or `NULL`), the `params` actually fit, the logical
row filter `keep`, and the fit `data` frame.
