# Warn about unmatched keys in an argument map

Shared by
[`resolve_channel_args()`](https://jemarnold.github.io/mnirs/reference/resolve_channel_args.md)
and
[`resolve_interval_args()`](https://jemarnold.github.io/mnirs/reference/resolve_interval_args.md):
unrecognised keys are warned about and ignored; omitted keys (only
reported by callers when the map has no unnamed fallback) fall back to
the argument's default.

## Usage

``` r
warn_map_keys(
  arg_nm,
  unknown,
  omitted,
  what,
  match_hint,
  env = rlang::caller_env()
)
```

## Arguments

- arg_nm:

  Character; the argument name.

- unknown, omitted:

  Character vectors (or `NULL`) of unrecognised and unspecified keys.

- what:

  Character; the key kind, `"channel"` or `"interval"`.

- match_hint:

  Character; what valid keys must match, may contain cli markup.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

`invisible(NULL)`, invoked for its warning side effects.
