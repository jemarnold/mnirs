# Resolve per-interval arguments

Peels the interval layer from `worker_args` before per-interval worker
dispatch, mirroring the per-channel convention of
[`resolve_channel_args()`](https://jemarnold.github.io/mnirs/reference/resolve_channel_args.md).
An argument is treated as an interval map when it is a
[`list()`](https://rdrr.io/r/base/list.html) with at least one named key
matching an interval name, no key matching a channel name (channel maps
keep their existing per-channel meaning), and at most one unnamed
element acting as the fallback for unlisted intervals. `fix` must
additionally be a list of lists, so a plain parameter list (e.g.
`fix = list(A = 0)`) stays global.

## Usage

``` r
resolve_interval_args(
  worker_args,
  interval_names,
  chan_names,
  verbose = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- worker_args:

  Named list of method-specific arguments.

- interval_names:

  Character vector of interval names from
  [`as_data_list()`](https://jemarnold.github.io/mnirs/reference/as_data_list.md).

- chan_names:

  Character vector of resolved channel names, used only to give channel
  keys precedence over interval keys.

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A named list with one element per interval; each element is the
`worker_args` list resolved for that interval.

## Details

Resolved values may themselves be per-channel maps, which pass untouched
to
[`resolve_channel_args()`](https://jemarnold.github.io/mnirs/reference/resolve_channel_args.md)
downstream. Intervals omitted from a map with no unnamed fallback
resolve to `NULL`, falling through to the argument's default, matching
omitted-channel behaviour.
