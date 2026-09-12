# Run a kinetics worker over each interval and collate results

Shared skeleton for `analyse_kinetics.*` methods: normalises `data` to a
named list of interval data frames, splits sample groups via
[`split_kinetics_groups()`](https://jemarnold.github.io/mnirs/reference/split_kinetics_groups.md),
calls the method worker in `kinetics_workers` once per interval, and
collates results via
[`build_kinetics_results()`](https://jemarnold.github.io/mnirs/reference/build_kinetics_results.md).

## Usage

``` r
analyse_kinetics_intervals(
  data,
  method,
  worker_args,
  nirs_quo,
  time_quo,
  group_intervals,
  zero_time,
  verbose,
  call,
  env,
  fallback = TRUE
)
```

## Arguments

- data:

  A data frame, list of data frames, or grouped data frame.

- method:

  Character; the canonical method name.

- worker_args:

  Named list of method-specific arguments passed to the method's worker
  in `kinetics_workers`.

- nirs_quo, time_quo:

  Quosures of the caller's `nirs_channels` and `time_channel` arguments,
  captured in the method frame.

- group_intervals:

  `"ensemble"` or a [`list()`](https://rdrr.io/r/base/list.html) of
  sample index vectors; see
  [`split_kinetics_groups()`](https://jemarnold.github.io/mnirs/reference/split_kinetics_groups.md).

- zero_time:

  Logical; if `TRUE`, rebases each interval's `time_channel` to start
  from zero, shifting `interval_times` metadata by the same offset.

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- call:

  The matched call from the user-facing method.

- env:

  The call recorded for condition reporting.

- fallback:

  Logical; resolve the method's fallback chain in `kinetics_fallbacks`
  per channel (see
  [`analyse_kinetics_channels()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics_channels.md)).

## Value

An *"mnirs_kinetics"* object from
[`build_kinetics_results()`](https://jemarnold.github.io/mnirs/reference/build_kinetics_results.md).
