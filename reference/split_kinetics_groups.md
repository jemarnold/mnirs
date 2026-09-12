# Split interval data frames into sample groups

Applies the `group_intervals` argument of
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
to a named list of data frames. `"ensemble"` returns `data_list`
unchanged; a [`list()`](https://rdrr.io/r/base/list.html) of sample
(row) indices subsets every data frame into one interval per group.
Samples in no group are dropped and samples in several groups are warned
about. Group names become interval names (`interval_<n>` when unnamed),
suffixed `<group>_<df>` when `data_list` holds more than one data frame.
Row-subset intervals no longer correspond to their
`interval_times`/`interval_span` metadata, so those attributes are
dropped.

## Usage

``` r
split_kinetics_groups(
  data_list,
  group_intervals,
  verbose = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- data_list:

  Named list of data frames from
  [`as_data_list()`](https://jemarnold.github.io/mnirs/reference/as_data_list.md).

- group_intervals:

  `"ensemble"` or a [`list()`](https://rdrr.io/r/base/list.html) of
  integer-valued sample index vectors; see
  [`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md).

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A named list of data frames, one per group per data frame.
