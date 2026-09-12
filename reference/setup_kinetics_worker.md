# Shared validation prologue for `analyse_<method>()` workers

Runs the identical per-interval setup shared by every kinetics worker:
validates `data`, resolves `nirs_channels` and `time_channel`,
broadcasts global arguments across channels via
[`resolve_channel_args()`](https://jemarnold.github.io/mnirs/reference/resolve_channel_args.md),
and validates the resolved per-channel arguments via
[`validate_kinetics_args()`](https://jemarnold.github.io/mnirs/reference/validate_kinetics_args.md).

## Usage

``` r
setup_kinetics_worker(
  data,
  nirs_quo,
  time_quo,
  arg_list,
  choices = list(),
  fix_params = NULL,
  verbose = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- data:

  A single *"mnirs"* data frame.

- nirs_quo, time_quo:

  Quosures of the worker's `nirs_channels` and `time_channel` arguments
  (captured with
  [`enquo()`](https://rlang.r-lib.org/reference/enquo.html) in the
  worker frame).

- arg_list:

  Named list of the method's per-channel-capable arguments.

- choices:

  Named list of valid values for choice-type arguments, passed to
  [`resolve_channel_args()`](https://jemarnold.github.io/mnirs/reference/resolve_channel_args.md).

- fix_params:

  An *optional* character vector of fixable model parameter names, or a
  function of a channel's resolved argument list returning that vector
  (for models whose fixable parameters depend on another argument, e.g.
  `use_TD`). When supplied, each channel's resolved `fix` is validated
  via
  [`validate_fix()`](https://jemarnold.github.io/mnirs/reference/validate_fix.md).

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A named list with `nirs_channels`, `time_channel`, and `per_channel`.

## Details

`fix` is itself a named list, so it is classified before resolution: a
plain list of parameter values applies globally to every channel, while
a list whose elements are all lists is a per-channel map keyed by
channel name. The resolved `fix` is validated per channel against
`fix_params` when supplied.
