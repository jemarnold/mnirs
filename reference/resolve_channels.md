# Resolve channels from user input, device defaults, or the Oxysoft legend

User-specified channels take priority; for Artinis, user originals given
as legend names (cleaned or raw trace) resolve to their column ids.
Otherwise `nirs` channels are read from the Oxysoft legend (Artinis) or
header cells starting with `SmO2`; `time` falls back to the device
default, and is detected later by
[`detect_time_channel()`](https://jemarnold.github.io/mnirs/reference/detect_time_channel.md)
when still `NULL`; `event` falls back to the device default when present
in the header row. Device companion columns (`extra`, `labels`) are
returned only with `keep_all = TRUE`.

## Usage

``` r
resolve_channels(
  raw,
  device,
  user,
  keep_all = FALSE,
  verbose = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- raw:

  A raw character data frame from
  [`read_file()`](https://jemarnold.github.io/mnirs/reference/read_file.md).

- device:

  Output of
  [`detect_mnirs_device()`](https://jemarnold.github.io/mnirs/reference/detect_mnirs_device.md).

- user:

  A list of user-specified `time`, `event`, and `nirs` channels, each a
  named character vector `c(new = "original")` or `NULL`.

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A list of `time`, `extra`, `event`, `labels`, and `nirs` channel
mappings, each a named `c(new = "original")` vector or `NULL`. List
order sets output column order.
