# Find the header row containing all `nirs_channels`

Find the header row containing all `nirs_channels`

## Usage

``` r
find_header_row(raw, nirs_channels, start = 1L, env = rlang::caller_env())
```

## Arguments

- raw:

  A raw character data frame from
  [`read_file()`](https://jemarnold.github.io/mnirs/reference/read_file.md).

- nirs_channels:

  Character vector of original column names.

- start:

  Integer row index to try first, from
  [`detect_mnirs_device()`](https://jemarnold.github.io/mnirs/reference/detect_mnirs_device.md).

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.
