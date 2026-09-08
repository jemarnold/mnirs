# Select, rename, and order channel columns

Original names are made unique to match
`rename_duplicates(names(data))`; duplicated new names are made unique
with a warning. Channel names take priority over clashing names of other
data columns. Columns are ordered by role, followed by all remaining
columns when `keep_all = TRUE`.

## Usage

``` r
select_channels(
  data,
  channels,
  keep_all = FALSE,
  verbose = TRUE,
  env = rlang::caller_env()
)
```

## Arguments

- data:

  The named character data table.

- channels:

  A list of named `c(new = "original")` channel mappings by role from
  [`resolve_channels()`](https://jemarnold.github.io/mnirs/reference/resolve_channels.md);
  `NULL` roles are dropped.

- verbose:

  Logical. `TRUE` (*default*) will display, and `FALSE` will silence
  warnings and information messages helpful for troubleshooting. Global
  default can be set via `options(mnirs.verbose = FALSE)`.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A list of the selected `data` and `channels` as new names by role.
