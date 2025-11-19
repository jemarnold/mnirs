# Changelog

## mnirs 0.1.3

- Remove redundant file mnirs.test-package.Rd.
- Remove obsolete {utils} dependency.
- Remove redundant `nirs_device` arg in internal
  [`read_data_table()`](https://jemarnold.github.io/mnirs/reference/read_data_table.md).
- Implement [air](https://github.com/soumyaray/air) formatter.
- Migrate package development into Positron IDE.

## mnirs 0.1.2

- Simplify use of `width` and `span` to be centred on `idx`.
  - Updated internal helpers:
    [`compute_local_windows()`](https://jemarnold.github.io/mnirs/reference/compute_helpers.md)
    &
    [`compute_window_of_valid_neighbours()`](https://jemarnold.github.io/mnirs/reference/compute_helpers.md)
  - Updated appropriate documentation.
  - Updated README, vignette, & examples to `width = 10`.
- Updated examples with
  `@examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))`.
  - Not entirely sure this will pass CRAN. May need to update.
- Updated
  [`create_mnirs_data()`](https://jemarnold.github.io/mnirs/reference/create_mnirs_data.md)
  to correctly accept listed or unlisted metadata.

## mnirs 0.1.1

- Fix formatting error in README.Rmd.
- Remove obsolete code in `resample_mnirs.R`.
- Add missing tests for
  [`replace_mnirs()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md),
  [`resample_mnirs()`](https://jemarnold.github.io/mnirs/reference/resample_mnirs.md),
  and
  [`plot.mnirs()`](https://jemarnold.github.io/mnirs/reference/plot.mnirs.md).
- Update documentation formatting and examples.

## mnirs 0.1.0 initial release

- Initial release of reading and cleaning functions.
- Basic README demonstrating functionality.
- Website documentation available
  [here](https://jemarnold.github.io/mnirs/), including function
  reference index and vignettes.

## mnirs 0.0.0.9000

- initial commit.
- project setup.
