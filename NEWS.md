# mnirs 0.1.4

* Breaking change: Update argument `inform` replaces `verbose` to display/hide messages & warnings.
* Implement global option to set `inform = FALSE` with `options(mnirs.inform = FALSE)`.
    * Global option `mnirs.inform = FALSE` will override functions' implicit default `inform = TRUE`.
    * Explicit call `inform = TRUE` will override the global option.
* Update `replace_mnirs()` and `replace_invalid()` with additional arguments.
    * Replace ranges above or below `invalid_above` and `invalid_below`, respectively.

# mnirs 0.1.3

* Remove redundant file mnirs.test-package.Rd.
* Remove obsolete {utils} dependency.
* Remove redundant `nirs_device` arg in internal `read_data_table()`.
* Implement `{air}` formatter.
* Migrate package development into Positron IDE.

# mnirs 0.1.2

* Simplify use of `width` and `span` to be centred on `idx`.
    * Update internal helpers: `compute_local_windows()` & `compute_window_of_valid_neighbours()`
    * Update appropriate documentation.
    * Update README, vignette, & examples to `width = 10`.
* Update examples with `@examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))`.
    * Not entirely sure this will pass CRAN. May need to update.
* Update `create_mnirs_data()` to correctly accept listed or unlisted metadata.

# mnirs 0.1.1

* Fix formatting error in README.Rmd.
* Remove obsolete code in `resample_mnirs.R`.
* Add missing tests for `replace_mnirs()`, `resample_mnirs()`, and `plot.mnirs()`.
* Update documentation formatting and examples.

# mnirs 0.1.0 initial release

* Initial release of reading and cleaning functions.
* Basic README demonstrating functionality.
* Website documentation available [here](https://jemarnold.github.io/mnirs/), including function reference index and vignettes.

# mnirs 0.0.0.9000

* initial commit.
* project setup.
