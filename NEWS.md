# mnirs 0.4.0

* Create `monoexponential()`, `SS_monoexp3()`, and `SS_monoexp4()` self-starting model functions.
    * `monoexponential()` is the equation for a 4-parameter monoexponential function with parametera "A", "B", "tau", and "TD".
    * `SS_monoexp*()` are self-starting functions for `nls()` or other curve fitting functions, for either the 4-parameter monoexponential function, or a reduced 3-parameter function without a time delay ("TD").
    * `fix_coef()` is used to update a model with any number of parameters set to fixed values, e.g. if the starting or ending values are known a priori. By entering fixed values, the remaining parameters are left free to be optimised (experimental function, may not work when called from within nested function conditions, need to further validate).  
* Add `vo2master.csv` example file recorded with VO2 Master Manager app (thanks Philip Skotzke).
* Update `read_mnirs()` to correctly convert "," decimal values to numeric (thanks Philip Skotzke).
* Simplify `data.table::fread()` for .csv files with multiple regional formats.
* `replace_invalid()` and `replace_outliers()` now inform the number of samples replaced (thanks Philip Skotzke).

# mnirs 0.3.0

* Create `peak_slope()`. `slope()` and `rolling_slope()` become internal.
* Remove `na.rm` arg from `slope()`, `rolling_slope()`, `peak_slope()` in favour of opinionated `na.rm = TRUE` behaviour.
    * `partial = FALSE` effectively covers `na.rm = FALSE` where fewer than `width` number valid samples exist in local vector.
    * `na.rm` arg currently remains in `filter_mnirs()` functions, but this may be removed in the future.
* Fix `read_mnirs()` internally `read_file()` to better handle .csv files.
    * Implement `data.table::fread()` to handle .csv with empty rows and long string metadata in the header above the data table.
    * Awaiting future expanded `{data.table}` implementation across `{mnirs}`, i.e. for `data.table::froll*` functions.
* Fix `filter_moving_average()` to correctly identify windows where `partial = FALSE` was not met due to `NA`s.
* Remove internal `{roll}` dependency.
* Update documentation in expectation of CRAN submission.
    * Remove internal function examples to minimise `\donttest()` issues.

# mnirs 0.2.0

* Implement tidy evaluation with `{rlang}` and `{tidyselect}`.
* Update function documentation & examples, `README`, vignette.
* Fix `filter_moving_average()` and `rolling_mean()` to correctly use `partial` and `na.rm` arguments.
* Fix `shift_mnirs()` to correctly use `width` and `span` arguments.

# mnirs 0.1.9

* Create `rolling_slope()` to calculate local linear regression slopes along a vector.
* Integrate [roll][roll::roll-package] for faster rolling median, mean, and lm implementation under certain conditions.
    * Currently used: `roll_lm()` for `rolling_slope()`, `roll_median()` for `replace_outliers()`, `roll_mean()` for `filter_moving_average()`.
    * May roll back in favour of `data.table::froll_*` when 1.17.99+ is updated on CRAN.
* Update local rolling functions with `align = c("center", "left", "right")` arg.
    * Currently used: internal `compute_local_windows()` for `rolling_slope()`.
* Update internal `compute_local_fun()` to allow additional arguments, e.g. for `na.rm = TRUE`.
* Remove redundant internal numeric rounding. Results may now have floating point precision issues, but avoid compounding rounding error.
    * Affects: `filter_mnirs.smooth_spline()`, `filter_mnirs.moving_average()`, `parse_sample_rate()` and `clean_invalid()` for `read_mnirs()`, `replace_mnirs()`, `resample_mnirs()`, `shift_mnirs()`, `validate_sample_rate()`.
* Add hidden `bypass_check` argument to reduce overhead from validation redundancy.
    * Affects: `replace_invalid()`, `replace_outliers()`, `replace_missing()`, `filter_moving_average()`, new functions `slope()`, `rolling_slope()`.
* Update `validate_numeric()` logic to reduce overhead.

# mnirs 0.1.8

* Create `extract_intervals()` to detect events and extract surrounding data frames.
* Replace internal `between()` with `within()` to differentiate from `dplyr::between()`.
* Update `README` with image of `{mnirs}` shiny app.
* Update `filter_mnirs()` with error message when `fc` outside 0 Hz and Nyquist frequency.

# mnirs 0.1.7

* Minor edits to functions and documentation to improve clarity.
* Changes to user-facing functions:
    * `filter_mnirs()` and `filter_butter()` arg from `n` to `order`.
    * `palette_mnirs()` arg from `n` as either numeric or character, to numeric, with `names` as the character arg.
    * `replace_mnirs()`, `replace_outleirs()`, `replace_missing()`, and `resample_mnirs()` arg from `method = "NA"` to `method = "none"`.
* Revert argument `inform` to `verbose`.
* Standardise `cli_inform()`, `cli_warn()`, and `cli_abort()` messages.
* Remove redundant `resample_mnirs()` argument `resample_time`.

# mnirs 0.1.6

* Create internal numeric vector helper functions
    * `signif_whole()` applies sig-fig rounding to decimal places, or whole values.
    * `signif_trailing()` applies decimal or sig-fig rounding and convers to character strings with trailing zeroes, for display.
    * `signif_pvalue()` applies formatting for p-value display as character strings or significance symbols.
    * `seq_range()` creates a numeric sequence spanning the range of an ipnut vector.
    * `wrap()` rotates vector elements from head to tail (or tail to head) by position.
* Add statement about use of generative AI codebots to README.Rmd

# mnirs 0.1.5

* Remove redundant validation checks in `replace_mnirs()`.
* Fix erroneous url in _pkgdown.yml.
* Update internal `validate_event_channel` to error on empty numeric or character column.
* Update test coverage:
    * Cover new `options(mnirs.inform = FALSE)`.
    * Cover unlisted metadata added to `create_mnirs_data()`.
    * Cover hidden `plot.mnirs()` options.

# mnirs 0.1.4

* Update argument `inform` replaces `verbose` to display/hide messages & warnings.
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
