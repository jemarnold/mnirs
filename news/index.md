# Changelog

## mnirs 0.5.2

#### Shiny app

- Update online shiny app hosted at
  <https://jemarnold-mnirs-app.share.connect.posit.cloud/> with basic
  reading and pre-processing functionality.

## mnirs 0.5.1

#### Updated core functions

- [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md)
  now has better automated channel detection logic for *“Artinis” /
  “Oxysoft”* file formats. `nirs_channels` and `time_channel` can be
  left blank to automatically *“sample”*, *“time”* (from `sample_rate`),
  and nirs channel *“2”*.

- [`replace_outliers()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  now accepts numeric `outlier_cutoff` values for more precise outlier
  detection thresholds (previously was integers only). Documentation
  also updated.

- [`filter_mnirs.moving_average()`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md)
  now accepts `partial` argument passed to
  [`filter_ma()`](https://jemarnold.github.io/mnirs/reference/filter_ma.md),
  which permits calculation with fewer than the specified number of
  valid samples in the local window, allowing calculation when missing
  values (`NA`) are present.

#### Updated internal functions

- [`signif_trailing()`](https://jemarnold.github.io/mnirs/reference/signif_trailing.md)
  simplified with `format = c("digits", "signif")` printing the lesser
  of either `digits` or the max decimal/sigfigs in the data, by default
  when new arg: `trim = TRUE`. Otherwise `trim = FALSE` will print to
  the exact number of `digits`.

## mnirs 0.5.0

#### Updated core functions

- [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md)
  expands `event_channel` to work with integer *“lap”* numbers, or
  character event label as previous.

  - Should now work on more .csv file formats; previously read errors
    may have occured where the file contained header rows above the data
    table, resulting in improper detection of columns.
  - `event_channel` can now be specified as an integer `lap` column, in
    addition to a character column as previous.
  - Other {mnirs} functions may expect `event_channel` to be either
    character or integer-ish.

- [`extract_intervals()`](https://jemarnold.github.io/mnirs/reference/extract_intervals.md)
  ***breaking change*** to argument specification:

  - Function arguments `start` and `end` replace `event_times`,
    `event_labels`, and `event_samples`. Allowing for more flexible and
    more clear interval boundary specifications.
  - Helper functions
    [`by_time()`](https://jemarnold.github.io/mnirs/reference/by_time.md),
    [`by_label()`](https://jemarnold.github.io/mnirs/reference/by_time.md),
    [`by_lap()`](https://jemarnold.github.io/mnirs/reference/by_time.md),
    and
    [`by_sample()`](https://jemarnold.github.io/mnirs/reference/by_time.md)
    added to specify `start`/`end` values.
  - Fix edge-cases where metadata were not returned as expected.

- [`resample_mnirs()`](https://jemarnold.github.io/mnirs/reference/resample_mnirs.md)
  now defaults to `method = "locf"` which is safer for more column types
  where interpolation may not be appropriate (integer, discrete numeric,
  character, factors, etc).

- [`filter_ma()`](https://jemarnold.github.io/mnirs/reference/filter_ma.md)
  has been renamed from
  [`filter_moving_average()`](https://jemarnold.github.io/mnirs/reference/filter_ma.md).
  The latter is kept as an alias of the former.

- [`peak_slope()`](https://jemarnold.github.io/mnirs/reference/peak_slope.md)
  now returns a *“fitted”* vector with the linear regression predicted
  values within the peak slope window.

#### Updated articles

- README:
  - Add basic use covering
    [`extract_intervals()`](https://jemarnold.github.io/mnirs/reference/extract_intervals.md).
  - Add rough draft *{mnirs}* hex icon.
- *“Reading and Cleaning Data with {mnirs}”* vignette:
  - Add section: *“Detect and extract intervals”* covering
    [`extract_intervals()`](https://jemarnold.github.io/mnirs/reference/extract_intervals.md)
    core functionality.
- Rough draft at creating an {mnirs} cheatsheet.

#### Documentation

- Many function help documents re-written to be more readible.
- Updated function examples with clearer conditions for CRAN submission.

#### Example files

- Update `train.red_intervals.csv` revert to include original onboard
  smoothed and unfiltered NIRS channels.
- Remove `vo2master.csv` from example data, as it is only used for
  internal testing.

## mnirs 0.4.2

- Update
  [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md)
  - Fix internal
    [`read_file()`](https://jemarnold.github.io/mnirs/reference/read_file.md)
    to better recognise .csv formats where the header has fewer columns
    than the data table.
  - If `nirs_channels` is left blank (now default),
    [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md)
    will attempt to recognise the NIRS device file format by searching
    for known data table column headers, and return the entire data
    table as if `keep_all = TRUE`, as a data exploration option.
  - Update `add_timestamp` to recognise date-time (POSIXct) values in
    the file header or `time_channel` and add `timestamp` column, and
    metadata `attr(df, "start_timestamp")`.
- Update
  [`plot.mnirs()`](https://jemarnold.github.io/mnirs/reference/plot.mnirs.md)
  - Rename argument to `time_labels` from `label_time` for more
    consistent naming convention.
  - Better label arguments `time_labels`, `n.breaks`, `na.omit`.
  - Remove `tidyr` dependency.
- Rename
  [`extract_intervals()`](https://jemarnold.github.io/mnirs/reference/extract_intervals.md)
  argument to `event_groups` from `group_events` for more consistent
  naming convention.
- Fix bug with `replace_mnirs(method = "linear")` unnecessarily calling
  for `width` or `span` to be specified.
- Update
  [`signif_trailing()`](https://jemarnold.github.io/mnirs/reference/signif_trailing.md)
  to avoid overprinting digits. Used internally for display.
  - `format = c("max_digits", "max_signif")` will print the lesser of
    either `digits`, or the maximum decimals/sigfigs in the data.
- Update function documentation.
- Update `README.Rmd` for updated function args.
- Update `reading-mnirs-data.qmd` vignette for updated function calls
  and some editing.
- Update `oxcap-analysis.qmd` with small edits.
- Remove [tidyr](https://tidyr.tidyverse.org) dependency from
  [`plot.mnirs()`](https://jemarnold.github.io/mnirs/reference/plot.mnirs.md),
  and from [mnirs](https://jemarnold.github.io/mnirs/) package
  dependencies.

## mnirs 0.4.1

- Create article “Analysing muscle oxidative capacity with {mnirs}”.
- Add `portamon-oxcap.xlsx` example file (thanks to Dr. Thomas Tripp and
  Dr. Martin MacInnis).
- Update `train.red_intervals.csv`, delete redundant NIRS channels to
  reduce file size.
- Minors updates to `monoexponential` family of functions.
  - Fix
    [`SS_monoexp3()`](https://jemarnold.github.io/mnirs/reference/SS_monoexp.md)
    to not internally look for `TD` parameter.
  - Remove
    [`monoexp_init()`](https://jemarnold.github.io/mnirs/reference/monoexp_init.md)
    from exported to internal.

## mnirs 0.4.0

- Create
  [`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md),
  [`SS_monoexp3()`](https://jemarnold.github.io/mnirs/reference/SS_monoexp.md),
  and
  [`SS_monoexp4()`](https://jemarnold.github.io/mnirs/reference/SS_monoexp.md)
  self-starting model functions.
  - [`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md)
    is the equation for a 4-parameter monoexponential function with
    parametera “A”, “B”, “tau”, and “TD”.
  - `SS_monoexp*()` are self-starting functions for
    [`nls()`](https://rdrr.io/r/stats/nls.html) or other curve fitting
    functions, for either the 4-parameter monoexponential function, or a
    reduced 3-parameter function without a time delay (“TD”).
  - `fix_coef()` is used to update a model with any number of parameters
    set to fixed values, e.g. if the starting or ending values are known
    a priori. By entering fixed values, the remaining parameters are
    left free to be optimised (experimental function, may not work when
    called from within nested function conditions, need to further
    validate).  
- Add `vo2master.csv` example file recorded with VO2 Master Manager app
  (thanks Philip Skotzke).
- Update
  [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md)
  to correctly convert “,” decimal values to numeric (thanks Philip
  Skotzke).
- Simplify
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
  for .csv files with multiple regional formats.
- [`replace_invalid()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  and
  [`replace_outliers()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  now inform the number of samples replaced (thanks Philip Skotzke).

## mnirs 0.3.0

- Create
  [`peak_slope()`](https://jemarnold.github.io/mnirs/reference/peak_slope.md).
  [`slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md)
  and
  [`rolling_slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md)
  become internal.
- Remove `na.rm` arg from
  [`slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md),
  [`rolling_slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md),
  [`peak_slope()`](https://jemarnold.github.io/mnirs/reference/peak_slope.md)
  in favour of opinionated `na.rm = TRUE` behaviour.
  - `partial = FALSE` effectively covers `na.rm = FALSE` where fewer
    than `width` number valid samples exist in local vector.
  - `na.rm` arg currently remains in
    [`filter_mnirs()`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md)
    functions, but this may be removed in the future.
- Fix
  [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md)
  internally
  [`read_file()`](https://jemarnold.github.io/mnirs/reference/read_file.md)
  to better handle .csv files.
  - Implement
    [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
    to handle .csv with empty rows and long string metadata in the
    header above the data table.
  - Awaiting future expanded [data.table](https://r-datatable.com)
    implementation across [mnirs](https://jemarnold.github.io/mnirs/),
    i.e. for `data.table::froll*` functions.
- Fix
  [`filter_moving_average()`](https://jemarnold.github.io/mnirs/reference/filter_ma.md)
  to correctly identify windows where `partial = FALSE` was not met due
  to `NA`s.
- Remove internal [roll](https://github.com/jasonjfoster/roll)
  dependency.
- Update documentation in expectation of CRAN submission.
  - Remove internal function examples to minimise `\donttest()` issues.

## mnirs 0.2.0

- Implement tidy evaluation with [rlang](https://rlang.r-lib.org) and
  [tidyselect](https://tidyselect.r-lib.org).
- Update function documentation & examples, `README`, vignette.
- Fix
  [`filter_moving_average()`](https://jemarnold.github.io/mnirs/reference/filter_ma.md)
  and `rolling_mean()` to correctly use `partial` and `na.rm` arguments.
- Fix
  [`shift_mnirs()`](https://jemarnold.github.io/mnirs/reference/shift_mnirs.md)
  to correctly use `width` and `span` arguments.

## mnirs 0.1.9

- Create
  [`rolling_slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md)
  to calculate local linear regression slopes along a vector.
- Integrate \[roll\]\[roll::roll-package\] for faster rolling median,
  mean, and lm implementation under certain conditions.
  - Currently used: `roll_lm()` for
    [`rolling_slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md),
    `roll_median()` for
    [`replace_outliers()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md),
    `roll_mean()` for
    [`filter_moving_average()`](https://jemarnold.github.io/mnirs/reference/filter_ma.md).
  - May roll back in favour of `data.table::froll_*` when 1.17.99+ is
    updated on CRAN.
- Update local rolling functions with
  `align = c("center", "left", "right")` arg.
  - Currently used: internal
    [`compute_local_windows()`](https://jemarnold.github.io/mnirs/reference/compute_helpers.md)
    for
    [`rolling_slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md).
- Update internal
  [`compute_local_fun()`](https://jemarnold.github.io/mnirs/reference/compute_helpers.md)
  to allow additional arguments, e.g. for `na.rm = TRUE`.
- Remove redundant internal numeric rounding. Results may now have
  floating point precision issues, but avoid compounding rounding error.
  - Affects:
    [`filter_mnirs.smooth_spline()`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md),
    [`filter_mnirs.moving_average()`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md),
    [`parse_sample_rate()`](https://jemarnold.github.io/mnirs/reference/parse_sample_rate.md)
    and `clean_invalid()` for
    [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md),
    [`replace_mnirs()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md),
    [`resample_mnirs()`](https://jemarnold.github.io/mnirs/reference/resample_mnirs.md),
    [`shift_mnirs()`](https://jemarnold.github.io/mnirs/reference/shift_mnirs.md),
    [`validate_sample_rate()`](https://jemarnold.github.io/mnirs/reference/validate_mnirs.md).
- Add hidden `bypass_check` argument to reduce overhead from validation
  redundancy.
  - Affects:
    [`replace_invalid()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md),
    [`replace_outliers()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md),
    [`replace_missing()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md),
    [`filter_moving_average()`](https://jemarnold.github.io/mnirs/reference/filter_ma.md),
    new functions
    [`slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md),
    [`rolling_slope()`](https://jemarnold.github.io/mnirs/reference/rolling_slope.md).
- Update
  [`validate_numeric()`](https://jemarnold.github.io/mnirs/reference/validate_mnirs.md)
  logic to reduce overhead.

## mnirs 0.1.8

- Create
  [`extract_intervals()`](https://jemarnold.github.io/mnirs/reference/extract_intervals.md)
  to detect events and extract surrounding data frames.
- Replace internal
  [`between()`](https://dplyr.tidyverse.org/reference/between.html) with
  [`within()`](https://jemarnold.github.io/mnirs/reference/within.md) to
  differentiate from
  [`dplyr::between()`](https://dplyr.tidyverse.org/reference/between.html).
- Update `README` with image of
  [mnirs](https://jemarnold.github.io/mnirs/) shiny app.
- Update
  [`filter_mnirs()`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md)
  with error message when `fc` outside 0 Hz and Nyquist frequency.

## mnirs 0.1.7

- Minor edits to functions and documentation to improve clarity.
- Changes to user-facing functions:
  - [`filter_mnirs()`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md)
    and
    [`filter_butter()`](https://jemarnold.github.io/mnirs/reference/filter_butter.md)
    arg from `n` to `order`.
  - [`palette_mnirs()`](https://jemarnold.github.io/mnirs/reference/palette_mnirs.md)
    arg from `n` as either numeric or character, to numeric, with
    `names` as the character arg.
  - [`replace_mnirs()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md),
    `replace_outleirs()`,
    [`replace_missing()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md),
    and
    [`resample_mnirs()`](https://jemarnold.github.io/mnirs/reference/resample_mnirs.md)
    arg from `method = "NA"` to `method = "none"`.
- Revert argument `inform` to `verbose`.
- Standardise `cli_inform()`, `cli_warn()`, and `cli_abort()` messages.
- Remove redundant
  [`resample_mnirs()`](https://jemarnold.github.io/mnirs/reference/resample_mnirs.md)
  argument `resample_time`.

## mnirs 0.1.6

- Create internal numeric vector helper functions
  - [`signif_whole()`](https://jemarnold.github.io/mnirs/reference/signif_trailing.md)
    applies sig-fig rounding to decimal places, or whole values.
  - [`signif_trailing()`](https://jemarnold.github.io/mnirs/reference/signif_trailing.md)
    applies decimal or sig-fig rounding and convers to character strings
    with trailing zeroes, for display.
  - [`signif_pvalue()`](https://jemarnold.github.io/mnirs/reference/signif_trailing.md)
    applies formatting for p-value display as character strings or
    significance symbols.
  - [`seq_range()`](https://jemarnold.github.io/mnirs/reference/seq_range.md)
    creates a numeric sequence spanning the range of an ipnut vector.
  - [`wrap()`](https://jemarnold.github.io/mnirs/reference/wrap.md)
    rotates vector elements from head to tail (or tail to head) by
    position.
- Add statement about use of generative AI codebots to README.Rmd

## mnirs 0.1.5

- Remove redundant validation checks in
  [`replace_mnirs()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md).
- Fix erroneous url in \_pkgdown.yml.
- Update internal `validate_event_channel` to error on empty numeric or
  character column.
- Update test coverage:
  - Cover new `options(mnirs.inform = FALSE)`.
  - Cover unlisted metadata added to
    [`create_mnirs_data()`](https://jemarnold.github.io/mnirs/reference/create_mnirs_data.md).
  - Cover hidden
    [`plot.mnirs()`](https://jemarnold.github.io/mnirs/reference/plot.mnirs.md)
    options.

## mnirs 0.1.4

- Update argument `inform` replaces `verbose` to display/hide messages &
  warnings.
- Implement global option to set `inform = FALSE` with
  `options(mnirs.inform = FALSE)`.
  - Global option `mnirs.inform = FALSE` will override functions’
    implicit default `inform = TRUE`.
  - Explicit call `inform = TRUE` will override the global option.
- Update
  [`replace_mnirs()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  and
  [`replace_invalid()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  with additional arguments.
  - Replace ranges above or below `invalid_above` and `invalid_below`,
    respectively.

## mnirs 0.1.3

- Remove redundant file mnirs.test-package.Rd.
- Remove obsolete {utils} dependency.
- Remove redundant `nirs_device` arg in internal
  [`read_data_table()`](https://jemarnold.github.io/mnirs/reference/read_data_table.md).
- Implement [air](https://github.com/soumyaray/air) formatter.
- Migrate package development into Positron IDE.

## mnirs 0.1.2

- Simplify use of `width` and `span` to be centred on `idx`.
  - Update internal helpers:
    [`compute_local_windows()`](https://jemarnold.github.io/mnirs/reference/compute_helpers.md)
    & `compute_window_of_valid_neighbours()`
  - Update appropriate documentation.
  - Update README, vignette, & examples to `width = 10`.
- Update examples with
  `@examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))`.
  - Not entirely sure this will pass CRAN. May need to update.
- Update
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
