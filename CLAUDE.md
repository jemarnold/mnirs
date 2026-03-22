# CLAUDE.md

## Package Overview

`{mnirs}` is an R package for reading, processing, and analysing muscle near-infrared spectroscopy (mNIRS) data. It imports raw data from CSV/XLS(X) files and provides a standardised pipeline of cleaning, filtering, and analysis functions. It is intended for mNIRS researchers and practitioners in exercise physiology, sports science, and clinical rehabilitation.

## Formatting

- Enforce line limit of 80 characters.
- Comments should briefly explain the "why" of the operation.
- Comments in the form: `## lower case comment`.
- Use explicit `return()` expression at the end of custom functions.

## Architecture

### The `mnirs` S3 Class

- The central data structure is an `"mnirs"` object — a tibble subclass carrying metadata as attributes, mainly including:
    - `nirs_channels` — names of the NIRS signal columns
    - `time_channel` — name of the time column
    - `event_channel` — name of an event (character) or lap (integer) column
    - `sample_rate` — numeric samples per second

- Most processing functions receive and return an `mnirs` data frame, preserving these attributes throughout the pipeline.
- Vector-wise functions operate on `x` (response variable) over `t` (time) and return a vector `y` of the same length as `x`.

### Processing Pipeline

Functions are designed to be pipe-chained in order:

1. **Read** — `read_mnirs()` imports files (CSV, XLS, XLSX) from supported device formats and returns an `mnirs` tibble data frame.
2. **Clean** — `resample_mnirs()` resamples time series; `replace_mnirs()` replaces invalid/outlier/missing values; 
3. **Filter** — `filter_mnirs()` applies Butterworth (`signal` pkg), smooth spline, or moving average filters (S3 dispatch via `method` arg)
4. **Transform** — `shift_mnirs()` for baseline correction; `rescale_mnirs()` to scale to a range
5. **Analyse** — `extract_intervals()` detects events and extracts intervals; `analyse_kinetics()` dispatches kinetics methods (`peak_slope`, with monoexponential/sigmoidal planned)
6. **Plot** — `plot.mnirs()` S3 method; `theme_mnirs()` / `palette_mnirs()` / `scale_colour_mnirs()` for ggplot2 integration

### Key Internal Helpers

In [R/helpers.R](R/helpers.R):
- `compute_local_windows()` / `compute_local_fun()` — rolling window indices and function application
- `compute_outliers()` — MAD-based outlier detection
- `preserve_na()` / `restore_na()` — NA position tracking around operations that drop NAs

### Messaging

- Use `cli_abort()`, `cli_warn()`, `cli_inform()` from `{cli}` for all user-facing messages. 
- The global option `mnirs.verbose` controls whether informational messages are shown.

### Validation

- Input validation lives in [R/validate_mnirs.R](R/validate_mnirs.R) with helpers `validate_numeric()`, `validate_nirs_channels()`, etc.

### Testing

- Unit tests live in `tests/testthat/` for key functionality. 
- Write core functionality and remind the user tests should be written next.
- Tests should not be deleted, modified, or added without asking confirmation.

### Devtools & git commands

- Don't run devtool commands, inform the user which commands to run themselves.
- Don't check previous git commits, only look at current state.

### Documentation

- Roxygen2 with markdown enabled (`Roxygen: list(markdown = TRUE)`). 
- Roxygen2 commented with `#'`
- pkgdown site config is in [_pkgdown.yml](_pkgdown.yml); sections follow the pipeline order: 
    - Read → Pre-process → Interval detection → Process kinetics → Plotting

### Example Data

- Example files in `inst/extdata/` cover supported device formats (Artinis, Moxy, Portamon, Train.Red, VO2 Master). 
- Access via e.g.`example_mnirs("moxy_ramp")` with appropriate column names and event times.
