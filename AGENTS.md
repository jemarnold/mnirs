# mnirs Package Reference for LLM Agents

This document gives external/user-facing structure for LLM agents working with
`{mnirs}`.

Use this file as a workflow and dependency map. For runnable examples and
end-user walkthroughs, refer to `README.md` and vignettes.

**Version:** 0.6.1 | **Language:** R | **License:** MIT

`{mnirs}` reads, processes, and analyses muscle near-infrared spectroscopy
(mNIRS) time-series data. It provides a standardised pipeline from raw device
files through to kinetics analysis.

---

## 1. Core Classes

### 1.1 `"mnirs"` — tibble subclass

Most pipeline functions accept and return a `"mnirs"` tibble (data frame). 
Metadata is stored in attributes.

| Attribute         | Type              | Description                                      |
|-------------------|-------------------|--------------------------------------------------|
| `nirs_channels`   | character vector  | Names of NIRS signal columns                     |
| `time_channel`    | character(1)      | Name of the time column                          |
| `event_channel`   | character(1)/NULL | Name of event/lap column                         |
| `nirs_device`     | character(1)/NULL | Device name if auto-detected                     |
| `sample_rate`     | numeric(1)        | Samples per second (Hz)                          |
| `start_timestamp` | POSIXct/NULL      | Absolute start datetime of recording             |
| `interval_times`  | numeric/NULL      | Start/end times set by `extract_intervals()`     |
| `interval_span`   | numeric(2)/NULL   | Span used in `extract_intervals()`               |

`verbose` is **not** a stored attribute. It is read from
`getOption("mnirs.verbose", default = TRUE)` per call or passed explicitly.

Access attributes with `attributes(data)` or `attr(data, "nirs_channels")` etc.

### 1.2 `"mnirs_kinetics"` — list

Returned by `analyse_kinetics()`.

| Element          | Type        | Description                                                    |
|------------------|-------------|----------------------------------------------------------------|
| `method`         | character   | Method used, e.g. `"peak_slope"`                               |
| `model`          | named list  | Per-interval, per-channel model objects (`lm`, `nls`)          |
| `coefficients`   | data frame  | One row per channel per interval; columns vary by method       |
| `data`           | named list  | Input `"mnirs"` dfs with `<channel>_fitted` columns            |
| `interval_times` | data frame  | `interval`, `interval_times` from metadata                     |
| `diagnostics`    | data frame  | `n_obs`, `r2`, `adj_r2`, `pseudo_r2`, `rmse`, `snr`, `cv_rmse` |
| `channel_args`   | data frame  | Resolved arguments per channel per interval                    |
| `call`           | call        | Matched function call                                          |

---

## 2. Pipeline Overview

```
read_mnirs()
  └── resample_mnirs()       [regularise time grid, up/down-sample]
        └── replace_mnirs()  [clean invalid / outliers / NA]
              └── filter_mnirs()  [smooth signal]
                    ├── shift_mnirs()    [optional: shift baseline]
                    ├── rescale_mnirs()  [optional: normalise range]
                    └── extract_intervals()
                              └── analyse_kinetics()
                                        └── plot() / print()
```

### Ordering recommendations

- `resample_mnirs()` before `replace_mnirs()`: regularises the grid first so
  window-based replacement operates on evenly spaced samples.
- `replace_mnirs()` before `filter_mnirs()`: `"smooth_spline"` and
  `"butterworth"` error on internal `NA` values unless `na.rm = TRUE`.
- `resample_mnirs()` before `filter_mnirs(..., method = "smooth_spline")`:
  smooth spline errors on duplicated time values.
- `resample_mnirs()` before `extract_intervals(..., event_groups = "ensemble")`:
  ensemble averaging requires regular samples (warns if irregular).
- `extract_intervals()` before `analyse_kinetics()` when working with multiple
  intervals.

---

## 3. Function Reference

### 3.1 Read

#### `read_mnirs()`

Import raw mNIRS data from `.csv`, `.xls`, or `.xlsx` files.

```r
read_mnirs(
    file_path,
    nirs_channels = NULL,   # char vec; named = rename: c(new = "old")
    time_channel  = NULL,   # char(1); named = rename: c(time = "Timestamp")
    event_channel = NULL,   # char(1); optional; named = rename
    sample_rate   = NULL,   # numeric(1) Hz; estimated if NULL
    add_timestamp = FALSE,  # add POSIXct "timestamp" column
    zero_time     = FALSE,  # rebase time to start at 0
    keep_all      = FALSE,  # keep non-specified columns
    verbose       = TRUE
)
```

- Returns a `"mnirs"` tibble.
- When `nirs_channels = NULL`, auto-detection is attempted (device must be
  recognised).
- POSIXct time columns are always converted to numeric seconds rebased to 0.
- Columns without headers are named `col_*` (e.g. Artinis event columns).

#### `example_mnirs()`

```r
example_mnirs(file = NULL)
## file = NULL  → returns character vector of available filenames
## file = "..."  → returns file path (use as file_path in read_mnirs())
## available names include: "moxy_ramp", "train.red", "artinis", "portamon",
##   "vo2master"
```

#### `create_mnirs_data()`

Low-level constructor; wraps a data frame as a `"mnirs"` object and sets
attributes. Called internally at the end of every data-frame pipeline function 
to propagate and update/overwrite metadata. Existing attributes not specified 
are preserved.

```r
create_mnirs_data(
    data, 
    nirs_channels = c("hhb", "smo2"),
    time_channel = "time", 
    ...
)
```

---

### 3.2 Resample

#### `resample_mnirs()`

Regularise the time grid or change sample rate.

```r
resample_mnirs(
    data,
    time_channel  = NULL,
    sample_rate   = NULL,         # source Hz; estimated if NULL
    resample_rate = sample_rate,  # target Hz; defaults to source rate
    method = c("none", "locf", "linear"),
    verbose = TRUE
)
```

- Default `method = "none"` passes any new samples as `NA`.
- Non-numeric columns always use `"locf"` (last observation carried forward) 
  regardless of `method`.
- When `resample_rate = sample_rate` (default) or `NULL`, regularises sample 
  (i.e. time) grid only, without changing sample rate.

---

### 3.3 Clean

#### `replace_mnirs()`

Replace invalid values, outliers, and missing values. Processing order within a
call: invalid → outliers → missing (NA).

```r
replace_mnirs(
    data,
    nirs_channels  = NULL,
    time_channel   = NULL,
    invalid_values = NULL,   # numeric vec; exact values to replace
    invalid_above  = NULL,   # numeric(1); replace x > threshold
    invalid_below  = NULL,   # numeric(1); replace x < threshold
    outlier_cutoff = NULL,   # numeric(1); Hampel MAD multiplier; NULL = skip
    width          = NULL,   # integer; rolling window in samples
    span           = NULL,   # numeric; rolling window in time units
    method = c("linear", "median", "locf", "none"),
    verbose = TRUE
)
```

Vector-level functions (operate on plain numeric vectors):

```r
replace_invalid(x, t, invalid_values, invalid_above, invalid_below,
                width, span, method = c("median", "none"), ...)
replace_outliers(x, t, outlier_cutoff = 3, width, span,
                 method = c("median", "none"), ...)
replace_missing(x, t, width, span,
                method = c("linear", "median", "locf"), ...)
```

Note: `replace_invalid()` and `replace_outliers()` default to 
`method = "median"`; `replace_mnirs()` and `replace_missing()` default 
to `"linear"`.

---

### 3.4 Filter

#### `filter_mnirs()`

Smooth NIRS signal channels. Dispatches via S3 on `method` with method-specific
additional arguments.

```r
filter_mnirs(
    data,
    nirs_channels = NULL,
    time_channel  = NULL,
    method = c("smooth_spline", "butterworth", "moving_average"),
    na.rm   = FALSE,
    verbose = TRUE,
    ...     # method-specific arguments
)
```

**Method aliases:**

| Canonical         | Accepted aliases                                 |
|-------------------|--------------------------------------------------|
| `"smooth_spline"` | `"spline"`, `"smooth spline"`, `"smooth-spline"` |
| `"butterworth"`   | `"butter"`                                       |
| `"moving_average"`| `"ma"`, `"moving average"`, `"moving-average"`   |

**`"smooth_spline"`** — wraps `stats::smooth.spline()`:
- `spar`: smoothing parameter; `NULL` = auto via GCV
- Errors on internal `NA` unless `na.rm = TRUE`
- Errors on duplicated time values — run `resample_mnirs()` first

**`"butterworth"`** — requires `{signal}`:
- `order`: integer, default `2L`
- `W` or `fc`: normalised cutoff (0–1) or frequency in Hz
- `type`: `"low"` (default), `"high"`, `"stop"`, `"pass"`
- `edges`: `"rev"` (default), `"rep1"`, `"none"` (edge handling)
- Errors on internal `NA` unless `na.rm = TRUE`

**`"moving_average"`**:
- `width`: integer; number of samples in window
- `span`: numeric; window duration in `time_channel` units
- `partial`: logical, default `FALSE`; allow edge windows with fewer samples

Vector-level functions:

```r
filter_ma(x, t, width, span, partial = FALSE, na.rm = FALSE, ...)
filter_moving_average(...)    # alias for filter_ma()
filter_butter(x, order = 2L, W, type = "low", edges = "rev", na.rm = FALSE)
```

---

### 3.5 Transform (optional)

#### `shift_mnirs()`

Shift NIRS channel absolute values to a new reference level.

```r
shift_mnirs(
    data,
    nirs_channels = list(), # list() or NULL → auto from metadata
    time_channel  = NULL,
    to       = NULL,        # numeric(1); target level (overrides `by`)
    by       = NULL,        # numeric(1); shift amount
    width    = NULL,        # integer; window samples for reference
    span     = NULL,        # numeric; window time units for reference
    position = c("min", "max", "first"),
    verbose  = TRUE
)
```

#### `rescale_mnirs()`

Normalise NIRS channel amplitudes to a specified numeric range.

```r
rescale_mnirs(
    data,
    nirs_channels = list(),   # list() or NULL → auto from metadata
    range,                    # numeric(2): c(min, max) target range
    verbose = TRUE
)
```

**`nirs_channels` list semantics** (applies to both `shift_mnirs` and
`rescale_mnirs`):

| Syntax                         | Behaviour                                                           |
|--------------------------------|---------------------------------------------------------------------|
| `list("A", "B")`               | Each channel processed independently                                |
| `list(c("A", "B"))`            | Both channels share a common reference (relative scaling preserved) |
| `list(c("A", "B"), c("C"))`    | A & B share reference; C processed independently                    |

- `list(c("A", "B"))` is default when `nirs_channels` retrieved from metadata.

---

### 3.6 Extract Intervals

#### `extract_intervals()`

Slice data into time windows. Returns a **named list** of `"mnirs"` data frames.

```r
extract_intervals(
    data,
    nirs_channels = NULL,
    time_channel  = NULL,
    event_channel = NULL,
    sample_rate   = NULL,
    start         = NULL,    # by_time(), by_label(), by_lap(), by_sample()
    end           = NULL,    # same; NULL = use span from start
    span          = list(c(-60, 60)),  # c(before, after) per interval
    event_groups  = c("distinct", "ensemble"),
    zero_time     = FALSE,
    verbose       = TRUE
)
```

- Raw coercion: numeric `start`/`end` → `by_time()`; character → `by_label()`;
  explicit integer (e.g. `2L`) → `by_lap()`.
- `span` recycling: single positive scalar → `c(0, x)`; single negative →
  `c(x, 0)`.
- `event_groups = "distinct"`: returns one data frame per event (list length n).
- `event_groups = "ensemble"`: returns a single-element list with 
  an ensemble-averaged data frame.
- `event_groups = list(c(1, 2), c(3, 4))`: returns a list with an 
  ensemble-averaged data frame for each group.
- List names: `ensemble` or `"interval_1"`, `"interval_2"`, ...

**Boundary helpers (all exported):**

```r
by_time(...)     # numeric time values in time_channel units
by_sample(...)   # integer row indices
by_label(...)    # character strings matching event_channel values
by_lap(...)      # integer lap numbers
```

---

### 3.7 Analyse Kinetics

#### `analyse_kinetics()`

Fit kinetics models to NIRS channels across intervals. Dispatches via S3 on 
`method` with method-specific additional arguments and returns formatted 
results table via `print.mnirs_kinetics()`.

```r
analyse_kinetics(
    data,                   # "mnirs" df, named list of "mnirs" dfs,
                            # or dplyr grouped df (requires {dplyr})
    nirs_channels  = NULL,
    time_channel   = NULL,
    method = c("half_response_time", "peak_slope", "monoexponential", "sigmoidal"),
    direction = c("auto", "positive", "negative"),
    end_fit_span   = 20,    # in time units; truncate fit after extreme
    channel_args   = list(),
    verbose        = TRUE,
    ...                     # method-specific arguments
)
```

**Method aliases:**

| Canonical              | Accepted aliases                                                         |
|------------------------|--------------------------------------------------------------------------|
| `"half_response_time"` | `"half time"`, `"response/recovery time"` (with `"-"` or `"_"`), `"HRT"` |
| `"peak_slope"`         | `"peak slope"`, `"slope"`                                                |
| `"monoexponential"`    | `"monoexp"`, `"exponential"`, `"MRT"`, `"tau"`                           |
| `"sigmoidal"`          | `"logistic"`, `"xmid"`                                                   |

**Data input formats:**
- Single `"mnirs"` df → processed as one interval named `"interval_1"`.
- Named list of `"mnirs"` dfs → each processed separately; use output of
  `extract_intervals()`.
- `dplyr::group_by()` grouped df → split by group levels. Requires `{dplyr}`.

**Per-method `...` arguments:**

`"half_response_time"`:
- `t0`: numeric(1); response start in `time_channel` units; default `0`
- `fraction`: numeric in `[0, 1]`; default `0.5`; `0.632` ≈ MRT

`"peak_slope"`:
- `width` or `span`: one required; samples (integer) or time units (numeric)
- `align`: `"centre"`/`"center"` (default), `"left"`, `"right"`
- `partial`: logical, default `FALSE`

`"monoexponential"`:
- `time_delay`: logical, default `TRUE`; attempt 4-param fit (`A B tau TD`)
  before falling back to 3-param (`A B tau`)
- Additional `stats::nls()` arguments accepted

**Per-channel overrides via `channel_args`:**

```r
analyse_kinetics(
    data,
    nirs_channels = c(hhb, smo2),
    method        = "peak_slope",
    span          = 5,
    channel_args  = list(
        smo2 = list(span = 10),
        hhb  = list(direction = "negative")
    )
)
```

Names in `channel_args` must match `nirs_channels` exactly.

#### `"mnirs_kinetics"` Coefficients by Method

**`"half_response_time"`**: `interval`, `nirs_channels`, `time_channel`, `A`, 
`B`, `response_time`, `response_value`, `fitted`, `idx`

**`"peak_slope"`**: `interval`, `nirs_channels`, `time_channel`, `slope`,
`intercept`, `fitted`, `<time_channel>`, `idx`

**`"monoexponential"`**: `interval`, `nirs_channels`, `time_channel`, `A`, `B`,
`tau`, `k`, `TD`, `MRT`, `HRT`, `tau_fitted`, `MRT_fitted`, `HRT_fitted`

**Diagnostics:** `pseudo_r2` is preferred for `"monoexponential"` (squared
Pearson correlation between observed and fitted). `adj_r2` is appropriate only
for `"peak_slope"` (OLS). `snr` is in dB.

#### Vector-level kinetics functions

```r
## monoexponential curve values
monoexponential(t, A, B, tau, TD = NULL)

## self-starting NLS models for use in stats::nls()
nls(x ~ SS_monoexp3(t, A, B, tau), data = df)
nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = df)

## peak local linear slope
peak_slope(
    x, 
    t = seq_along(x),
    width = NULL, 
    span = NULL,
    align = c("centre", "left", "right"),
    direction = c("auto", "positive", "negative"),
    partial = FALSE, 
    na.rm = FALSE, 
    verbose = TRUE
)
## returns list: slope, intercept, y, t, idx, fitted, window_idx, model

## fractional response time
response_time(
    x, 
    t = seq_along(x),
    t0 = 0, 
    fraction = 0.5, 
    end_fit_span = 20,
    direction = c("auto", "positive", "negative"),
    verbose = TRUE
)
## returns list: A, B, response_time, response_value, fitted,
##               baseline_idx, response_idx, extreme_idx
```

#### Data frame-level analysis functions

These internal functions wrap the vector-level kinetics functions and return 
formatted results for a single data frame, to be combined by `analyse_kinetics`
into a list of results.

```r
## monoexponential curve values
analyse_monoexponential(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    time_delay = TRUE,
    direction = c("auto", "positive", "negative"),
    end_fit_span = 20,
    channel_args = list(),
    verbose = TRUE,
    ...
)

## peak local linear slope
analyse_peak_slope(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    width = NULL,
    span = NULL,
    align = c("centre", "left", "right"),
    direction = c("auto", "positive", "negative"),
    end_fit_span = 20,
    partial = FALSE,
    na.rm = FALSE,
    channel_args = list(),
    verbose = TRUE,
    ...
)

## fractional response time
analyse_response_time <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    t0 = 0,
    fraction = 0.5,
    direction = c("auto", "positive", "negative"),
    end_fit_span = 20,
    channel_args = list(),
    verbose = TRUE,
    ...
)
```

---

### 3.8 Plot and Print

#### `plot.mnirs()`

Requires `{ggplot2}` and optionally `{scales}`. Called via generic `plot(data)`
or explicit `plot.mnirs(data)` to preview in-line arguments. Call documentation
with `?plot.mnirs`.

```r
plot.mnirs(
    x,                      # "mnirs" object
    points      = FALSE,    # add geom_point()
    time_labels = FALSE,    # format x-axis as "mm:ss"
    n.breaks    = 5,        # number of x-axis breaks
    na.omit     = FALSE     # TRUE: connects lines across missing values
)
## returns a ggplot2 object
```

#### Theme and palette

Custom `{ggplot2}` theme and helper functions

```r
theme_mnirs(
    base_size = 14, 
    base_family = "sans",
    border = c("partial", "full"), 
    ...
)

palette_mnirs()              # all 12 named colours (character vector)
palette_mnirs(4)             # first 4 colours
palette_mnirs("red", "blue") # by name

scale_colour_mnirs(...)      # discrete colour scale
scale_color_mnirs(...)       # alias
scale_fill_mnirs(...)        # discrete fill scale

breaks_timespan(unit = "secs", n = 5)  # pretty time axis breaks
format_hmmss(x)              # format numeric seconds as "mm:ss" or "h:mm:ss"
```

#### `print.mnirs_kinetics()`

```r
print(result)   # formatted coefficient table; truncates at 10 rows
```

---

## 4. Global Options

Warnings and informational messages can be silenced globally when working
through a familiar workflow. Having verbose messages enabled by default is
preferred for working through an exploratory data file.

```r
options(mnirs.verbose = FALSE)   # suppress all informational messages
options(mnirs.verbose = TRUE)    # restore (default)
```

The `verbose` argument in any function overrides the global option for that
call only.

---

## 5. Dependencies

| Package      | Role                               | Type      | Notes                              |
|--------------|------------------------------------|-----------|------------------------------------|
| `cli`        | User-facing messages               | Import    |                                    |
| `data.table` | Internal data manipulation         | Import    |                                    |
| `lifecycle`  | Deprecation helpers                | Import    |                                    |
| `readxl`     | XLS/XLSX reading                   | Import    |                                    |
| `rlang`      | NSE / tidy evaluation              | Import    |                                    |
| `tibble`     | `"mnirs"` tibble class             | Import    |                                    |
| `tidyselect` | Column selection                   | Import    |                                    |
| `signal`     | Butterworth filter                 | Suggests  | Required for `"butterworth"` method |
| `ggplot2`    | Plotting                           | Suggests  | Required for `plot.mnirs()`         |
| `scales`     | Axis formatting                    | Suggests  | Required for `plot.mnirs()`         |
| `dplyr`      | Grouped df input                   | Suggests  | Required for grouped `analyse_kinetics()` |
| `zoo`        | Some replacement interpolation     | Suggests  |                                    |

---

## 6. Complete Workflow Example

```r
library(mnirs)

options(mnirs.verbose = FALSE) ## silence messages for a familiar workflow

## -- 1. Read --
data <- read_mnirs(
    file_path     = example_mnirs("train.red"),
    nirs_channels = c(smo2 = "SmO2 unfiltered"),
    time_channel  = c(time = "Timestamp (seconds passed)"),
    zero_time     = TRUE
)
## attr(data, "nirs_channels") == "smo2"
## attr(data, "time_channel")  == "time"
## attr(data, "sample_rate")   == 2

## -- 2. Resample --
data <- resample_mnirs(data, method = "linear")
## regularises time grid with linear interpolation; no rate change by default

## -- 3. Clean --
data <- replace_mnirs(
    data,
    invalid_below  = 0,
    invalid_above  = 100,
    outlier_cutoff = 3,        # Hampel MAD multiplier
    method         = "linear"
)

## -- 4. Intermediate data visualisation --
plot(data, points = TRUE, time_labels = TRUE)
## visualising data early and often is recommended for exploratory workflows

## -- 5. Filter --
data <- filter_mnirs(data, method = "moving_average", span = 15)

## -- 6. Extract intervals --
intervals <- extract_intervals(
    data,
    start        = c(66, 781),  # two interval onset times (seconds)
    span         = c(-30, 90),  # 30 s pre-baseline, 90 s deoxygenation kinetics
    event_groups = "distinct",  # return a two-item list
    zero_time    = TRUE
)
## → named list: list(interval_1 = <mnirs>, interval_2 = <mnirs>)

plot(intervals$interval_1, time_labels = TRUE) ## intermeiate plot

## -- 7. Analyse --
result <- analyse_kinetics(
    data          = intervals,
    method        = "monoexponential",
    direction     = "auto", ## detect direction by net slope direction
    end_fit_span  = 20,     ## end kinetics fit at first extreme with no
                            ## greater/lesser values within given time span
    time_delay    = TRUE    ## specify a 4-parameter monoexp model
)

## -- 8. Inspect results --
result$coefficients   # A, B, tau, k, TD, MRT, HRT, smo2_fitted
result$diagnostics    # n_obs, r2, pseudo_r2, rmse, snr, cv_rmse
result$model          # named list of nls model objects
result$data           # list of class "mnirs" dfs with smo2_fitted column and metadata

## -- 9. Plot --
plot(result$data, time_labels = TRUE)

```

---

## 7. Common Constraints and Gotchas

| Constraint | Detail |
|---|---|
| `example_mnirs()` returns a **file path**, not data | Pass the result to `read_mnirs(file_path = example_mnirs("..."))` |
| Irregular samples warning | `read_mnirs()` may return a warning about irregular samples in the data frame at the start of a processing pipeline, even if `resample_mnirs()` is run further on. The issue will be corrected and can be confirmed by glimpsing the data frame after resampling.|
| Pipeline order | Correct order: `resample → replace → filter`. Reversing these steps can produce incorrect results. |
| `replace_invalid` / `replace_outliers` vector-level default methods are `"median"` | `replace_mnirs()` data frame-level default method is `"linear"`(via `replace_missing()`) |
| `"smooth_spline"` / `"butterworth"` fail on `NA` | Run `replace_mnirs()` before these methods, or set `na.rm = TRUE` |
| `"smooth_spline"` fails on duplicated time values | Run `resample_mnirs()` before smooth spline |
| Ensemble averaging requires regular samples | `extract_intervals(..., event_groups = "ensemble")` warns if irregular; run `resample_mnirs()` first |
| `nirs_channels = list()` in `shift_mnirs` / `rescale_mnirs` | Empty list auto-retrieves from metadata; see §3.5 for grouping semantics |
| `channel_args` names must match `nirs_channels` exactly | Uses `utils::modifyList()`; unrecognised names are silently ignored |
| `find_kinetics_idx()` searches only `t >= 0` for extreme | Use `zero_time = TRUE` in `extract_intervals()` to align baseline correctly |
| `monoexponential`: 4-param fit first, falls back to attempt 3-param, HRT fallback if both fail to converge | Convergence failure triggers a warning; falls back automatically |
| Single df input to `analyse_kinetics()` → `"interval_1"` | Coefficients will have `interval = "interval_1"` |

---

## 8. Key Source Files

| File | Contents |
|---|---|
| `R/read_mnirs.R` | `read_mnirs()`, `example_mnirs()`, `create_mnirs_data()` |
| `R/read_mnirs_helpers.R` | Internal file reading & formatting helpers |
| `R/resample_mnirs.R` | `resample_mnirs()` |
| `R/replace_mnirs.R` | `replace_mnirs()`, `replace_invalid()`, `replace_outliers()`, `replace_missing()` |
| `R/replace_helpers.R` | Internal rolling window and NA-action helpers |
| `R/filter_mnirs.R` | `filter_mnirs()` S3 dispatch, `filter_ma()` (alias: `filter_moving_average()`), `filter_butter()` |
| `R/shift_mnirs.R` | `shift_mnirs()` |
| `R/rescale_mnirs.R` | `rescale_mnirs()` |
| `R/extract_intervals.R` | `extract_intervals()` |
| `R/extract_interval_helpers.R` | `by_time()`, `by_label()`, `by_lap()`, `by_sample()` |
| `R/analyse_kinetics.R` | `analyse_kinetics()` S3 dispatch, `compute_diagnostics()` |
| `R/analyse_kinetics_helpers.R` | Internal kinetics analysis helpers |
| `R/analyse_peak_slope.R` | `peak_slope()` |
| `R/analyse_monoexponential.R` | `monoexponential()`, `SS_monoexp3()`, `SS_monoexp4()` |
| `R/analyse_response_time.R` | `response_time()` |
| `R/plot.mnirs.R` | `plot.mnirs()`, `theme_mnirs()`, `palette_mnirs()`, `{ggplot2}` scale functions |
| `R/signif_trailing.R` | Internal numeric vector manipulation and display helpers |
| `R/validate_mnirs.R` | Internal input validation |
| `R/data.R` | Package example files descriptions |
