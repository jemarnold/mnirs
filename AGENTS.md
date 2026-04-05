# mnirs Package Reference for LLM Agents

This document gives external/user-facing structure for LLM agents working with
`{mnirs}`.

Use this file as a workflow and dependency map. For runnable examples and
end-user walkthroughs, refer to `README.md` and vignettes.

**Version:** 0.6.1 | **Language:** R | **License:** MIT

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

`verbose` is **not** a stored attribute — read from
`getOption("mnirs.verbose", default = TRUE)` per call or passed explicitly.

Access attributes with `attr(data, "nirs_channels")` etc.

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

- `resample_mnirs()` before `replace_mnirs()`: window-based replacement
  operates on regularised `time_channel` samples.
- `replace_mnirs()` before `filter_mnirs()`: `"smooth_spline"` and
  `"butterworth"` error on internal `NA` unless `na.rm = TRUE`.
- `resample_mnirs()` before `filter_mnirs(..., method = "smooth_spline")`:
  smooth spline errors on duplicated time values.
- `resample_mnirs()` before `extract_intervals(..., event_groups = "ensemble")`:
  ensemble averaging requires regular samples (warns if irregular).
- `extract_intervals()` before `analyse_kinetics()` when working with multiple
  intervals.

---

## 3. Function Reference

### 3.1 Read

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

example_mnirs(file = NULL) # char vec; returns example file paths
## available: "moxy_ramp", "train.red", "artinis", "portamon", "vo2master"

create_mnirs_data(data, ...)
```

- `example_mnirs()` calls example mNIRS file paths for 
  `read_mnirs(file_path = example_mnirs(...))`. If left blank will return file
  names of all included files. Uses partial name matching.
- `create_mnirs_data()` is a low-level constructor called by `*_mnirs()` 
  functions; wraps a data frame as "mnirs" and sets/updates attributes.


---

### 3.2 Resample

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

- Default `method = "none"` passes new (interpolated) samples as `NA`.
- Non-numeric columns always use `"locf"` regardless of `method`.
- When `resample_rate = sample_rate` (default), regularises samples only 
  without changing rate.

---

### 3.3 Clean

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

- Processing order within a call: invalid → outliers → missing (NA).
- Vector-level functions (operate on simple numeric response variable `x` and
  predictor variable/time series `t`):

```r
replace_invalid(x, t, invalid_values, invalid_above, invalid_below,
                width, span, method = c("median", "none"), ...)
replace_outliers(x, t, outlier_cutoff = 3, width, span,
                 method = c("median", "none"), ...)
replace_missing(x, t, width, span,
                method = c("linear", "median", "locf"), ...)
```

- `replace_invalid()` and `replace_outliers()` default to `method = "median"`.
- `replace_mnirs()` and `replace_missing()` default to `"linear"`.

---

### 3.4 Filter

```r
filter_mnirs(
    data,
    nirs_channels = NULL,
    time_channel  = NULL,
    method = c("smooth_spline", "butterworth", "moving_average"),
    na.rm   = FALSE,
    verbose = TRUE,
    ...     # method-specific arguments (see below)
)
```

**Method aliases:**

| Canonical          | Accepted aliases                                 |
|--------------------|--------------------------------------------------|
| `"smooth_spline"`  | `"spline"`, `"smooth spline"`, `"smooth-spline"` |
| `"butterworth"`    | `"butter"`                                       |
| `"moving_average"` | `"ma"`, `"moving average"`, `"moving-average"`   |

**Method-specific arguments:**

**`"smooth_spline"`** — wraps `stats::smooth.spline()`:
- `spar`: smoothing parameter; `NULL` = auto via GCV
- Errors on internal `NA` unless `na.rm = TRUE`
- Errors on duplicated time values — run `resample_mnirs()` first

**`"butterworth"`** — requires `{signal}`:
- `order`: integer, default `2L`
- `W` or `fc`: normalised cutoff (0–1) or frequency in Hz
- `type`: `"low"` (default), `"high"`, `"stop"`, `"pass"`
- `edges`: `"rev"` (default), `"rep1"`, `"none"`
- Errors on internal `NA` unless `na.rm = TRUE`

**`"moving_average"`**:
- `width`: integer; number of samples in window
- `span`: numeric; window duration in `time_channel` units
- `partial`: logical, default `FALSE`; allow edge windows with fewer samples

- Vector-level functions:

```r
filter_ma(x, t, width, span, partial = FALSE, na.rm = FALSE, ...)
filter_moving_average()    # alias for filter_ma()
filter_butter(x, order = 2L, W, type = "low", edges = "rev", na.rm = FALSE)
```

---

### 3.5 Transform (optional)

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

rescale_mnirs(
    data,
    nirs_channels = list(),   # list() or NULL → auto from metadata
    range,                    # numeric(2): c(min, max) target range
    verbose = TRUE
)
```

**`nirs_channels` list semantics** (applies to both functions):

| Syntax                      | Behaviour                                                           |
|-----------------------------|---------------------------------------------------------------------|
| `list("A", "B")`            | Each channel processed independently                                |
| `list(c("A", "B"))`         | Both channels share a common reference (relative scaling preserved) |
| `list(c("A", "B"), c("C"))` | A & B share reference; C processed independently                    |

- `list(c("A", "B"))` is the default when `nirs_channels` retrieved from 
  metadata.

---

### 3.6 Extract Intervals

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

Returns a **named list** of `"mnirs"` data frames.

- Raw coercion of `start` and `end` values: numeric → `by_time()`; 
  character → `by_label()`; explicit integer (e.g. `2L`) → `by_lap()`.
- `start = by_lap(), end = NULL` uses the full range of the specified lap 
  integers. `start = by_lap(), end = by_lap()` uses the range from the first
  `start` lap sample to the last `end` lap sample.
- `span` recycled to boundaries as specified; `span = c(A, B)` → `A` applies 
  to the first boundary (`start`), `B` applies to the last boundary (`end` if 
  specified, otherwise `start`).
- `span` negative values apply a buffer before the boundary, positive values 
  buffer after the boundary.
- `event_groups = "distinct"`: one data frame per event.
- `event_groups = "ensemble"`: single-element list with ensemble-averaged df.
- `event_groups = list(c(1, 2), c(3, 4))`: one ensemble-averaged df per group.
- List names: `"ensemble"` or `"interval_1"`, `"interval_2"`, ...

**Boundary helpers (all exported):**

```r
by_time(...)     # numeric time values in time_channel units
by_sample(...)   # integer row indices
by_label(...)    # character strings matching event_channel values
by_lap(...)      # integer lap numbers
```

---

### 3.7 Analyse Kinetics

```r
analyse_kinetics(
    data,                   # "mnirs" df, named list of "mnirs" dfs,
                            # or dplyr grouped df (requires {dplyr})
    nirs_channels  = NULL,
    time_channel   = NULL,
    method = c("half_response_time", "peak_slope", "monoexponential", "sigmoidal"),
    direction = c("auto", "positive", "negative"),
    end_fit_span   = 20,    # time units; truncate fit after extreme
    channel_args   = list(),
    verbose        = TRUE,
    ...                     # method-specific arguments (see below)
)
```

- Returns a formatted results table via `print.mnirs_kinetics()`.fs

**Method aliases:**

| Canonical              | Accepted aliases                                   |
|------------------------|----------------------------------------------------|
| `"half_response_time"` | `"half time"`, `response|recovery time"`, `"HRT"` |
| `"peak_slope"`         | `"peak slope"`, `"slope"`                          |
| `"monoexponential"`    | `"monoexp"`, `"exponential"`, `"MRT"`, `"tau"`     |
| `"sigmoidal"`          | `"logistic"`, `"xmid"`                             |

**Data input formats:**
- Single `"mnirs"` df → processed as one interval named `"interval_1"`.
- Named list of `"mnirs"` dfs → each processed separately (use output of
  `extract_intervals()`).
- `dplyr::group_by()` grouped df → split by group levels.

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

- Names in `channel_args` must match `nirs_channels` exactly; unrecognised 
  names are silently ignored.

#### Coefficients by method

**`"half_response_time"`**: `interval`, `nirs_channels`, `time_channel`, `A`,
`B`, `response_time`, `response_value`, `fitted`, `idx`

**`"peak_slope"`**: `interval`, `nirs_channels`, `time_channel`, `slope`,
`intercept`, `fitted`, `<time_channel>`, `idx`

**`"monoexponential"`**: `interval`, `nirs_channels`, `time_channel`, `A`, `B`,
`tau`, `k`, `TD`, `MRT`, `HRT`, `tau_fitted`, `MRT_fitted`, `HRT_fitted`

**Diagnostics:** `pseudo_r2` preferred for `"monoexponential"` (squared Pearson
correlation); `adj_r2` for `"peak_slope"` (OLS only); `snr` in dB.

#### Vector-level kinetics functions

```r
monoexponential(t, A, B, tau, TD = NULL)   # monoexponential curve values

## self-starting NLS models for use in stats::nls()
nls(x ~ SS_monoexp3(t, A, B, tau), data = df)
nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = df)

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

---

### 3.8 Plot and Print

```r
plot.mnirs(
    x, 
    points = FALSE, 
    time_labels = FALSE, 
    n.breaks = 5,
    na.omit = FALSE
)
## requires {ggplot2}; called via plot(data); returns a ggplot2 object
## na.omit = TRUE connects lines across missing values

print(result)   # "mnirs_kinetics"; formatted coefficient table (max 10 rows)

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

scale_colour_mnirs(...)      # discrete colour scale (alias: scale_color_mnirs)
scale_fill_mnirs(...)        # discrete fill scale
breaks_timespan(unit = "secs", n = 5)   # pretty time axis breaks
format_hmmss(x)              # format numeric seconds as "mm:ss" or "h:mm:ss"
```

---

## 4. Dependencies

| Package      | Role                       | Type     | Condition                             |
|--------------|----------------------------|----------|---------------------------------------|
| `cli`        | User-facing messages       | Import   |                                       |
| `data.table` | Internal data manipulation | Import   |                                       |
| `lifecycle`  | Deprecation helpers        | Import   |                                       |
| `readxl`     | XLS/XLSX reading           | Import   |                                       |
| `rlang`      | NSE / tidy evaluation      | Import   |                                       |
| `tibble`     | `"mnirs"` tibble class     | Import   |                                       |
| `tidyselect` | Column selection/matching  | Import   |                                       |
| `signal`     | Butterworth filter         | Suggests | Required for `"butterworth"` method   |
| `ggplot2`    | Plotting                   | Suggests | Required for `plot.mnirs()`           |
| `scales`     | Axis formatting            | Suggests | Required for `plot.mnirs()`           |
| `dplyr`      | Grouped df input           | Suggests | Required for grouped `analyse_kinetics()` |
| `zoo`        | rolling functions          | Suggests | Required for internal testing         |

---

## 5. Constraints and Caveats

| Constraint | Detail |
|---|---|
| `example_mnirs()` returns a file path | Pass to `read_mnirs(file_path = example_mnirs("..."))` |
| Irregular samples warning from `read_mnirs()` | Warning will be displayed in a pipe even after `resample_mnirs()` runs downstream; Correct resampling can be confirmed by inspecting the output |
| Pipeline order | `resample → replace → filter`; out of order processing can produce incorrect results |
| `replace_invalid` / `replace_outliers` default `method` is `"median"` | `replace_mnirs()` default is `"linear"` (via `replace_missing()`) |
| `"smooth_spline"` / `"butterworth"` fail on `NA` | Run `replace_mnirs()` first, or set `na.rm = TRUE` |
| `"smooth_spline"` fails on duplicated time values | Run `resample_mnirs()` first |
| Ensemble averaging requires regularised samples | `event_groups = "ensemble"` warns if irregular; run `resample_mnirs()` first |
| `nirs_channels = list()` default in `shift_mnirs` / `rescale_mnirs` | Auto-retrieves from metadata and groups all channels together; see §3.5 |
| `channel_args` names must match `nirs_channels` exactly | Unrecognised names are silently ignored |
| `monoexponential` convergence fallback | 4-param → 3-param → `"half_response_time"` fallback with warnings |
| Single df input to `analyse_kinetics()` | Coefficients will have `interval = "interval_1"` |

---

## 6. Key Source Files

| File | Contents |
|---|---|
| `R/read_mnirs.R` | `read_mnirs()`, `example_mnirs()`, `create_mnirs_data()` |
| `R/resample_mnirs.R` | `resample_mnirs()` |
| `R/replace_mnirs.R` | `replace_mnirs()`, `replace_invalid()`, `replace_outliers()`, `replace_missing()` |
| `R/filter_mnirs.R` | `filter_mnirs()` S3 dispatch, `filter_ma()`, `filter_butter()` |
| `R/shift_mnirs.R` | `shift_mnirs()` |
| `R/rescale_mnirs.R` | `rescale_mnirs()` |
| `R/extract_intervals.R` | `extract_intervals()`, `by_time()`, `by_label()`, `by_lap()`, `by_sample()` |
| `R/analyse_kinetics.R` | `analyse_kinetics()` S3 dispatch, `compute_diagnostics()` |
| `R/analyse_peak_slope.R` | `peak_slope()`, `rolling_slope()` |
| `R/analyse_monoexponential.R` | `monoexponential()`, `SS_monoexp3()`, `SS_monoexp4()` |
| `R/analyse_response_time.R` | `response_time()` |
| `R/plot.mnirs.R` | `plot.mnirs()`, `theme_mnirs()`, `palette_mnirs()`, scale functions |
| `R/validate_mnirs.R` | Internal input validation |
| `R/data.R` | Package example files descriptions |