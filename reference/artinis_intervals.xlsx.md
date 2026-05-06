# 10 Hz Artinis Oxysoft export recorded with Oxymon MKIII

Exported from Artinis Oxysoft, recorded on Oxymon MKIII at 50 Hz and
exported at 10 Hz. Containing two 5-minute cycling work intervals and an
ischaemic occlusion, placed on the vastus lateralis muscle site.

## Format

.xlsx file with header metadata and five columns and 20919 rows:

- Column 1:

  Sample index (divide by sample rate for seconds).

- Column 2:

  O2Hb: oxyhaemoglobin concentration change (\\\mu\\M).

- Column 3:

  HHb: deoxyhaemoglobin concentration change (\\\mu\\M).

- Column 4:

  Event marker (character).

- Column 5:

  Unmarked event label (character).

Channel mapping for
[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md):

- `nirs_channels = c(O2Hb = 2, HHb = 3)`

- `time_channel = c(sample = 1)`

- `event_channel = c(event = 4, label = "col_5")`

- `interval_times = list(start = c(158, 999, 1750) end = c(493, 1333, 1961))`
  two intervals and post-exercise occlusion

## Source

Artinis Medical Systems. Oxymon MKIII, exported via Oxysoft desktop
software (https://artinis.com/)

## See also

[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md),
[`example_mnirs()`](https://jemarnold.github.io/mnirs/reference/example_mnirs.md)

## Examples

``` r
example_mnirs("artinis_intervals")
#> [1] "/home/runner/work/_temp/Library/mnirs/extdata/artinis_intervals.xlsx"
```
