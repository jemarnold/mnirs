# 2 Hz PerfPro export of Moxy data

Exported from PerfPro Studio software, recorded at 0.5 Hz no smoothing
and exported at 2 Hz. Containing a ramp incremental cycling protocol,
placed on bilateral vastus lateralis muscle sites. Intentional data
errors (outliers, invalid values, and missing `NA` values) have been
introduced to demonstrate *mnirs* cleaning functions.

## Format

.xlsx file with five columns and 2202 rows:

- mm-dd:

  Recording date (dd-MMM format).

- hh:mm:ss:

  Time of day (hh:mm:ss format).

- SmO2 Live:

  Muscle oxygen saturation, left leg (%). Contains simulated erroneous
  and missing samples.

- SmO2 Live(2):

  Muscle oxygen saturation, right leg (%).

- Lap:

  Lap marker (integer).

Channel mapping for
[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md):

- `nirs_channels = c("SmO2 Live", "SmO2 Live(2)")`

- `time_channel = c("hh:mm:ss")`

- `event_channel = c("Lap")`

- `interval_times = list(start = c(204, 878))` (start and end of
  exercise)

## Source

Moxy Monitor (Fortiori Design LLC), exported via PerfPro Studio desktop
software (https://perfprostudio.com/).

## See also

[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md),
[`example_mnirs()`](https://jemarnold.github.io/mnirs/reference/example_mnirs.md)

## Examples

``` r
example_mnirs("moxy_ramp")
#> [1] "/home/runner/work/_temp/Library/mnirs/extdata/moxy_ramp.xlsx"
```
