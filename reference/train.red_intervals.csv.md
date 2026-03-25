# 10 Hz Train.Red App export

Exported from Train.Red app, recorded at 10 Hz. Containing two 5-minute
cycling work intervals, placed on bilateral vastus lateralis muscle
sites. Some data channels have been omitted to reduce file size.

## Format

.csv file with header metadata and 10 columns and 12000 rows:

- Timestamp (seconds passed):

  Elapsed time (s).

- Lap/Event:

  Lap number (numeric).

- SmO2:

  Muscle oxygen saturation, filtered (%). Two channels have duplicated
  names. If both are called, the second will be renamed to `SmO2_1`.

- SmO2 unfiltered:

  Muscle oxygen saturation, raw signal (%). Two channels have duplicated
  names. If both are called, the second will be renamed to
  `SmO2 unfiltered_1`.

- O2HB unfiltered:

  Oxyhaemoglobin concentration, raw signal (arbitrary units). Two
  channels have duplicated names. If both are called, the second will be
  renamed to `O2HB unfiltered_1`.

- HHB unfiltered:

  Deoxyhaemoglobin concentration, raw signal (arbitrary units). Two
  channels have duplicated names. If both are called, the second will be
  renamed to `HHb unfiltered_1`.

Channel mapping for
[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md):

- `nirs_channels = c("SmO2", "SmO2 unfiltered", "O2HB unfiltered", "HHb unfiltered")`

- `time_channel = c("Timestamp (seconds passed)")`

- `event_channel = c("Lap/Event")`

- `interval_times = list(start = c(2150.09, 2865.05), end = c(2441.06, 3168.08))`

- `interval_times = list(start = c(65.94, 780.90), end = c(356.91, 1183.93))`
  from zero_time

## Source

Train.Red (Train.Red B.V.), exported via Train.Red app
(https://train.red/)

## See also

[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md),
[`example_mnirs()`](https://jemarnold.github.io/mnirs/reference/example_mnirs.md)

## Examples

``` r
example_mnirs("train.red")
#> [1] "/home/runner/work/_temp/Library/mnirs/extdata/train.red_intervals.csv"
```
