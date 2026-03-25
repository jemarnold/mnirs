# 0.5 Hz Moxy onboard export

Exported from Moxy onboard recording at 0.5 Hz no smoothing. Containing
four 4-minute cycling work intervals, placed on the vastus lateralis
muscle site.

## Format

.csv file with seven columns and 936 rows:

- mm-dd:

  Recording date (dd-MMM format).

- hh:mm:ss:

  Recording time of day (hh:mm:ss format).

- SmO2 Live:

  Muscle oxygen saturation, raw signal (%).

- SmO2 Averaged:

  Muscle oxygen saturation, rolling average (%).

- THb:

  Total haemoglobin (arbitrary units).

- Lap:

  Lap marker (integer). Not typically in use.

- Session Ct:

  Session count of recordings.

Channel mapping for
[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md):

- `nirs_channels = c("SmO2 Live", "SmO2 Averaged", "THb")`

- `time_channel = c("hh:mm:ss")`

- `interval_times = list(start = c(124, 486, 848, 1210), end = c(364, 726, 1088, 1450))`

## Source

Moxy Monitor (Fortiori Design LLC), exported via Moxy Portal App.
(https://www.moxymonitor.com/)

## See also

[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md),
[`example_mnirs()`](https://jemarnold.github.io/mnirs/reference/example_mnirs.md)

## Examples

``` r
example_mnirs("moxy_intervals")
#> [1] "/home/runner/work/_temp/Library/mnirs/extdata/moxy_intervals.csv"
```
