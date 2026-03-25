# 10 Hz Artinis Oxysoft export recorded with Portamon

Exported from Artinis Oxysoft, recorded on Portamon at 10 Hz on the
vastus lateralis muscle. Containing two trials of repeated occlusion
oxidative capacity testing, each with 17 occlusions.

## Format

.xlsx file with header metadata and six columns and 7943 rows:

- Column 1:

  Sample index (divide by sample rate for seconds).

- Column 2:

  tHb: total haemoglobin concentration change (\\\mu\\M).

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

- `nirs_channels = c(THb = 2, HHb = 3, O2Hb = 4)`

- `time_channel = c(sample = 1)`

- `event_channel = c(event = 5, label = "col_6")`

## Source

Artinis Medical Systems. Portamon, exported via Oxysoft desktop software
(https://www.artinis.com/)

## See also

[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md),
[`example_mnirs()`](https://jemarnold.github.io/mnirs/reference/example_mnirs.md)

## Examples

``` r
example_mnirs("portamon")
#> [1] "/home/runner/work/_temp/Library/mnirs/extdata/portamon-oxcap.xlsx"
```
