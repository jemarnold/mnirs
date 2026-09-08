# 1 Hz PIONIRS NIRSBOX-DUAL export

Exported from PIONIRS software at 1 Hz, two channels. Containing
baseline, arterial occlusion, and recovery phases marked by event tags,
from the thenar eminence (CH1) and medial forearm (CH2).

## Format

tab-separated .ftn2 file with 26 columns and 700 rows. Each signal is
duplicated for channel 1 `(CH1)` and channel 2 `(CH2)`:

- Iteration:

  Sample index.

- Time:

  Elapsed time (seconds).

- uA_L1, uA_L2:

  Absorption coefficient at wavelengths 1 (685 nm) and 2 (830 nm;
  cm^-1).

- uS_L1, uS_L2:

  Reduced scattering coefficient at wavelengths 1 and 2 (cm^-1).

- DPF_L1, DPF_L2:

  Differential pathlength factor at wavelengths 1 and 2 (\\\mu\\M).

- O2Hb:

  Oxyhaemoglobin concentration (\\\mu\\M).

- HHb:

  Deoxyhaemoglobin concentration (\\\mu\\M).

- THb:

  Total haemoglobin concentration (\\\mu\\M).

- StO2:

  Tissue oxygen saturation (%).

- DQI:

  Data quality index (0-1).

- Tag:

  Event marker (integer). `0` - no tag; `1` - manual tag; `2` -
  automatic tag from external trigger; `3` - automatic protocol-
  specific tag from the measurement software

- TagLabel:

  Event label text.

Channels are detected automatically, or can be specified explicitly for
[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md):

- `nirs_channels = c("StO2(CH1)", "StO2(CH2)")`

- `time_channel = c("Time")`

- `event_channel = c("TagLabel")`

- `interval_times = list(start = 91, end = 391)`

## Source

PIONIRS S.r.l. (https://www.pionirs.com/)

## See also

[`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md),
[`example_mnirs()`](https://jemarnold.github.io/mnirs/reference/example_mnirs.md)

## Examples

``` r
example_mnirs("pionirs")
#> [1] "/home/runner/work/_temp/Library/mnirs/extdata/pionirs_occlusion.ftn2"
```
