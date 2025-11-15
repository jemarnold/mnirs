# Package index

## Read data

Read raw data from exported files.

- [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md)
  : Read mNIRS Data From File

- [`example_mnirs()`](https://jemarnold.github.io/mnirs/reference/example_mnirs.md)
  :

  Get path to *mnirs* example files

- [`create_mnirs_data()`](https://jemarnold.github.io/mnirs/reference/create_mnirs_data.md)
  : Create an mNIRS data frame with Metadata

## Clean data

Clean, filter, and process data.

- [`replace_mnirs()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  [`replace_invalid()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  [`replace_outliers()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  [`replace_missing()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  : Replace outliers, invalid values, and missing values
- [`resample_mnirs()`](https://jemarnold.github.io/mnirs/reference/resample_mnirs.md)
  : Re-sample a Data Frame
- [`filter_butter()`](https://jemarnold.github.io/mnirs/reference/filter_butter.md)
  : Apply a Butterworth Digital Filter
- [`filter_mnirs(`*`<smooth_spline>`*`)`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md)
  [`filter_mnirs(`*`<butterworth>`*`)`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md)
  [`filter_mnirs(`*`<moving_average>`*`)`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md)
  : Filter Data
- [`filter_moving_average()`](https://jemarnold.github.io/mnirs/reference/filter_moving_average.md)
  : Apply a Moving Average Filter
- [`shift_mnirs()`](https://jemarnold.github.io/mnirs/reference/shift_mnirs.md)
  : Shift Data Range
- [`rescale_mnirs()`](https://jemarnold.github.io/mnirs/reference/rescale_mnirs.md)
  : Re-scale Data Range

## Plotting

Tools for pretty plotting.

- [`plot(`*`<mnirs>`*`)`](https://jemarnold.github.io/mnirs/reference/plot.mnirs.md)
  : Plot mnirs objects
- [`theme_mnirs()`](https://jemarnold.github.io/mnirs/reference/theme_mnirs.md)
  : Custom mNIRS ggplot2 theme
- [`palette_mnirs()`](https://jemarnold.github.io/mnirs/reference/palette_mnirs.md)
  : Custom mNIRS colour palette
- [`scale_colour_mnirs()`](https://jemarnold.github.io/mnirs/reference/scale_colour_mnirs.md)
  [`scale_color_mnirs()`](https://jemarnold.github.io/mnirs/reference/scale_colour_mnirs.md)
  [`scale_fill_mnirs()`](https://jemarnold.github.io/mnirs/reference/scale_colour_mnirs.md)
  : Scales for custom mNIRS palette
- [`breaks_timespan()`](https://jemarnold.github.io/mnirs/reference/breaks_timespan.md)
  : Breaks for Timespan Data
- [`format_hmmss()`](https://jemarnold.github.io/mnirs/reference/format_hmmss.md)
  : Format Timespan Data as h:mm:ss

## Data

Example data files included in the package

- [`moxy_intervals.csv`](https://jemarnold.github.io/mnirs/reference/moxy_intervals.csv.md)
  : 0.5 Hz Moxy onboard export
- [`moxy_ramp.xlsx`](https://jemarnold.github.io/mnirs/reference/moxy_ramp.xlsx.md)
  : 2 Hz PerfPro export of Moxy data
- [`train.red_intervals.csv`](https://jemarnold.github.io/mnirs/reference/train.red_intervals.csv.md)
  : 10 Hz Train.Red App export
- [`artinis_intervals.xlsx`](https://jemarnold.github.io/mnirs/reference/artinis_intervals.xlsx.md)
  : 10 Hz Artinis Oxysoft export
