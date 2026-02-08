# Package index

## Read data

Read raw data from exported files.

- [`read_mnirs()`](https://jemarnold.github.io/mnirs/reference/read_mnirs.md)
  :

  Read *mnirs* data from file

- [`example_mnirs()`](https://jemarnold.github.io/mnirs/reference/example_mnirs.md)
  :

  Get path to *mnirs* example files

- [`create_mnirs_data()`](https://jemarnold.github.io/mnirs/reference/create_mnirs_data.md)
  :

  Create an *mnirs* data frame with metadata

## Pre-process data

Clean, filter, and process data.

- [`replace_mnirs()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  [`replace_invalid()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  [`replace_outliers()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  [`replace_missing()`](https://jemarnold.github.io/mnirs/reference/replace_mnirs.md)
  : Replace outliers, invalid values, and missing values
- [`resample_mnirs()`](https://jemarnold.github.io/mnirs/reference/resample_mnirs.md)
  : Re-sample a data frame
- [`filter_mnirs()`](https://jemarnold.github.io/mnirs/reference/filter_mnirs.md)
  : Filter a data frame
- [`filter_butter()`](https://jemarnold.github.io/mnirs/reference/filter_butter.md)
  : Apply a Butterworth digital filter
- [`filter_moving_average()`](https://jemarnold.github.io/mnirs/reference/filter_moving_average.md)
  : Apply a moving average filter
- [`shift_mnirs()`](https://jemarnold.github.io/mnirs/reference/shift_mnirs.md)
  : Shift data range
- [`rescale_mnirs()`](https://jemarnold.github.io/mnirs/reference/rescale_mnirs.md)
  : Re-scale data range

## Interval detection

Detect and extract intervals for further analysis

- [`extract_intervals()`](https://jemarnold.github.io/mnirs/reference/extract_intervals.md)
  :

  Extract intervals from *mnirs* data

## Process kinetics

Estimate response dynamics and kinetic parameters

- [`peak_slope()`](https://jemarnold.github.io/mnirs/reference/peak_slope.md)
  : Find peak linear slope
- [`SS_monoexp3()`](https://jemarnold.github.io/mnirs/reference/SS_monoexp.md)
  [`SS_monoexp4()`](https://jemarnold.github.io/mnirs/reference/SS_monoexp.md)
  : Self-starting monoexponential models
- [`monoexponential()`](https://jemarnold.github.io/mnirs/reference/monoexponential.md)
  : Monoexponential function with 4 parameters

## Plotting

Tools for pretty plotting with [ggplot2](https://ggplot2.tidyverse.org).

- [`plot(`*`<mnirs>`*`)`](https://jemarnold.github.io/mnirs/reference/plot.mnirs.md)
  :

  Plot *mnirs* objects

- [`theme_mnirs()`](https://jemarnold.github.io/mnirs/reference/theme_mnirs.md)
  :

  Custom *mnirs* ggplot2 theme

- [`palette_mnirs()`](https://jemarnold.github.io/mnirs/reference/palette_mnirs.md)
  :

  Custom *mnirs* colour palette

- [`scale_colour_mnirs()`](https://jemarnold.github.io/mnirs/reference/scale_colour_mnirs.md)
  [`scale_color_mnirs()`](https://jemarnold.github.io/mnirs/reference/scale_colour_mnirs.md)
  [`scale_fill_mnirs()`](https://jemarnold.github.io/mnirs/reference/scale_colour_mnirs.md)
  :

  Scales for custom *mnirs* palette

- [`breaks_timespan()`](https://jemarnold.github.io/mnirs/reference/breaks_timespan.md)
  : Breaks for timespan data

- [`format_hmmss()`](https://jemarnold.github.io/mnirs/reference/format_hmmss.md)
  : Format timespan data as h:mm:ss

## Files

Example data files included in the package

- [`moxy_intervals.csv`](https://jemarnold.github.io/mnirs/reference/moxy_intervals.csv.md)
  : 0.5 Hz Moxy onboard export
- [`train.red_intervals.csv`](https://jemarnold.github.io/mnirs/reference/train.red_intervals.csv.md)
  : 10 Hz Train.Red App export
- [`vo2master.csv`](https://jemarnold.github.io/mnirs/reference/vo2master.csv.md)
  : 1 Hz VO2master app recording from Moxy
- [`artinis_intervals.xlsx`](https://jemarnold.github.io/mnirs/reference/artinis_intervals.xlsx.md)
  : 10 Hz Artinis Oxysoft export recorded with Oxymon MKIII
- [`moxy_ramp.xlsx`](https://jemarnold.github.io/mnirs/reference/moxy_ramp.xlsx.md)
  : 2 Hz PerfPro export of Moxy data
- [`portamon-oxcap.xlsx`](https://jemarnold.github.io/mnirs/reference/portamon-oxcap.xlsx.md)
  : 10 Hz Artinis Oxysoft export recorded with Portamon
