# Build per-panel kinetics marker and label annotations

Maps a fitted `mnirs_kinetics` method to its key coefficient markers
(`xval`, `yval`) and formatted label lines for
[`plot.mnirs_kinetics()`](https://jemarnold.github.io/mnirs/reference/plot.mnirs_kinetics.md).
Marker rows are one per `nirs_channel` per key point per interval, with
x-coordinates the resolved onset plus the method's time coefficient.
Label rows are one per label line, anchored (`xval = Inf`, `yval = -Inf`
or `Inf`) at the corner of the panel's right edge vacated by the
observed signal: the bottom corner when the median of all channels over
the right half of the interval sits above the y-axis midpoint, otherwise
the top. `vjust` stacks the lines inward from the corner in channel
order within each interval.

## Usage

``` r
kinetics_annotations(x, free_y = FALSE)
```

## Arguments

- x:

  An *"mnirs_kinetics"* object from
  [`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md).

- free_y:

  Logical. Default is `FALSE`; the y-axis midpoint spans all intervals,
  matching a shared facet axis. If `TRUE` (facet `scales = "free_y"` or
  `"free"`) each interval uses its own midpoint.

## Value

A `data.frame` with columns `interval`, `nirs_channels`, `xval`, `yval`,
`label`, and `vjust`. Marker rows have an empty `label` and `NA`
`vjust`; label rows have infinite `xval`/`yval`. Rows are annotated by
the model that fit them (the `model` coefficient column where the method
has a fallback chain, else the method). `NULL` for a method with no
annotation spec, in which case
[`plot.mnirs_kinetics()`](https://jemarnold.github.io/mnirs/reference/plot.mnirs_kinetics.md)
draws the fitted curve alone.
