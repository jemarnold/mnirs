# Re-scale data range

Expand or reduce the range (min and max values) of data channels to a
new amplitude/dynamic range, e.g. re-scale the range of NIRS data to
`c(0, 100)`.

## Usage

``` r
rescale_mnirs(
  data,
  nirs_channels = list(),
  range,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing time series data and
  metadata.

- nirs_channels:

  A [`list()`](https://rdrr.io/r/base/list.html) of character vectors
  indicating grouping structure of mNIRS channel names to operate on
  (see *Details*). Must match column names in `data` exactly. Retrieved
  from metadata if not defined explicitly.

  `list("A", "B", "C")`

  :   Will operate on each channel independently, losing the relative
      scaling between channels.

  `list(c("A", "B", "C"))`

  :   Will operate on all channels together, preserving the relative
      scaling between channels.

  `list(c("A", "B"), c("C", "D"))`

  :   Will operate on channels `A` & `B` in one group, and `C` & `D` in
      another group, preserving relative scaling within, but not between
      groups.

- range:

  A numeric vector in the form `c(min, max)`, indicating the range of
  output values to which data channels will be re-scaled.

- verbose:

  Logical. Default is `TRUE`. Will display or silence (if `FALSE`)
  warnings and information messages helpful for troubleshooting. A
  global default can be set via `options(mnirs.verbose = FALSE)`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of class *"mnirs"* with metadata available with
[`attributes()`](https://rdrr.io/r/base/attributes.html).

## Details

`nirs_channels = list()` can be used to group data channels (column
names) to preserve absolute or relative scaling.

- Channels grouped together in a vector (e.g. `list(c("A", "B"))`) will
  be re-scaled to a common range, and the relative scaling within that
  group will be preserved.

- Channels in separate list vectors (e.g. `list("A", "B")`) will be
  re-scaled independently, and relative scaling between groups will be
  lost.

- A single vector of channel names (e.g. `c("A", "B")`) will group
  channels together.

- Channels (columns) in `data` not explicitly defined in `nirs_channels`
  will be passed through untouched to the output data frame.

`nirs_channels` can be retrieved automatically from `data` of class
*"mnirs"* which has been processed with `{mnirs}`, if not defined
explicitly. This will default to returning all `nirs_channels` grouped
together, and should be defined explicitly for other grouping
arrangements.

## Examples

``` r
## read example data
data <- read_mnirs(
    file_path = example_mnirs("moxy_ramp"),
    nirs_channels = c(smo2_left = "SmO2 Live",
                      smo2_right = "SmO2 Live(2)"),
    time_channel = c(time = "hh:mm:ss"),
    verbose = FALSE
) |>
    rescale_mnirs(        ## un-grouped nirs channels to rescale separately 
        nirs_channels = list(smo2_left, smo2_right), 
        range = c(0, 100) ## rescale to a 0-100% functional exercise range
    )

data
#> # A tibble: 2,203 × 3
#>     time smo2_left smo2_right
#>    <dbl>     <dbl>      <dbl>
#>  1 0            54       78.2
#>  2 0.400        54       78.2
#>  3 0.960        54       78.2
#>  4 1.51         54       75.6
#>  5 2.06         54       75.6
#>  6 2.61         54       75.6
#>  7 3.16         54       75.6
#>  8 3.71         57       76.9
#>  9 4.26         57       76.9
#> 10 4.81         57       76.9
#> # ℹ 2,193 more rows

# \donttest{
    if (requireNamespace("ggplot2", quietly = TRUE)) {
        plot(data, time_labels = TRUE) +
            ggplot2::geom_hline(yintercept = c(0, 100), linetype = "dotted")
    }

# }
```
