# Re-scale Data Range

Expand or reduce the range (min and max values) of data channels to a
new amplitude/dynamic range, e.g. re-scale the range of NIRS data to
`c(0, 100)`.

## Usage

``` r
rescale_mnirs(data, nirs_channels = list(), range, verbose = TRUE)
```

## Arguments

- data:

  A data frame of class *"mnirs"* containing at least one column with
  numeric time or sample values, and one column with numeric mNIRS
  values, along with metadata.

- nirs_channels:

  A [`list()`](https://rdrr.io/r/base/list.html) of character vectors
  indicating the column names for data channels to be re-scaled (see
  *Details*).

  `list("A", "B", "C")`

  :   Will re-scale each channel independently, losing the relative
      scaling between channels.

  `list(c("A", "B", "C"))`

  :   Will re-scale all channels together, preserving the relative
      scaling between channels.

  `list(c("A", "B"), c("C", "D"))`

  :   Will re-scale channels `A` and `B` in one group, and channels `C`
      and `D` in another group, preserving relative scaling within, but
      not between groups.

  Must match column names in data exactly. Will be taken from metadata
  if not defined explicitly.

- range:

  A numeric vector in the form `c(min, max)`, indicating the range of
  output values to which data channels will be re-scaled.

- verbose:

  A logical to return (the *default*) or silence warnings and messages
  which can be used for data error checking. Abort errors will always be
  returned.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of class *"mnirs"* with metadata available with
[`attributes()`](https://rdrr.io/r/base/attributes.html).

## Details

`nirs_channels = list()` can be used to group data channels to preserve
absolute or relative scaling.

- Channels grouped together in a list item will be re-scaled to a common
  value, and the relative scaling within that group will be preserved.

- Channels grouped in separate list items will be re-scaled
  independently, and relative scaling between groups will be lost.

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
    nirs_channels = c(smo2 = "SmO2 Live"),
    time_channel = c(time = "hh:mm:ss"),
    verbose = FALSE
) |>
    resample_mnirs(verbose = FALSE) |>
    replace_mnirs(
        invalid_values = c(0, 100),
        outlier_cutoff = 3,
        width = 7,
        verbose = FALSE
    ) |>
    filter_mnirs(na.rm = TRUE, verbose = FALSE)

data_rescaled <- rescale_mnirs(
    data,
    # nirs_channels = NULL, ## taken from metadata
    range = c(0, 100),      ## rescale to a 0-100% functional exercise range
    verbose = FALSE
)

if (FALSE) { # \dontrun{
plot(data_rescaled, display_timestamp = TRUE) +
    geom_hline(yintercept = c(0, 100), linetype = "dotted")
} # }
```
