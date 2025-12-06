# Custom *mnirs* colour palette

Custom *mnirs* colour palette

## Usage

``` r
palette_mnirs(n = NULL, names = NULL)
```

## Arguments

- n:

  A numeric vector specifying the number of colours to return.

- names:

  A character vector specifying colour names to return.

## Value

Named or unnamed character vector of hex colours.

## See also

[`theme_mnirs()`](https://jemarnold.github.io/mnirs/reference/theme_mnirs.md)
[`scale_colour_mnirs()`](https://jemarnold.github.io/mnirs/reference/scale_colour_mnirs.md)

## Examples

``` r
scales::show_col(palette_mnirs())

scales::show_col(palette_mnirs(n = 2))

scales::show_col(palette_mnirs(names = c("red", "orange")))
```
