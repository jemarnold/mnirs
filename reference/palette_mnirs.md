# Custom *mnirs* colour palette

Custom *mnirs* colour palette

## Usage

``` r
palette_mnirs(...)
```

## Arguments

- ...:

  Either a single numeric specifying the number of colours to return, or
  character strings specifying colour names. If empty, all colours are
  returned.

## Value

Named (when selecting by name) or unnamed character vector of hex
colours.

## See also

[`theme_mnirs()`](https://jemarnold.github.io/mnirs/reference/theme_mnirs.md),
[`scale_colour_mnirs()`](https://jemarnold.github.io/mnirs/reference/scale_colour_mnirs.md)

## Examples

``` r
scales::show_col(palette_mnirs())

scales::show_col(palette_mnirs(2))

scales::show_col(palette_mnirs("red", "orange"))
```
