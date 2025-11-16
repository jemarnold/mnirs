# Custom *mnirs* colour palette

Custom *mnirs* colour palette

## Usage

``` r
palette_mnirs(n = NULL)
```

## Arguments

- n:

  A character or numeric vector specifying either the name or the number
  in order of colours to return.

## Value

Named or unnamed character vector of hex colours.

## See also

[`theme_mnirs()`](https://jemarnold.github.io/mnirs/reference/theme_mnirs.md)
[`scale_colour_mnirs()`](https://jemarnold.github.io/mnirs/reference/scale_colour_mnirs.md)

## Examples

``` r
scales::show_col(palette_mnirs())

scales::show_col(palette_mnirs(2))

scales::show_col(palette_mnirs(c("red", "orange")))
```
