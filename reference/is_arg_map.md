# Classify a per-channel/per-interval argument map

An argument is a map when it is a
[`list()`](https://rdrr.io/r/base/list.html) with at least one named
element and at most one unnamed element (the fallback for unlisted
keys). Shared by
[`resolve_channel_args()`](https://jemarnold.github.io/mnirs/reference/resolve_channel_args.md).

## Usage

``` r
is_arg_map(x)
```

## Arguments

- x:

  An argument value.

## Value

A logical scalar.
