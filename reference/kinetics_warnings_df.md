# Zero-row kinetics warnings scaffold

Stable column template for captured fit conditions, so binding and the
returned `warnings` element keep consistent columns when none fire.

## Usage

``` r
kinetics_warnings_df()
```

## Value

A zero-row `data.frame` with columns `interval`, `nirs_channels`,
`type`, and `message`.
