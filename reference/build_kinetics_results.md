# Gather per-interval `mnirs_kinetics` into results structure

Shared helper for `analyse_kinetics.*` methods. Takes a list of
per-interval (per-data frame) kinetics results data frames (each
carrying `"fitted_data"`, `"channel_args"`, and `"diagnostics"`
attributes) and the original `data_list`. Interval names are taken from
`names(data_list)`. Where rows carry a `model` column, coefficient
columns owned only by fallback models no row resolved to are dropped.

## Usage

``` r
build_kinetics_results(data_list, result_list, method, call)
```

## Arguments

- data_list:

  Named list of original interval data frames.

- result_list:

  List of per-interval result data frames with attributes.

## Value

A named list with: `method`, `model`, `coefficients`, `data`,
`interval_times`, `diagnostics`, `channel_args`, `warnings`, `call`.
