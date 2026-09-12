# Validate fixed model parameters

Validates the `fix` argument of parametric
[`analyse_kinetics()`](https://jemarnold.github.io/mnirs/reference/analyse_kinetics.md)
methods: a named list of finite numeric scalars whose names match the
model's fixable parameters. At least one parameter must remain free.

## Usage

``` r
validate_fix(fix, params, env = rlang::caller_env())
```

## Arguments

- fix:

  A named list of model parameters to hold constant, or `NULL`.

- params:

  Character vector of fixable parameter names for the model.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

`fix` as a named list; an empty list when `NULL`.
