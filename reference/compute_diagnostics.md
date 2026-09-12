# Compute model diagnostics

Compute model diagnostics

## Usage

``` r
compute_diagnostics(x, t, fitted, n_params = 1L, env = rlang::caller_env())
```

## Arguments

- x:

  A numeric vector of the response variable.

- t:

  An *optional* numeric vector of the predictor variable (e.g. time).
  Default is `seq_along(x)`.

- fitted:

  A numeric vector of the predicted values.

- n_params:

  Integer; total number of estimated coefficients in the model (default
  `1L`). For linear models pass the number of regression coefficients
  (e.g. `2L` for `lm(x ~ t)`). For non-linear models
  (`"monoexponential"`, `"sigmoidal"`), pass the number of free
  parameters fit by the solver.

- env:

  The calling environment or a defused call, used to report errors and
  warnings as coming from the user-facing function rather than the
  validator.

## Value

A 1-row `data.frame` with columns `n_obs`, `n_params`, `r2`, `adj_r2`,
`rmse`, `cv_rmse`, `snr`, `aic`, `aicc`, and `bic`.

## Details

### r2

Squared Pearson correlation between observed and fitted values. Equals
the classic `1 - SSres / SStot` for OLS linear fits (matches
`summary(lm)$r.squared`); a bounded `[0, 1]` pseudo-`R^2` for non-linear
fits such as `"monoexponential"` and `"sigmoidal"`.

### adj_r2

Adjusted `R^2` penalised by `n_params`. Appropriate for OLS linear
models; interpret with caution for non-linear fits.

### aic, aicc, bic

Information criteria derived from a Gaussian log-likelihood with the
maximum-likelihood residual variance `sigma_hat^2 = SSres / n_obs`. The
effective parameter count is `k = n_params + 1` (the `+1` accounts for
the estimated residual variance). Values match
[`stats::AIC()`](https://rdrr.io/r/stats/AIC.html) and
[`stats::BIC()`](https://rdrr.io/r/stats/AIC.html) for `lm` and `nls`
fits. `aicc` is the small-sample correction and is `NA` when
`n_obs - k - 1 <= 0`.
