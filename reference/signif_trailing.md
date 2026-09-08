# Format numbers for display as character strings

`signif_trailing()` converts numeric values to character strings to
preserve trailing zeroes

`signif_whole()` rounds numeric values to a specified number of
significant figures, or the nearest whole value if the number of digits
of `x` are greater than `digits`.

`signif_pvalue()` displays p-values as either formatted numeric strings
or significance symbols.

## Usage

``` r
signif_trailing(x, digits = 2L, format = c("digits", "signif"), trim = TRUE)

signif_whole(x, digits = 5L)

signif_pvalue(
  x,
  digits = 3L,
  format = c("digits", "threshold"),
  display = c("value", "symbol"),
  symbol = "*",
  symbol_repeat = FALSE,
  alpha = 0.05
)
```

## Arguments

- x:

  A numeric vector.

- digits:

  An integer specifying the number of *decimal places* or *significant
  figures* to preserve. Negative `digits` values will round to the
  nearest whole value of `10^(digits)`.

- format:

  Indicates how to treat `digits`. Either the desired significance
  criteria over which to display the absolute p value
  (`format = "digits"`, the *default*), or the smallest significance
  criteria to print as less than (`format = "signif"`).

- trim:

  Logical; if `TRUE` (the *default*), caps `digits` at the number of
  decimal places or significant figures observed in `x`. If `FALSE`,
  uses the exact `digits` value.

- display:

  Specifies output type, either *"value"* (the *default*) for formatted
  numbers or *"symbol"* for significance symbols.

- symbol:

  Character string specifying the significance symbol. *Default* is
  "\*".

- symbol_repeat:

  Logical indicating whether to repeat symbols for different
  significance levels. Default is *FALSE*.

- alpha:

  A numeric value specifying significance threshold. *Default* is
  `0.05`.

## Value

`signif_trailing()` returns a character vector of formatted numbers the
same length as `x`.

`signif_whole()` returns a numeric vector the same length as `x`.

`signif_pvalue()` returns a character vector of formatted p-values or
significance symbols the same length as `x`.

## Details

`signif_trailing()`

- Negative `digits` round to the respective integer place, e.g.
  `signif_trailing(123, digits = -1)` returns `"120"`.

Decimal rounding is based on the "banker's rounding" default behaviour
of [`signif()`](https://rdrr.io/r/base/Round.html) and
[`round()`](https://rdrr.io/r/base/Round.html), where
`signif(123.45, 4)` or `round(123.45, 1)` each return `123.4`.

`signif_whole()`

- Negative `digits` round to the nearest whole value as if `digits = 0`,
  e.g. `signif_whole(123, digits = -5)` still returns `123`.

`signif_pvalue()`

- When `format = "digits"` and e.g. `digits = 3`, `x` is rounded to 3
  decimal places, or shown as *"p \< 0.001"* below a 3-decimal place
  significance threshold.

- `digits = 1` with `format = "digits"` displays *"p \< `alpha`"*, e.g.
  *"p \< 0.05"*.

- When `format = "signif"`, `digits` sets the lowest threshold (e.g.
  `digits = 3` gives thresholds `alpha`, `0.01`, `0.001`). Values below
  `alpha` show the nearest threshold above them, e.g. `p = 0.04` gives
  *"p \< 0.05"*; `p = 0.009` gives *"p \< 0.01"*.

- When `display = "symbol"`, if `symbol_repeat = TRUE`: Uses repeated
  symbols based on thresholds
  `(0.001 = "***", 0.01 = "**", alpha = "*", ns = "")`.

- If `symbol_repeat = FALSE`: Shows one symbol `"*"` for p \< alpha,
  otherwise empty string.

## See also

[`formatC()`](https://rdrr.io/r/base/formatc.html),
[`round()`](https://rdrr.io/r/base/Round.html),
[`signif()`](https://rdrr.io/r/base/Round.html)
