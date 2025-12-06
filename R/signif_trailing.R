#' Format numbers for display as character strings
#'
#' `signif_trailing()` converts numeric values to character strings to
#' preserve trailing zeroes
#'
#' @param x A numeric vector.
#' @param digits An integer specifying the number of *decimal places* or
#'   *significant figures* to preserve. Negative `digits` values will round to
#'   the nearest whole value of `10^(digits)`.
#' @param format Indicates how to treat `digits`. Either the desired number of
#'   decimal places (`format = "digits"`, the *default*) or significant figures
#'   after the decimal place (`format = "signif"`).
#'
#' @returns `signif_trailing()` returns a character vector of formatted numbers
#'   the same length as `x`.
#'
#' @details
#' `signif_trailing()`
#'
#' - Negative `digits` round to the respective integer place, e.g.
#'   `signif_trailing(123, digits = -1)` returns `"120"`.
#'
#' @seealso [formatC()] [round()] [signif()]
#'
#' @examples
#' x <- c(123, 123.4, 123.45)
#'
#' mnirs:::signif_trailing(x, digits = 1, format = "digits")
#' mnirs:::signif_trailing(x, digits = -1, format = "digits")
#' mnirs:::signif_trailing(x, digits = 5, format = "signif")
#' mnirs:::signif_trailing(x, digits = 0, format = "signif")
#'
#' @rdname signif_trailing
#' @order 1
#' @keywords internal
signif_trailing <- function(x, digits = 2L, format = c("digits", "signif")) {
    format <- match.arg(format)

    if (format == "digits") {
        validate_numeric(x)
        validate_numeric(digits, 1, c(-Inf, Inf), FALSE, TRUE)
        formatC_x <- round(x, digits)
        formatC_format <- "f"
    } else {
        ## if whole digits >= sig figs, return rounded whole number
        ## x & digits already validated
        formatC_x <- signif_whole(x, digits)
        formatC_format <- "fg"
    }

    result <- formatC(
        x = formatC_x,
        digits = max(0, digits),
        format = formatC_format,
        flag = "#"
    )

    ## remove trailing `.` or "NA" https://stackoverflow.com/a/35280610/15389854
    return(gsub("\\.$|NA| ", "", result))
}


#' Round numbers to significant figures or whole value
#'
#' `signif_whole()` rounds numeric values to a specified number of significant
#' figures, or the nearest whole value if the number of digits of `x` are
#' greater than `digits`.
#'
#' @returns `signif_whole()` returns a numeric vector the same length as `x`.
#'
#' @details
#' Decimal rounding is based on the "banker's rounding" default behaviour of
#'   `signif()` and `round()`, where `signif(123.45, 4)` or `round(123.45, 1)`
#'   each return `123.4`.
#'
#' `signif_whole()`
#'
#' - Negative `digits` round to the nearest whole value as if `digits = 0`,
#'   e.g. `signif_whole(123, digits = -5)` still returns `123`.
#'
#' @examples
#' x <- c(123, 123.4, 123.45)
#'
#' mnirs:::signif_whole(x, digits = 5)
#' mnirs:::signif_whole(x, digits = 2)
#' mnirs:::signif_whole(x, digits = -1)
#'
#' @rdname signif_trailing
#' @order 2
#' @keywords internal
signif_whole <- function(x, digits = 5L) {
    validate_numeric(x)
    validate_numeric(digits, 1, c(-Inf, Inf), FALSE, TRUE)

    ## if whole digits >= sig figs, return rounded whole number
    whole_digits <- floor(log10(abs(x))) + 1
    whole_digits[!is.finite(whole_digits)] <- 1 ## handle 0, NA, Inf
    should_round <- whole_digits >= digits

    ## vectorised ifelse evaluation
    return(ifelse(should_round, round(x), signif(x, digits)))
}


#' Format p-values for display
#'
#' `signif_pvalue()` displays p-values as either formatted numeric strings
#' or significance symbols.
#'
#' @param display Specifies output type, either *"value"* (the *default*) for
#'   formatted numbers or *"symbol"* for significance symbols.
#' @param symbol Character string specifying the significance symbol.
#'   *Default* is "*".
#' @param symbol_repeat Logical indicating whether to repeat symbols for
#'   different significance levels. Default is *FALSE*.
#' @param alpha A numeric value specifying significance threshold.
#'   *Default* is `0.05`.
#'
#' @returns `signif_pvalue()` returns a character vector of formatted p-values
#'   or significance symbols the same length as `x`.
#'
#' @details
#' `signif_pvalue()`
#'
#' - When `display = "value"` and e.g. `digits = 3`, `x` will be either
#'   rounded to 3 decimal places with `signif_trailing()`, or appear as e.g.
#'   *"< 0.001"*.
#' - `digits = 1` will display *"less than `alpha`"*, e.g. *"< 0.05"*.
#' - When `display = "symbol"`, if `symbol_repeat = TRUE`: Uses repeated
#'   symbols based on thresholds
#'   `(0.001 = "***", 0.01 = "**", alpha = "*", ns = "")`.
#' - If `symbol_repeat = FALSE`: Shows one symbol `"*"` for p < alpha,
#'   otherwise empty string.
#'
#' @examples
#' p_vals <- c(0.0001, 0.003, 0.02, 0.08, 0.15)
#'
#' ## format as numeric
#' mnirs:::signif_pvalue(p_vals)
#' mnirs:::signif_pvalue(p_vals, digits = 1)
#'
#' ## format as symbols
#' mnirs:::signif_pvalue(p_vals, display = "symbol")
#' mnirs:::signif_pvalue(p_vals, display = "symbol", symbol_repeat = TRUE)
#'
#' ## custom alpha and symbol
#' mnirs:::signif_pvalue(p_vals, display = "symbol", alpha = 0.01, symbol = "†", symbol_repeat = TRUE)
#'
#' @rdname signif_trailing
#' @order 3
#' @keywords internal
signif_pvalue <- function(
    x,
    digits = 3L,
    format = c("digits", "signif"),
    display = c("value", "symbol"),
    symbol = "*",
    symbol_repeat = FALSE,
    alpha = 0.05
) {
    format <- match.arg(format)
    display <- match.arg(display)
    validate_numeric(x)
    validate_numeric(digits, 1, c(0, Inf), FALSE, TRUE)
    validate_numeric(alpha, 1, c(0, 1), FALSE, msg = "probability")

    if (display == "symbol" && symbol_repeat) {
        return(strrep(symbol, 3L - findInterval(x, c(0.001, 0.01, alpha))))
    }
    if (display == "symbol") {
        return(ifelse(x >= alpha, "", symbol))
    }

    threshold <- 10^-digits
    if (digits == 1) {
        digits <- -floor(log10(alpha))
        threshold <- alpha
    }

    ifelse(
        x < threshold,
        sprintf("< %.*f", digits, threshold),
        signif_trailing(x, digits, format)
    )
}


#' Generate numeric sequence from range of a vector
#'
#' Creates a numeric sequence spanning the range of input data with specified
#' step size and direction. Optionally rounds the range endpoints before
#' generating the sequence.
#'
#' @param x A numeric vector.
#' @param by A numeric step size for the sequence. *Default* is `1`.
#' @param direction A character string specifying sequence direction. Either
#'   *"up"* (the *default*) for ascending or *"down"* for descending sequence.
#' @param digits An integer specifying number of decimal places to round range
#'   endpoints, or `NA` (the *default*) for no rounding.
#'
#' @returns A numeric vector spanning the range of the input data.
#'
#' @details
#' The output vector will likely be a different length than the input vector.
#'
#' The function:
#' - Calculates the range of `x` (ignoring NA values).
#' - Optionally rounds the range endpoints if `digits` is specified.
#' - Generates a sequence with the specified step size.
#' - Reverses the sequence if `direction = "down"`.
#'
#' @examples
#' data <- c(1.2, 3.7, 2.1, 4.9, 2.8)
#' mnirs:::seq_range(data)
#' mnirs:::seq_range(data, by = 0.5)
#' mnirs:::seq_range(data, by = 0.5, direction = "down")
#'
#' ## with rounding
#' mnirs:::seq_range(data, by = 0.2, digits = 1)
#'
#' @seealso [seq()], [range()]
#'
#' @keywords internal
seq_range <- function(x, by = 1, direction = c("up", "down"), digits = NA) {
    direction <- match.arg(direction)
    validate_numeric(x)
    validate_numeric(by, 1)

    x_range <- range(x, na.rm = TRUE)
    if (!is.na(digits)) {
        validate_numeric(digits, 1)
        x_range <- round(x_range, digits)
    }

    sequence <- seq(x_range[1L], x_range[2L], by = by)
    if (direction == "down") {
        return(rev(sequence))
    } else {
        return(sequence)
    }
}


#' Wrap vector elements
#'
#' Rotates vector elements by moving the first `n` elements to the end.
#'
#' @param x A vector.
#' @param n An integer specifying number of elements to move from start to end.
#'   *Default* is `0` (no wrapping). Negative values move elements from end
#'   to start.
#'
#' @returns A vector with all the same elements as `x`.
#'
#' @details
#' The function:
#' - Returns `x` unchanged if `n = 0`.
#' - Moves first `n` elements to the end of the vector.
#' - For negative `n`, effectively moves elements from end to start.
#' - If `n` is larger than `length(x)`, positions wrap around.
#'
#' @examples
#' x <- 1:5
#' mnirs:::wrap(x, 2)
#' mnirs:::wrap(x, 0)
#'
#' ## wrapping with n larger than vector length
#' mnirs:::wrap(x, 7)  ## same as wrap(x, 2)
#'
#' ## negative wrapping (move from end to start)
#' mnirs:::wrap(x, -1)
#'
#' ## works with any vector type
#' mnirs:::wrap(letters[1:4], 1)
#'
#' @keywords internal
wrap <- function(x, n = 0L) {
    validate_numeric(n, 1, integer = TRUE)

    if (length(x) == 0L) {
        return(x)
    }

    n <- n %% length(x)
    if (n == 0L) {
        return(x)
    }

    return(c(x[-(1:n)], x[1:n]))
}
