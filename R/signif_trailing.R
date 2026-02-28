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
#' @details
#' `signif_trailing()`
#'
#' - Negative `digits` round to the respective integer place, e.g.
#'   `signif_trailing(123, digits = -1)` returns `"120"`.
#'
#' @returns `signif_trailing()` returns a character vector of formatted numbers
#'   the same length as `x`.
#'
#' @seealso [formatC()], [round()], [signif()]
#'
#' @rdname signif_trailing
#' @order 1
#' @keywords internal
signif_trailing <- function(
    x,
    digits = 2L,
    format = c("digits", "signif", "max_digits", "max_signif")
) {
    format <- match.arg(format)

    if (format == "max_digits") {
        validate_numeric(x)
        validate_numeric(digits, 1, c(0, Inf), integer = TRUE)
        digits <- min(digits, count_max_decimals(x))
        format <- "digits"
    }

    if (format == "max_signif") {
        validate_numeric(x)
        validate_numeric(digits, 1, c(0, Inf), FALSE, TRUE)
        digits <- min(digits, count_max_sigfigs(x))
        format <- "signif"
    }

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

    ## remove trailing `.` or "NA"
    return(gsub("\\.$|NA| ", "", result))
}


#' Count maximum decimal places across a numeric vector
#'
#' Returns the largest number of decimal places present in any finite,
#' non-NA element of `x`. Used internally by `signif_trailing()` for
#' `format = "max_digits"`.
#'
#' @param x A numeric vector.
#'
#' @returns A single non-negative integer.
#'
#' @keywords internal
count_max_decimals <- function(x) {
    ## handle non-finite elements where nchar shouldn't apply
    x <- x[is.finite(x)]
    if (length(x) == 0L) {
        return(0L)
    }

    ## format with enough precision to capture true decimal places
    txt <- format(x, scientific = FALSE, trim = TRUE)
    decimals <- vapply(strsplit(txt, ".", fixed = TRUE), \(.s) {
        if (length(.s) < 2L) 0L else nchar(.s[[2L]])
    }, integer(1))

    return(max(decimals))
}


#' Count maximum significant figures across a numeric vector
#'
#' Returns the largest number of significant figures present in any finite,
#' non-NA element of `x`. Used internally by `signif_trailing()` for
#' `format = "max_signif"`.
#'
#' @param x A numeric vector.
#'
#' @returns A single positive integer (minimum 1).
#'
#' @keywords internal
count_max_sigfigs <- function(x) {
    ## handle non-finite elements where nchar shouldn't apply
    x <- x[is.finite(x) & x != 0]
    if (length(x) == 0L) {
        return(1L)
    }

    ## format with enough precision, strip sign and leading/trailing zeros
    txt <- format(abs(x), scientific = FALSE, trim = TRUE)
    sigfigs <- vapply(txt, \(.s) {
        ## remove decimal point
        .s <- gsub(".", "", .s, fixed = TRUE)
        ## remove leading zeros
        .s <- sub("^0+", "", .s)
        ## remove trailing zeros only for values without a decimal
        ## (integers represented exactly)
        nchar(.s)
    }, integer(1), USE.NAMES = FALSE)

    return(max(sigfigs))
}


#' Round numbers to significant figures or whole value
#'
#' `signif_whole()` rounds numeric values to a specified number of significant
#' figures, or the nearest whole value if the number of digits of `x` are
#' greater than `digits`.
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
#' @returns `signif_whole()` returns a numeric vector the same length as `x`.
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
#' @returns `signif_pvalue()` returns a character vector of formatted p-values
#'   or significance symbols the same length as `x`.
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
    validate_numeric(
        alpha, 1, c(0, 1), FALSE, 
        msg1 = "one-element",
        msg2 = " between {col_blue('[0, 1]')}"
    )

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
#' @details
#' The output vector will likely be a different length than the input vector.
#'
#' The function:
#' - Calculates the range of `x` (ignoring NA values).
#' - Optionally rounds the range endpoints if `digits` is specified.
#' - Generates a sequence with the specified step size.
#' - Reverses the sequence if `direction = "down"`.
#'
#' @returns A numeric vector spanning the range of the input data.
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
#' @details
#' The function:
#' - Returns `x` unchanged if `n = 0`.
#' - Moves first `n` elements to the end of the vector.
#' - For negative `n`, effectively moves elements from end to start.
#' - If `n` is larger than `length(x)`, positions wrap around.
#'
#' @returns A vector with all the same elements as `x`.
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


#' Detect if numeric values fall within range of a vector
#'
#' Vectorised check for `x %in% vec`, inclusive or exclusive of left and right
#' boundary values, specified independently.
#'
#' @param x A numeric vector.
#' @param vec A numeric vector from which `left` and `right` boundary values
#'   for `x` will be taken.
#' @param inclusive A character vector to specify which of `left` and/or
#'   `right` boundary values should be included in the range, or both (the
#'   default), or excluded if `FALSE`.
#'
#' @details
#' `inclusive = FALSE` can be used to test for positive non-zero values:
#'   `within(x, c(0, Inf), inclusive = FALSE)`.
#'
#' @returns A logical vector the same length as `x`.
#'
#' @seealso [dplyr::between()]
#'
#' @keywords internal
within <- function(x, vec, inclusive = c("left", "right")) {
    if (!is.numeric(x)) {
        abort_validation(substitute(x))
    }
    if (!is.numeric(vec)) {
        abort_validation(substitute(vec))
    }
    inclusive <- match.arg(
        as.character(inclusive),
        choices = c("left", "right", "FALSE"),
        several.ok = TRUE
    )

    ## extract bounds from vec
    left <- min(vec, na.rm = TRUE)
    right <- max(vec, na.rm = TRUE)

    if ("FALSE" %in% inclusive) {
        return(x > left & x < right)
    }

    left_op <- if ("left" %in% inclusive) `>=` else `>`
    right_op <- if ("right" %in% inclusive) `<=` else `<`

    return(left_op(x, left) & right_op(x, right))
}
