#' Calculate linear slope
#'
#' Calculates the linear regression slope of a numeric vector.
#'
#' @param na.rm A logical indicating whether missing values should be ignored
#'   (`TRUE`). Otherwise `FALSE` (the *default*) will return `NA` when there
#'   are any missing data within the vector.
#' @inheritParams replace_invalid
#'
#' @details
#' Uses the least squares formula. When `na.rm = TRUE` uses complete case
#'   analysis, where at least two valid samples will return a slope value.
#'   Otherwise, a single `NA` sample will return `NA`.
#'
#' @returns A numeric slope of `x/t`.
#'
#' @examples
#' x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
#' slope(x)
#'
#' x <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, NA)
#' slope(x)
#'
#' @export
slope <- function(x, t = seq_along(x), na.rm = FALSE, bypass_checks = FALSE) {
    if (!bypass_checks) {
        validate_x_t(x, t)
        if (na.rm) {
            complete <- !is.na(x) & !is.na(t)
            x <- x[complete]
            t <- t[complete]
        }
    }
    n <- length(x)
    if (n < 2L) {
        return(NA_real_)
    }
    sum_t <- sum(t)
    sum_x <- sum(x)
    sum_tx <- sum(t * x)
    sum_t2 <- sum(t^2)
    denom <- n * sum_t2 - sum_t^2 ## should not cause integer overflow
    if (is.na(denom) || denom == 0) {
        return(NA_real_)
    }
    return((n * sum_tx - sum_t * sum_x) / denom)
}

#' Calculate rolling slope
#'
#' Computes rolling linear regression slopes within a local window along a
#' numeric vector.
#'
#' @param align Window alignment as *"center"* (the *default*), *"left"*, or
#'   *"right"*. Where *"left"* is *forward looking*, and *"right"* is
#'   *backward looking* from the current sample.
#' @inheritParams slope
#' @inheritParams replace_mnirs
#'
#' @details
#' Uses the least squares formula. When `na.rm = TRUE` uses complete case
#'   analysis, where at least two valid samples will return a slope value.
#'   Otherwise, a single `NA` sample will return `NA`.
#'
#' The local rolling window can be specified by either `width` as the number of
#'   samples centred on `idx` between
#'   `[idx - floor(width/2), idx + floor(width/2)]`, or `span` as the timespan
#'   in units of `time_channel` centred on `idx` between
#'   `[t - span/2, t + span/2]`. Specifying `width` calls [roll::roll_lm()]
#'   which is often much faster than specifying `span`. A partial moving
#'   average will be calculated at the edges of the data.
#'
#' @seealso [zoo::rollapply()], [roll::roll_lm()]
#'
#' @return A numeric vector of rolling local slopes in units of `x/t` the
#'   same length as `x`.
#'
#' @examples
#' x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
#' rolling_slope(x, span = 3)
#' rolling_slope(x, width = 3)
#'
#' x_na <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
#' rolling_slope(x_na, span = 3)
#' rolling_slope(x_na, span = 3, na.rm = TRUE)
#'
#' @export
rolling_slope <- function(
    x,
    t = seq_along(x),
    width = NULL,
    span = NULL,
    align = c("center", "left", "right"),
    na.rm = FALSE,
    bypass_checks = FALSE,
    verbose = TRUE
) {
    if (!bypass_checks) {
        validate_x_t(x, t)
        if (na.rm) {
            complete <- !is.na(x) & !is.na(t)
            x <- x[complete]
            t <- t[complete]
        }
        if (length(x) < 2L) {
            return(NA_real_)
        }
        validate_width_span(width, span, verbose)
        align <- match.arg(align)
    }

    ## use {roll} for fast rolling ==================================
    if (!is.null(width) && is.null(span)) {
        rlang::check_installed(
            c("roll", "RcppParallel"),
            reason = "to use fast rolling functions"
        )
        if (rlang::is_installed("roll")) {
            return(roll_lm_centred(x, t, width))
        }
    }

    ## process =====================================================
    window_idx <- compute_local_windows(
        t, width = width, span = span, align = align
    )
    compute_local_fun(
        x, window_idx, slope, na.rm = na.rm, bypass_checks = TRUE
    )
}

## ! add align parameter to roll_* functions
