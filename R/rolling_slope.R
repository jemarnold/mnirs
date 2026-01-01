#' Calculate linear slope
#'
#' Calculates the linear regression slope of a numeric vector.
#'
#' @param na.rm A logical indicating whether missing values should be ignored
#'   (`TRUE`). Otherwise `FALSE` (the *default*) will return `NA` when there
#'   are any missing data within the vector.
#' @param ... Additional arguments.
#' @inheritParams replace_invalid
#'
#' @details
#' Uses the least squares formula.
#' 
#' @returns A numeric slope in units of `x/t`.
#' 
#' @examples
#' x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
#' mnirs:::slope(x)
#'
#' x_na <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
#' mnirs:::slope(x_na)
#' mnirs:::slope(x_na, na.rm = TRUE)
#' 
#' @keywords internal
slope <- function(
    x,
    t = seq_along(x),
    na.rm = FALSE,
    ...
) {
    ## validation =================================================
    args <- list(...)
    if (na.rm) {
        complete <- which(is.finite(x) & is.finite(t))
        x <- x[complete]
        t <- t[complete]
    }

    n <- length(x)
    if (!(args$bypass_checks %||% FALSE)) {
        if (!is.numeric(t)) {
            abort_validation("t", integer = FALSE, msg1 = "", msg2 = ".")
        }
        if (n != length(t)) {
            cli_abort(c(
                "x" = "{.arg x} and {.arg t} must be {.cls numeric} vectors \\
            of equal length."
            ))
        }
    }
    
    if (n < max(args$min_obs, 2L)) {
        return(NA_real_)
    }

    ## processing =================================================
    sum_t <- sum(t)
    sum_x <- sum(x)
    sum_tx <- sum(t * x)
    sum_t2 <- sum(t^2)
    denom <- n * sum_t2 - sum_t^2 ## should not cause integer overflow
    if (is.na(denom) || denom == 0) {
        return(NA_real_)
    }
    slope_val <- (n * sum_tx - sum_t * sum_x) / denom

    if (args$intercept %||% FALSE) {
        intercept <- (sum_x - slope_val * sum_t) / n
        attr(slope_val, "intercept") <- intercept
    }

    return(slope_val)
}

#' Calculate rolling slope
#'
#' Computes rolling linear regression slopes within a local window along a
#' numeric vector.
#'
#' @param partial A logical specifying whether to perform the operation over a
#'   subset of available data within the local rolling window (`TRUE`), or
#'   requiring a complete window of valid samples (`FALSE`, by *default*). See
#'   *Details*.
#' @inheritParams slope
#' @inheritParams compute_local_windows
#'
#' @details
#' The local rolling window can be specified by either `width` as the number of
#'   samples, or `span` as the timespan in units of `t`. Specifying `width`
#'   tries to call [roll::roll_lm()] if `na.rm = TRUE` or there are
#'   no `NA`s, which is often *much* faster than specifying `span`.
#'
#' *`<CAUTION>`*, under certain edge-conditions the `roll::roll_lm()` method
#'   may return slightly different values than the equivalent specifying `span`.
#'
#' `align` defaults to *"centre"* the local window around `idx` between
#'   `[idx - floor((width-1)/2),` `idx + floor(width/2)]` when `width` is
#'   specified. Even `width` values will bias `align` to *"left"*, with the
#'   unequal sample forward of `idx`. When `span` is specified with
#'   `align = "centre"`, the local window is between `[t - span/2, t + span/2]`.
#'
#' `partial = TRUE` allows calculation of slope over partial windows with at
#'   least `2` valid samples, such as at edge conditions. However, this can
#'   return unstable results with noisy data and should not be used for certain
#'   applications, such as peak slope detection over a vector of noisy data.
#'
#' `na.rm = TRUE` will return a valid slope value as long as there are a
#'   minimum number of valid samples within the window (at least `2` when
#'   `partial = TRUE`). Otherwise, a single `NA` sample in the window will
#'   return `NA`.
#'
#' @seealso [zoo::rollapply()], [roll::roll_lm()]
#'
#' @returns A numeric vector of rolling local slopes in units of `x/t` the
#'   same length as `x`.
#'
#' @examples
#' x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
#' rolling_slope(x, span = 3)
#' rolling_slope(x, width = 3)
#'
#' x_na <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
#' rolling_slope(x_na, span = 3)
#' rolling_slope(x_na, span = 3, partial = TRUE)
#' rolling_slope(x_na, span = 3, partial = TRUE, na.rm = TRUE)
#' rolling_slope(x_na, width = 3, partial = TRUE)
#' rolling_slope(x_na, width = 3, partial = TRUE, na.rm = TRUE)
#'
#' @export
rolling_slope <- function(
    x,
    t = seq_along(x),
    width = NULL,
    span = NULL,
    align = c("centre", "left", "right"),
    partial = FALSE,
    na.rm = FALSE,
    verbose = TRUE,
    ...
) {
    ## validation =================================================
    args <- list(...)
    n <- length(x)

    if (!(args$bypass_checks %||% FALSE)) {
        align <- sub("^center$", "centre", align)
        align <- match.arg(align)
        if (missing(verbose)) {
            verbose <- getOption("mnirs.verbose", default = TRUE)
        }
        if (!is.numeric(x)) {
            abort_validation("x", integer = FALSE, msg1 = "", msg2 = ".")
        }
        if (!is.numeric(t)) {
            abort_validation("t", integer = FALSE, msg1 = "", msg2 = ".")
        }
        if (n != length(t)) {
            cli_abort(c(
                "x" = "{.arg x} and {.arg t} must be {.cls numeric} vectors \\
                of equal length."
            ))
        }
        ## validate all t values identical
        if (all(diff(t) == 0)) {
            return(rep(NA_real_, n))
        }
        validate_width_span(width, span, verbose)
    }

    ## min_obs default to estimated width when span is specified
    min_obs <- if (partial) {
        2L
    } else {
        ## less strict span_width - 2 to allow start & end buffer
        ## with irregular t values
        max(width %||% (floor(span * estimate_sample_rate(t)) - 2L), 2L)
    }

    if (n < min_obs) {
        return(rep(NA_real_, n))
    }

    ## processing =================================================
    if (
        (na.rm || !anyNA(x)) &&
            !is.null(width) &&
            is.null(span) &&
            !(args$window_idx %||% FALSE)
    ) {
        ## use {roll} for fast rolling
        rlang::check_installed(
            c("roll", "RcppParallel"),
            reason = "to use fast rolling functions"
        )
        if (rlang::is_installed("roll")) {
            return(
                rolling_lm(
                    x,
                    t,
                    width,
                    align,
                    min_obs,
                    verbose,
                    bypass_checks = TRUE
                )
            )
        }
    }

    window_idx <- compute_local_windows(
        t, width = width, span = span, align = align
    )
    if (verbose && all(lengths(window_idx) < min_obs)) {
        ## TODO should warn for rolling_lm() condition
        cli_warn(c(
            "!" = "Less than {.val {min_obs}} valid samples detected in \\
            {.fn rolling_slope} windows.",
            "i" = "Specify {.arg width} >= {.val {2}} or increase {.arg span} \\
            to include more samples."
        ))
    }
    slopes <- vapply(window_idx, \(.idx) {
        slope(x[.idx], t[.idx], na.rm, min_obs = min_obs, bypass_checks = TRUE)
    }, numeric(1))

    if (args$window_idx %||% FALSE) {
        attr(slopes, "window_idx") <- window_idx
    }

    return(slopes)
}


#' Find peak slope
#'
#' Identifies the maximum positive or negative local linear slope within a
#' numeric vector and returns regression parameters
#'
#' @param direction A character string specifying either the peak
#'   `"positive"` or `"negative"` slope, or `"auto"` detect (the *default*)
#'   based on the overal trend of the signal.
#' @inheritParams rolling_slope
#'
#' @returns A named list containing:
#'   \item{`slope`}{The peak slope value in units of `x/t`.}
#'   \item{`intercept`}{The y-intercept of the peak local regression equation.}
#'   \item{`y`}{The response value predicted from `x` at `t`.}
#'   \item{`t`}{The time value at the index of the peak slope window.}
#'   \item{`idx`}{The index position of the peak slope.}
#'   \item{`window_idx`}{An integer vector of indices for the peak slope 
#'   window.}
#'
#' @details
#' The function computes rolling slopes via [rolling_slope()].
#'
#' When `direction = "auto"`, the net slope across all of `x` is calculated 
#'   to determine the trend direction. If the net slope equals zero, will 
#'   return the greatest absolute slope.
#'
#' When `direction = "positive"`, returns the greatest positive slope. And 
#'   vice versa for `"negative"`. If no positive/negative slopes exist, returns
#'   `NA` with a warning.
#'
#' @examples
#' x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
#' peak_slope(x, width = 5)
#'
#' x_dec <- rev(x)
#' peak_slope(x_dec, width = 5)
#'
#' @export
peak_slope <- function(
    x,
    t = seq_along(x),
    width = NULL,
    span = NULL,
    align = c("centre", "left", "right"),
    direction = c("auto", "positive", "negative"),
    partial = FALSE,
    na.rm = FALSE,
    verbose = TRUE,
    ...
) {
    align <- sub("^center$", "centre", align)
    align <- match.arg(align)
    direction <- match.arg(direction)
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }

    ## pre-return NA
    na_result <- list(
        t = NA_real_,
        y = NA_real_,
        slope = NA_real_,
        intercept = NA_real_,
        idx = NA_integer_,
        window_idx = NA_integer_
    )

    ## calculate all rolling slopes
    slopes <- rolling_slope(
        x,
        t,
        width,
        span,
        align,
        partial,
        na.rm,
        verbose,
        window_idx = TRUE,
        bypass_checks = TRUE
    )

    if (all(is.na(slopes))) {
        return(na_result)
    }

    ## auto-detect direction from net trend
    if (direction == "auto") {
        net_slope <- slope(x, t, na.rm = TRUE, bypass_checks = TRUE)

        direction <- if (is.na(net_slope) || net_slope == 0) {
            ## fallback to magnitude comparison when net slope is zero/NA
            max_pos <- max(slopes, na.rm = TRUE)
            min_neg <- min(slopes, na.rm = TRUE)
            if (abs(max_pos) >= abs(min_neg)) {
                "positive"
            } else {
                "negative"
            }
        } else if (net_slope > 0) {
            "positive"
        } else {
            "negative"
        }
    }

    ## manual direction calculation
    candidates <- switch(
        direction,
        positive = which(slopes > 0),
        negative = which(slopes < 0)
    )

    if (length(candidates) == 0L) {
        if (verbose) {
            cli_warn(c(
                "!" = "No {direction} slopes detected."
            ))
        }
        return(na_result)
    }

    ## find peak (max for positive, min for negative)
    peak_idx <- switch(
        direction,
        positive = candidates[which.max(slopes[candidates])],
        negative = candidates[which.min(slopes[candidates])]
    )

    ## get window indices at peak
    window_idx <- attr(slopes, "window_idx")[[peak_idx]]

    ## calculate peak slope, intercept, and predicted y val
    peak_slope_val <- slope(
        x[window_idx],
        t[window_idx],
        na.rm = na.rm,
        intercept = TRUE,
        bypass_checks = TRUE
    )
    intercept <- attr(peak_slope_val, "intercept")
    peak_slope_val <- as.numeric(peak_slope_val) ## remove attribute
    t_peak <- t[peak_idx]
    y_peak <- intercept + peak_slope_val * t_peak

    ## return
    list(
        slope = peak_slope_val,
        intercept = intercept,
        y = y_peak,
        t = t_peak,
        idx = peak_idx,
        window_idx = window_idx
    )
}