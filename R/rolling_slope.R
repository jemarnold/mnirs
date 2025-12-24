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
    if (na.rm) {
        complete <- which(is.finite(x) & is.finite(t))
        x <- x[complete]
        t <- t[complete]
    }

    n <- length(x)
    args <- list(...)
    bypass_checks <- args$bypass_checks %||% FALSE
    if (!bypass_checks) {
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
    return((n * sum_tx - sum_t * sum_x) / denom)
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
#' `align` defaults to *"center"* the local window around `idx` between
#'   `[idx - floor((width-1)/2),` `idx + floor(width/2)]` when `width` is
#'   specified. Even `width` values will bias `align` to *"left"*, with the
#'   unequal sample forward of `idx`. When `span` is specified with 
#'   `align = "center"`, the local window is between `[t - span/2, t + span/2]`.
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
    align = c("center", "left", "right"),
    # min_obs = width,
    partial = FALSE,
    na.rm = FALSE,
    verbose = TRUE,
    ...
) {
    ## validation =================================================
    n <- length(x)
    bypass_checks <- list(...)$bypass_checks %||% FALSE
    if (!bypass_checks) {
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
        ## min_obs default to estimated width when span is specified
        min_obs <- if (partial) {
            2L
        } else {
            ## less strict span_width - 2 to allow start & end buffer 
            ## with irregular t values
            max(width %||% (floor(span * estimate_sample_rate(t)) - 2L), 2L)
        }

        ## TODO intend to warn when # samples < min_obs
        # if (verbose && span_width < min_obs) {
        #     cli_warn(c(
        #         "!" = "Less than {.val {min_obs}} valid samples detected in \\
        #         {.fn rolling_slope} windows.",
        #         "i" = "Specify {.arg width} ≥ {.val {2}} or increase \\
        #         {.arg span}."
        #     ))
        # }
        # if (min_obs < 2L || (!is.null(width) && min_obs > span_width)) {
        #     obs_range <- range(2L, span_width)
        #     min_obs <- max(2L, min(min_obs, span_width))
        #     if (verbose && span_width >= 2L) {
        #         cli_warn(c(
        #             "!" = "{.arg min_obs} must be an {.cls integer} between \\
        #             {.val {obs_range[1L]}} and {.val {obs_range[2L]}}.",
        #             "i" = "{.arg min_obs} set to {.val {min_obs}}."
        #         ))
        #     }
        # }
    }
    
    if (n < min_obs) {
        return(rep(NA_real_, n))
    }
    
    ## processing =================================================
    if ((na.rm || !anyNA(x)) && !is.null(width) && is.null(span)) {
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
            "i" = "Specify {.arg width} >= {.val {2}} or increase {.arg span}."
        ))
    }
    vapply(seq_along(window_idx), \(.i) {
        idx <- window_idx[[.i]]
        slope(x[idx], t[idx], na.rm, min_obs = min_obs, bypass_checks = TRUE)
    }, numeric(1))
}