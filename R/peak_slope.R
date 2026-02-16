#' Calculate linear slope
#'
#' `slope()`: Calculates the linear regression slope of a numeric vector.
#'
#' @inheritParams peak_slope
#'
#' @returns `slope()` returns a numeric slope in units of `x/t`.
#'
#' @rdname rolling_slope
#' @order 2
#' @keywords internal
slope <- function(
    x,
    t = seq_along(x),
    ...
) {
    ## validation =================================================
    args <- list(...)

    if (!(args$bypass_checks %||% FALSE)) {
        validate_x_t(x, t, invalid = TRUE)
    }

    ## remove invalid
    complete <- which(is.finite(x) & is.finite(t))
    x <- x[complete]
    t <- t[complete]
    n <- length(x)

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
        attr(slope_val, "intercept") <- (sum_x - slope_val * sum_t) / n
    }

    return(slope_val)
}

#' Calculate rolling linear slope
#'
#' `rolling_slope()`: Computes rolling linear regression slopes within a
#' local window along a numeric vector.
#'
#' @inheritParams peak_slope
#'
#' @details
#' See details in [peak_slope()].
#'
#' Additional args (`...`) accepts:
#' \describe{
#'   \item{`bypass_checks`}{Logical; Speeds operation by bypassing validation
#'   checks. These checks should be performed upstream.}
#'   \item{`min_obs`}{Integer; The minimum number of observations required
#'   to calculate `slope()`. Defined by either `width` or `span`, or equal to
#'   `2` when `partial = TRUE`}
#'   \item{`intercept`}{Logical; When `TRUE`, `slope()` will also return a
#'   numeric intercept value retrievable with `attr(slope, "intercept")`.}
#'   \item{`window_idx`}{Logical; When `TRUE`, `rolling_slope()` will also
#'   return a list of numeric window indices retrievable with
#'   `attr(rolling_slope, "window_idx")`.}
#' }
#'
#' @seealso [zoo::rollapply()]
#'
#' @returns `rolling_slope()` returns a numeric vector of rolling local slopes
#'   in units of `x/t` the same length as `x`.
#'
#' @rdname rolling_slope
#' @order 1
#' @keywords internal
rolling_slope <- function(
    x,
    t = seq_along(x),
    width = NULL,
    span = NULL,
    align = c("centre", "left", "right"),
    partial = FALSE,
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

        validate_x_t(x, t, invalid = TRUE)

        ## informative warning message for length zero without aborting
        if (n == 0L) {
            cli_warn(c(
                "!" = "Slopes cannot be calculated over an empty vector."
            ))
            return(numeric(0))
        }
        ## validate all t values identical
        if (all(diff(t) == 0)) {
            return(rep(NA_real_, n))
        }
        validate_x_t(x, t, invalid = TRUE)
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
    window_idx <- compute_local_windows(
        t, width = width, span = span, align = align
    )
    if (verbose && all(lengths(window_idx) < min_obs)) {
        cli_warn(c(
            "!" = "Less than {.val {min_obs}} valid samples detected in \\
            {.fn rolling_slope} windows.",
            "i" = "Specify {.arg width} >= {.val {2}} or increase {.arg span} \\
            to include more samples."
        ))
    }
    slopes <- vapply(window_idx, \(.idx) {
        slope(x[.idx], t[.idx], min_obs = min_obs, bypass_checks = TRUE)
    }, numeric(1))

    if (args$window_idx %||% FALSE) {
        attr(slopes, "window_idx") <- window_idx
    }

    return(slopes)
}


#' Find peak linear slope
#'
#' Identifies the maximum positive or negative local linear
#' slope within a numeric vector and returns regression parameters
#'
#' @param direction A character string to detect either the peak
#'   `"positive"` or `"negative"` slope, or `"auto"` detect (the *default*)
#'   based on the overal trend of the signal (see *Details*).
#' @param partial A logical specifying whether to perform the operation over a
#'   subset of available data within the local rolling window (`TRUE`), or
#'   requiring a complete window of valid samples (`FALSE`, by *default*). See
#'   *Details*.
#' @param ... Additional arguments.
#' @inheritParams compute_local_windows
#' @inheritParams replace_invalid
#'
#' @details
#' Uses rolling slope calculations via the least squares formula on complete
#'   case data. The local rolling window can be specified by either `width`
#'   as the number of samples, or `span` as the timespan in units of `t`.
#'
#' `align` defaults to *"centre"* the local window around `idx` between
#'   `[idx - floor((width-1)/2),` `idx + floor(width/2)]` when `width` is
#'   specified. Even `width` values will bias `align` to *"left"*, with the
#'   unequal sample forward of `idx`. When `span` is specified with
#'   `align = "centre"`, the local window is between `[t - span/2, t + span/2]`.
#'
#' When `direction = "auto"`, the net slope across all of `x` is calculated
#'   to determine the trend direction (positive or negative), then the
#'   greatest local slope in that direction is returned. If the net slope
#'   equals zero, will return the greatest absolute local slope. When
#'   `direction = "positive"` or `"negative"`, returns the greatest
#'   respective directional slope. If no positive/negative slopes exist,
#'   returns `NA` with a warning.
#'
#' The default `partial = FALSE` requires complete case data with the same
#'   number of valid samples as specified by `width` or `span` (number of
#'   samples is estimated for `span` from the sample rate of `t`). If fewer
#'   than the requires valid samples are present in the local vector, `NA` is
#'   returned.
#'
#' `partial = TRUE` allows calculation over partial windows with at least `2`
#'   valid samples, such as at edge conditions or over missing data `NA`s.
#'   However, these slope values will be sensitive to noisy data, so use
#'   with caution.
#'
#' @returns A named list containing:
#'   \item{`slope`}{The peak slope value in units of `x/t`.}
#'   \item{`intercept`}{The y-intercept of the peak local regression equation.}
#'   \item{`y`}{The response value predicted from `x` at `t`.}
#'   \item{`t`}{The time value at the index of the peak slope window.}
#'   \item{`idx`}{The index position of the peak slope.}
#'   \item{`fitted`}{A numeric vector of predicted values from `x` over 
#'   `t[window_idx]` from the peak slope and intercept.}
#'   \item{`window_idx`}{An integer vector of indices for the peak slope
#'   window.}
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
    verbose = TRUE,
    ...
) {
    args <- list(...)
    if (!(args$bypass_checks %||% FALSE)) {
        direction <- match.arg(direction)
        if (missing(verbose)) {
            verbose <- getOption("mnirs.verbose", default = TRUE)
        }
    }

    ## calculate all rolling slopes
    slopes <- rolling_slope(
        x,
        t,
        width,
        span,
        align,
        partial,
        verbose,
        window_idx = TRUE,
        bypass_checks = args$bypass_checks %||% FALSE ## use validations
    )

    ## pre-return NA
    na_result <- list(
        t = NA_real_,
        y = NA_real_,
        slope = NA_real_,
        intercept = NA_real_,
        idx = NA_integer_,
        fitted = NA_real_,
        window_idx = NA_integer_
    )

    if (all(is.na(slopes))) {
        return(na_result)
    }

    ## auto-detect direction from net trend
    if (direction == "auto") {
        net_slope <- slope(x, t, bypass_checks = TRUE)

        direction <- if (is.na(net_slope) || net_slope == 0) {
            ## fallback to magnitude comparison when net slope is zero/NA
            max_pos <- max(slopes, na.rm = TRUE)
            min_neg <- min(slopes, na.rm = TRUE)
            if (abs(max_pos) >= abs(min_neg)) "positive" else "negative"
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
        intercept = TRUE,
        bypass_checks = TRUE
    )
    intercept <- attr(peak_slope_val, "intercept")
    peak_slope_val <- as.numeric(peak_slope_val) ## remove attribute
    t_peak <- t[peak_idx]
    y_peak <- intercept + peak_slope_val * t_peak
    fitted <- intercept + peak_slope_val * t[window_idx]

    ## return
    list(
        slope = peak_slope_val,
        intercept = intercept,
        y = y_peak,
        t = t_peak,
        idx = peak_idx,
        fitted = fitted,
        window_idx = window_idx
    )
}


#' Analyse peak linear slope
#'
#' Processes the maximum positive or negative local linear slope for each
#' `nirs_channel` within a data frame and return a data frame of regression
#' parameters.
#'
#' @param channel_args An *optional* `list()` named by `nirs_channels` with 
#'   unique per-channel arguments to override global default arguments (see
#'   *Details*).
#' @inheritParams peak_slope
#' @inheritParams validate_mnirs
#' 
#' @details
#' ## `channel_args` per `nirs_channel`
#'
#' Arguments in `analyse_peak_slope()` apply to all `nirs_channels` by default.
#'   `channel_args` allows overriding defaults with unique values per 
#'   `nirs_channel`. e.g.:
#'
#' ```
#' analyse_peak_slope(
#'     data = df,
#'     nirs_channels = c(hhb, smo2),
#'     span = 3, 
#'     direction = "positive",
#'     channel_args = list(
#'         hhb = list(span = 5),
#'         smo2 = list(direction = "negative")
#'     )
#' )
#' ```
#' 
#' @returns A data frame of model results with rows for each `nirs_channel`,
#'   and metadata containing named lists for `"fitted"` and `"window_idx"` 
#'   values named by `nirs_channels`.
#' 
#' @keywords internal
analyse_peak_slope <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    width = NULL,
    span = NULL,
    align = c("centre", "left", "right"),
    direction = c("auto", "positive", "negative"),
    partial = FALSE,
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    ## validation ==============================================
    validate_mnirs_data(data)
    metadata <- attributes(data)
    nirs_channels <- validate_nirs_channels(
        enquo(nirs_channels), data, verbose = FALSE
    )
    time_channel <- validate_time_channel(enquo(time_channel), data)
    validate_width_span(width, span, verbose)
    align <- sub("^center$", "centre", align)
    align <- match.arg(align)
    direction <- match.arg(direction)
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }

    time_vec <- data[[time_channel]]
    default_args <- list(
        width = width,
        span = span,
        align = align,
        direction = direction,
        partial = partial,
        verbose = verbose,
        bypass_checks = TRUE,
        ...
    )

    ## process =================================
    ## iterate peak_slope per channel, bind results by row
    results_df <- do.call(rbind, lapply(nirs_channels, \(.nirs) {
        ## override defaults with per channel args
        nirs_args <- utils::modifyList(
            default_args,
            channel_args[[.nirs]] %||% list()
        )
        
        ## call peak_slope with per channel args
        result <- do.call(peak_slope, c(
                list(x = data[[.nirs]], t = time_vec),
                nirs_args
        ))
        
        ## return results as a data frame
        tibble::tibble(
            nirs_channels = .nirs,
            slope         = result$slope,
            intercept     = result$intercept,
            y             = result$y,
            t             = result$t,
            idx           = result$idx,
            fitted        = list(result$fitted),
            window_idx    = list(result$window_idx),
            channel_args  = list(nirs_args)
        )
    }))

    return(results_df)
}