#' Calculate linear slope
#'
#' `slope()`: Calculate the linear regression slope of a numeric vector via
#' the least-squares formula.
#'
#' @inheritParams peak_slope
#'
#' @returns `slope()` returns a numeric slope value in units of `x / t`, or
#'  `NA_real_` when insufficient valid observations are present.
#'
#' @rdname rolling_slope
#' @order 2
#' @keywords internal
slope <- function(
    x,
    t = seq_along(x),
    na.rm = FALSE,
    ...
) {
    ## validation =================================================
    args <- list(...)
    if (!(args$bypass_checks %||% FALSE)) {
        validate_x_t(x, t, invalid = TRUE)
    }

    if (length(x) < max(args$min_obs, 2L) || !na.rm && anyNA(x)) {
        return(NA_real_)
    }

    ## remove invalid
    ## ! redundant with `find_kinetics_idx`
    complete <- which(is.finite(x) & is.finite(t))
    x <- x[complete]
    t <- t[complete]
    n <- length(x)

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
#' `rolling_slope()`: Compute rolling linear regression slopes within a local
#' window along a numeric vector.
#'
#' @inheritParams peak_slope
#'
#' @details
#' See [peak_slope()] for details on window specification (`width`, `span`,
#' `align`), partial windows, and direction detection.
#'
#' Additional arguments (`...`) accepted:
#'
#' \describe{
#'   \item{`bypass_checks`}{Logical; if `TRUE`, skips input validation.
#'   Intended for internal use when checks have already been performed
#'   upstream.}
#'   \item{`min_obs`}{Integer; minimum number of valid observations required
#'   per window to return a slope. Derived from `width` or `span`, or `2L`
#'   when `partial = TRUE`.}
#'   \item{`intercept`}{Logical; if `TRUE`, `slope()` also attaches the
#'   y-intercept as `attr(slope_val, "intercept")`.}
#'   \item{`window_idx`}{Logical; if `TRUE`, the list of per-observation
#'   window indices is attached as `attr(slopes, "window_idx")`.}
#' }
#'
#' @seealso [peak_slope()]
#'
#' @returns `rolling_slope()` returns a numeric vector of rolling local slopes
#'   in units of `x / t`, the same length as `x`.
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
    na.rm = FALSE,
    verbose = TRUE,
    ...
) {
    ## validation =================================================
    args <- list(...)
    n <- length(x)

    if (!(args$bypass_checks %||% FALSE)) {
        validate_x_t(x, t, invalid = TRUE)
        align <- sub("^center$", "centre", align)
        align <- match.arg(align)

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

        if (missing(verbose)) {
            verbose <- getOption("mnirs.verbose", default = TRUE)
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
        cli_warn(c(
            "!" = "Insufficient valid samples detected in {.fn rolling_slope}.",
            "i" = "{.arg width} or {.arg span} must be smaller than \\
            the range of {.arg x}."
        ))
        return(rep(NA_real_, n))
    }

    ## processing =================================================
    window_idx <- compute_local_windows(
        t, width = width, span = span, align = align
    )

    if (verbose && all(lengths(window_idx) < min_obs)) {
        cli_warn(c(
            "!" = "Insufficient valid samples detected in \\
            {.fn rolling_slope} windows.",
            "i" = "Specify greater {.arg width} or {.arg span} to include \\
            more samples."
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


#' Find peak linear slope
#'
#' Identify the maximum positive or negative local linear slope within a
#' numeric vector using rolling least-squares regression and return a list
#' of regression parameters for the peak window.
#'
#' @param direction A character string specifying the slope direction to
#'   detect — `"auto"` (*default*), `"positive"`, or `"negative"`. See
#'   *Details*.
#' @param ... Additional arguments.
#' @inheritParams compute_local_windows
#' @inheritParams replace_invalid
#' @inheritParams filter_mnirs
#' @inheritParams filter_ma
#'
#' @details
#' ## Rolling window
#'
#' The local rolling window is defined by either `width` (number of samples)
#' or `span` (time duration in units of `t`). When `align = "centre"` and
#' `width` is specified, the window spans
#' `[idx - floor((width - 1) / 2), idx + floor(width / 2)]`. Even `width`
#' values bias alignment to *"left"*, placing the unequal sample forward of
#' `idx`. 
#' 
#' When `span` is specified with `align = "centre"`, the window spans
#' `[t - span / 2, t + span / 2]`.
#' 
#' ## Direction detection
#'
#' When `direction = "auto"`, the net slope across all of `x` is computed to
#' determine the overall trend (positive or negative), and the greatest local
#' slope in that direction is returned. 
#' 
#' If the net slope equals zero or is `NA`, the greatest absolute local slope 
#' is returned. When `direction = "positive"` or `"negative"`, the greatest 
#' respective directional slope is returned. If no slopes in the requested 
#' direction exist, `NA` is returned with a warning.
#'
#' ## Partial windows
#'
#' The default `partial = FALSE` requires a complete number of samples
#' specified by `width` or `span` (estimated from the sample rate of `t` when
#' `span` is used). `NA` is returned if fewer samples are present in the
#' local window. 
#' 
#' Setting `partial = TRUE` allows computation with at least 2 valid samples, 
#' such as at edge conditions. But these values will be more sensitive to 
#' noise and should be used with caution.
#' 
#' ## Missing values
#'
#' `na.rm` controls whether missing values (`NA`s) within each local window are 
#' either propagated to the returned vector when `na.rm = FALSE` (the default),
#' or ignored before processing if `na.rm = TRUE`.
#'
#' @returns A named list containing:
#'   \item{`slope`}{The peak slope value in units of `x / t`.}
#'   \item{`intercept`}{The y-intercept of the peak local regression line.}
#'   \item{`y`}{The predicted response value at the peak slope window index.}
#'   \item{`t`}{The time value at the peak slope window index.}
#'   \item{`idx`}{The integer index position of the peak slope window.}
#'   \item{`fitted`}{A numeric vector of predicted values spanning the peak 
#'   slope window.}
#'   \item{`window_idx`}{An integer vector of indices spanning the peak slope
#'   window.}
#'
#' @seealso [rolling_slope()]
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
        na.rm,
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
        net_slope <- slope(x, t, na.rm = TRUE, bypass_checks = TRUE)

        direction <- if (is.na(net_slope) || net_slope == 0) {
            ## fallback to slope magnitude comparison when net slope is zero/NA
            max_pos <- abs(max(slopes, na.rm = TRUE))
            min_neg <- abs(min(slopes, na.rm = TRUE))
            if (max_pos >= min_neg) "positive" else "negative"
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
            cli_warn(c("!" = "No {direction} slopes detected."))
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
        na.rm = TRUE,
        intercept = TRUE,
        bypass_checks = TRUE
    )
    intercept <- attr(peak_slope_val, "intercept")
    peak_slope_val <- as.numeric(peak_slope_val) ## remove attribute
    t_peak <- t[peak_idx]
    y_peak <- intercept + peak_slope_val * t_peak
    fitted <- intercept + peak_slope_val * t[window_idx]

    ## return
    return(list(
        slope = peak_slope_val,
        intercept = intercept,
        y = y_peak,
        t = t_peak,
        idx = peak_idx,
        fitted = fitted,
        window_idx = window_idx
    ))
}


#' Analyse peak linear slope across NIRS channels
#'
#' Compute the maximum local linear slope for each `nirs_channel` within a
#' *"mnirs"* data frame and return a data frame of regression parameters
#' with per-channel metadata as attributes.
#'
#' @param channel_args An *optional* named `list()` with names corresponding
#'   to `nirs_channels`, each containing a list of arguments to override the
#'   global defaults for that channel. See *Details*.
#' @inheritParams validate_mnirs
#' @inheritParams peak_slope
#'
#' @details
#' ## Per-channel argument overrides
#'
#' Arguments passed to `analyse_peak_slope()` apply to all `nirs_channels`
#' by default. `channel_args` allows overriding any argument for individual
#' channels, e.g.:
#'
#' ```r
#' analyse_peak_slope(
#'     data = df,
#'     nirs_channels = c(hhb, smo2),
#'     span = 3,
#'     direction = "positive",
#'     channel_args = list(
#'         hhb  = list(span = 5),
#'         smo2 = list(direction = "negative")
#'     )
#' )
#' ```
#'
#' @returns A `data.frame` with one row per `nirs_channel` and columns
#'   `nirs_channels`, `slope`, `intercept`, `y`, `<time_channel>`, `idx`.
#'   Per-channel metadata are attached as attributes:
#'   - `"predicted"`: a named list of data frames (per `nirs_channel`)
#'     with columns `window_idx` and `fitted`.
#'   - `"diagnostics"`: a `data.frame` with one row per `nirs_channel`
#'     containing model fit diagnostics.
#'   - `"channel_args"`: a `data.frame` with one row per `nirs_channel`
#'     recording the resolved arguments used.
#'
#' @seealso [analyse_kinetics()], [peak_slope()]
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
    end_fit_span = 20,
    partial = FALSE,
    na.rm = FALSE,
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    ## validation ==============================================
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }
    validate_mnirs_data(data)
    nirs_channels <- validate_nirs_channels(
        enquo(nirs_channels), data, verbose = FALSE
    )
    time_channel <- validate_time_channel(enquo(time_channel), data)
    validate_width_span(width, span, verbose)
    align <- sub("^center$", "centre", align)
    align <- match.arg(align)
    direction <- match.arg(direction)

    time_vec <- data[[time_channel]]
    default_args <- list(
        width = width,
        span = span,
        align = align,
        direction = direction,
        partial = partial,
        na.rm = na.rm,
        verbose = verbose,
        bypass_checks = TRUE,
        ...
    )

    ## process per-channel =================================
    results <- lapply(nirs_channels, \(.nirs) {
        all_args <- utils::modifyList(
            default_args,
            channel_args[[.nirs]] %||% list()
        )
    
        ## filter for valid finite idx before first extreme + end_fit_span
        valid <- find_kinetics_idx(
            data[[.nirs]],
            time_vec,
            end_fit_span,
            direction
        )
        x_fit <- data[[.nirs]][valid]
        t_fit <- time_vec[valid] ## ! is channel idx unstable for `t` idx??
        
        slopes <- do.call(peak_slope, c(list(x = x_fit, t = t_fit), all_args))
        diag <- compute_diagnostics(
            x = x_fit[slopes$window_idx],
            t = t_fit[slopes$window_idx],
            fitted = slopes$fitted,
            n_params = 1L,
            verbose = verbose
        )
        coefs <- data.frame(
            nirs_channels = .nirs,
            slope         = slopes$slope,
            intercept     = slopes$intercept,
            y             = slopes$y, ## predicted response value at idx
            t             = slopes$t,
            idx           = slopes$idx
        )
        names(coefs)[names(coefs) == "t"] <- time_channel ## rename

        list(
            coefficients = coefs,
            predicted = data.frame(
                window_idx = slopes$window_idx,
                fitted = slopes$fitted
            ),
            diagnostics = cbind(data.frame(nirs_channels = .nirs), diag),
            channel_args = safe_channel_args(.nirs, all_args)
        )
    })
    names(results) <- nirs_channels

    ## coefs tibble with per-channel metadata as attributes
    return(build_channel_results(results))
}
