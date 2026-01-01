#' Computes rolling local values
#'
#' `compute_local_windows()`: Compute a list of rolling window indices along a
#' time variable `t`.
#'
#' @param idx A numeric vector of indices of `t` at which to calculate local
#'   windows. All indices of `t` by *default*, or can be used to only calculate
#'   for known indicies, such as invalid values of `x`.
#' @param width An integer defining the local window in number of samples
#'   around `idx` in which to perform the operation, according to `align`.
#' @param span A numeric value defining the local window timespan around `idx`
#'   in which to perform the operation, according to `align`. In units of
#'   `time_channel` or `t`.
#' @param align Window alignment as *"centre"/"center"* (the *default*),
#'   *"left"*, or *"right"*. Where *"left"* is *forward looking*, and *"right"*
#'   is *backward looking* from the current sample.
#' @inheritParams replace_invalid
#'
#' @returns
#' `compute_local_windows()`: A list the same length as `idx` and the same or
#'   shorter length as `t` with numeric vectors of sample indices of length
#'   `width` samples or `span` units of time `t`.
#'
#' @details
#' The local rolling window can be specified by either `width` as the number of
#'   samples, or `span` as the timespan in units of `t`. Specifying `width`
#'   is often faster than `span`.
#'
#' `align` defaults to *"centre"* the local window around `idx` between
#'   `[idx - floor((width-1)/2),` `idx + floor(width/2)]` when `width` is
#'   specified. Even `width` values will bias `align` to *"left"*, with the
#'   unequal sample forward of `idx`, effectively returning `NA` at the last
#'   sample index. When `span` is specified, the local window is between
#'   `[t - span/2, t + span/2]`.
#'
#' @examples
#' x <- c(1, 2, 3, 100, 5)
#' t <- seq_along(x)
#'
#' ## a list of numeric vectors of rolling local windows along `t`
#' (window_idx <- mnirs:::compute_local_windows(t, width = 2, span = NULL))
#'
#' ## a numeric vector of local medians of `x`
#' (local_medians <- mnirs:::compute_local_fun(x, window_idx, median))
#'
#' ## a logical vector of local outliers of `x`
#' (is.outlier <- mnirs:::compute_outliers(x, window_idx, local_medians, outlier_cutoff = 3L))
#'
#' ## a list of numeric vectors of local windows of valid values of `x` neighbouring `NA`s.
#' x <- c(1, 2, 3, NA, NA, 6)
#' (window_idx <- mnirs:::compute_valid_neighbours(x, width = 2))
#'
#' (local_medians <- mnirs:::compute_local_fun(
#'     x, window_idx, median, na.rm = TRUE)
#' )
#'
#' x[is.na(x)] <- local_medians
#' x
#'
#' @rdname compute_helpers
#' @keywords internal
compute_local_windows <- function(
    t,
    idx = seq_along(t),
    width = NULL,
    span = NULL,
    align = c("centre", "left", "right")
) {
    align <- sub("^center$", "centre", align)
    align <- match.arg(align)
    n <- length(t)

    if (!is.null(width)) {
        ## right = backward looking; left = forward looking
        ## centre = left-biased
        offsets <- switch(
            align,
            centre = c(-floor((width - 1L) / 2L), floor(width / 2L)),
            left = c(0L, width - 1L),
            right = c(-(width - 1L), 0L)
        )
        start_idx <- pmax.int(1L, idx + offsets[1L])
        end_idx <- pmin.int(n, idx + offsets[2L])
    } else {
        # fmt: skip
        offsets <- switch(
            align,
            centre = c(-0.5, 0.5),
            left = c(0, 1),
            right = c(-1, 0)
        ) * span
        # fmt: skip
        start_idx <- findInterval(
            t[idx] + offsets[1L], t, left.open = TRUE
        ) + 1L
        end_idx <- findInterval(t[idx] + offsets[2L], t)
    }

    ## inclusive of x[i] for detect outliers
    Map(`:`, start_idx, end_idx)
}


#' @description
#' `compute_local_fun()`: Compute a rolling function along `x` from a list of
#' rolling sample windows.
#'
#' @param window_idx A list the same or shorter length as `x` with numeric
#'   vectors for the sample indices of local rolling windows.
#' @param fn A function to pass through for local rolling calculation.
#' @param ... Additional arguments.
#'
#' @details
#' Currently used functions are `c([stats::median()], [base::mean], [slope()])`.
#'
#' @returns
#' `compute_local_fun()`: A numeric vector the same length as `x`.
#'
#' @rdname compute_helpers
#' @keywords internal
compute_local_fun <- function(x, window_idx, fn, ...) {
    vapply(seq_along(window_idx), \(.i) {
        fn(x[window_idx[[.i]]], ...)
    }, numeric(1))
}


#' @description
#' `compute_outliers()`: Computes a vector of logicals indicating local
#' outliers of `x` within a list of rolling sample windows `window_idx`.
#'
#' @param local_medians A numeric vector the same length as `x` of local
#'   median values.
#'
#' @returns
#' `compute_outliers()`: A logical vector the same length as `x`.
#'
#' @rdname compute_helpers
#' @keywords internal
compute_outliers <- function(
    x,
    window_idx,
    local_medians,
    outlier_cutoff
) {
    n <- length(x)
    L <- 1.4826 ## 1 / qnorm(0.75): MAD at the 75% percentile of |Z|
    # MAD = median(|x - median(x)|) within each window
    ## median of absolute local residuals from the local median
    local_mad <- vapply(seq_len(n), \(.i) {
        median(abs(x[window_idx[[.i]]] - local_medians[.i]), na.rm = TRUE)
    }, numeric(1))

    ## robust variance threshold based on minimum sample difference
    ## TODO need to verify behaviour in edge cases
    abs_diffs <- abs(diff(x[!is.na(x)]))
    smallest_var <- suppressWarnings(min(abs_diffs[abs_diffs > 1e-5]))

    ## logical outlier positions
    abs_dev <- abs(x - local_medians)
    is_outlier <- abs_dev > smallest_var &
        abs_dev > (L * outlier_cutoff * local_mad)
    ## NAs from is_outlier check should return FALSE
    is_outlier[is.na(is_outlier)] <- FALSE
    return(is_outlier)
}


#' @description
#' `compute_valid_neighbours()`: Compute a list of rolling window indices along
#' `x` to either side of `NA`s.
#'
#' @returns
#' `compute_valid_neighbours()`: A list the same length as the `NA` values in
#'   `x` with numeric vectors of sample indices of length `width` samples or
#'   `span` units of time `t` for valid values neighbouring split to either
#'   side of the invalid `NA`s.
#'
#' @rdname compute_helpers
#' @keywords internal
compute_valid_neighbours <- function(
    x,
    t = seq_along(x),
    width = NULL,
    span = NULL,
    verbose = TRUE
) {
    na_idx <- which(is.na(x))
    valid_idx <- which(!is.na(x))
    n_valid <- length(valid_idx)
    n_na <- length(na_idx)

    if (!is.null(width)) {
        ## Find position to the left of each NA in valid_idx sequence
        pos <- findInterval(na_idx, valid_idx)
        half_width <- floor(width / 2L)

        window_idx <- vector("list", n_na)
        for (i in seq_len(n_na)) {
            ## Extract width samples before and after
            left <- max(1L, pos[i] - half_width + 1L):pos[i]
            right <- min(n_valid, pos[i] + 1L):min(n_valid, pos[i] + half_width)
            window_idx[[i]] <- valid_idx[sort(unique(c(left, right)))]
        }
        return(window_idx)
    }

    ## Pre-compute for span approach
    t_valid <- t[valid_idx]
    t_na <- t[na_idx]
    half_span <- span * 0.5

    window_idx <- lapply(seq_len(n_na), \(.i) {
        t_range <- c(t_na[.i] - half_span, t_na[.i] + half_span)
        in_range <- valid_idx[t_valid >= t_range[1L] & t_valid <= t_range[2L]]

        if (length(in_range) == 0) {
            pos <- findInterval(na_idx[.i], valid_idx)
            in_range <- valid_idx[c(pos, min(n_valid, pos + 1L))]
            in_range <- unique(in_range)
        }
        in_range
    })

    ## window of valid values exclusive around `x`
    return(window_idx)
}


#' @description
#' `rolling_median()`: Compute rolling median using [roll][roll::roll-package]
#' with configurable alignment
#'
#' @returns
#' `rolling_median()`: A numeric vector of local median values, the same length
#'   as `x`.
#'
#' @rdname compute_helpers
#' @keywords internal
rolling_median <- function(
    x,
    width,
    align = c("centre", "left", "right")
) {
    align <- sub("^center$", "centre", align)
    align <- match.arg(align)

    ## centre = left-biased
    if (align == "centre") {
        n <- length(x)
        shift <- floor(width / 2L)
        x_padded <- c(x, rep_len(x[n], shift))
        rolled <- roll::roll_median(
            x_padded,
            width,
            min_obs = 1L,
            na_restore = TRUE
        )

        return(rolled[seq_len(n) + shift])
    }

    ## roll::roll_lm is right-aligned: result[i] uses x[(i-width+1):i]
    if (align == "right") {
        rolled <- roll::roll_median(
            x,
            width,
            min_obs = 1L,
            na_restore = TRUE
        )
        return(rolled)
    }

    ## right = backward looking; left = forward looking
    rolled <- roll::roll_median(
        rev(x),
        width,
        min_obs = 1L,
        na_restore = TRUE
    )

    return(rev(rolled))
}


#' @description
#' `rolling_mean()`: Compute rolling mean using [roll][roll::roll-package]
#' with configurable alignment
#'
#' @returns
#' `rolling_mean()`: A numeric vector of local mean values, the same length as
#'   `x`.
#'
#' @rdname compute_helpers
#' @keywords internal
rolling_mean <- function(
    x,
    width,
    align = c("centre", "left", "right"),
    min_obs = width,
    verbose = TRUE,
    ...
) {
    align <- sub("^center$", "centre", align)
    align <- match.arg(align)

    bypass_checks <- list(...)$bypass_checks %||% FALSE
    if (!bypass_checks) {
        if (missing(verbose)) {
            verbose <- getOption("mnirs.verbose", default = TRUE)
        }
        validate_numeric(
            width, 1, c(1, Inf), integer = TRUE, msg1 = "one-element positive"
        )
        # if (min_obs < 1L || min_obs > width) {
        #     min_obs <- max(1L, min(min_obs, width))
        #     if (verbose) {
        #         cli_warn(c(
        #             "!" = "{.arg min_obs} must be an {.cls integer} between \\
        #         {.val {2}} and {.val {width}}.",
        #             "i" = "{.arg min_obs} set to {.val {min_obs}}."
        #         ))
        #     }
        # }
    }

    ## centre = left-biased
    if (align == "centre") {
        n <- length(x)
        shift <- floor(width / 2L)
        x_padded <- c(x, rep_len(NA_real_, shift))
        rolled <- roll::roll_mean(
            x_padded,
            width,
            min_obs = min_obs
        )
    
        return(rolled[seq_len(n) + shift])
    }

    ## roll::roll_lm is right-aligned: result[i] uses x[(i-width+1):i]
    if (align == "right") {
        rolled <- roll::roll_mean(
            x,
            width,
            min_obs = min_obs
        )
        return(rolled)
    }

    ## right = backward looking; left = forward looking
    rolled <- roll::roll_mean(
        rev(x),
        width,
        min_obs = min_obs
    )
 
    return(rev(rolled))
}


#' @description
#' `rolling_lm()`: Compute rolling linear regression slopes using
#' [roll][roll::roll-package] with configurable alignment
#'
#' @param min_obs An integer specifying the minimum number of valid samples
#'   required to return a value within a window, otherwise will return `NA`.
#' @inheritParams slope
#' @param ... Additional arguments.
#'
#' @returns
#' `rolling_lm()`: A numeric vector of local linear regression slopes, the same
#'   length as `x`.
#'
#' @rdname compute_helpers
#' @keywords internal
rolling_lm <- function(
    x,
    t = seq_along(x),
    width,
    align = c("centre", "left", "right"),
    min_obs = width,
    verbose = TRUE,
    ...
) {
    bypass_checks <- list(...)$bypass_checks %||% FALSE
    if (!bypass_checks) {
        align <- sub("^center$", "centre", align)
        align <- match.arg(align)
        if (!is.numeric(x)) {
            abort_validation("x", integer = FALSE, msg1 = "", msg2 = ".")
        }
        if (!is.numeric(t)) {
            abort_validation("t", integer = FALSE, msg1 = "", msg2 = ".")
        }
        if (length(x) != length(t)) {
            cli_abort(c(
                "x" = "{.arg x} and {.arg t} must be {.cls numeric} vectors \\
                of equal length."
            ))
        }
        validate_numeric(
            width, 1, c(2, Inf), integer = TRUE, msg1 = "one-element positive"
        )
        if (min_obs < 2L || min_obs > width) {
            min_obs <- max(2L, min(min_obs, width))
            if (verbose) {
                cli_warn(c(
                    "!" = "{.arg min_obs} must be an {.cls integer} between \\
                    {.val {2}} and {.val {width}}.",
                    "i" = "{.arg min_obs} set to {.val {min_obs}}."
                ))
            }
        }
    }

    ## centre = left-biased
    if (align == "centre") {
        n <- length(x)
        shift <- floor(width / 2L)
        x_padded <- c(x, rep_len(NA_real_, shift))
        t_padded <- c(t, seq(t[n] + 1L, length.out = shift))

        slopes <- roll::roll_lm(
            x = t_padded,
            y = x_padded,
            width = width,
            min_obs = min_obs
        )$coefficients[, 2L]

        return(slopes[seq_len(n) + shift])
    }

    ## roll::roll_lm is right-aligned: result[i] uses x[(i-width+1):i]
    if (align == "right") {
        slopes <- roll::roll_lm(
            x = t,
            y = x,
            width = width,
            min_obs = min_obs
        )$coefficients[, 2L]
        return(slopes)
    }

    ## right = backward looking; left = forward looking
    slopes <- rev(
        roll::roll_lm(
            x = rev(t),
            y = rev(x),
            width = width,
            min_obs = min_obs
        )$coefficients[, 2L]
    )

    return(slopes)
}


#' Preserve and Restore NA Information Within a Vector
#'
#' `preserve_na()` stores `NA` vector positions and extracts valid non-`NA`
#' values for later restoration with `restore_na()`.
#'
#' @param x A vector containing missing `NA` values.
#'
#' @returns
#' `preserve_na()` returns a list `na_info` with components:
#'   - `na_info$x_valid`: A vector with `NA` values removed.
#'   - `na_info$x_length`: A numeric value of the original input vector length.
#'   - `na_info$na_idx`: A logical vector preserving `NA` positions.
#'
#' `restore_na()` returns a vector `y` the same length as the original
#'   input vector `x` with `NA` values restored to their original positions.
#'
#' @examples
#' x <- c(1, NA, 3, NA, 5)
#' (na_info <- mnirs:::preserve_na(x))#'
#'
#' ## process with a function that would normally fail on NA
#' y <- na_info$x_valid * 2
#' (result <- mnirs:::restore_na(y, na_info))
#'
#' x <- c("A", "B", "C", NA, NA)
#' (na_info <- mnirs:::preserve_na(x))
#'
#' ## process with a function that would normally fail on NA
#' y <- tolower(na_info$x_valid)
#' (result <- mnirs:::restore_na(y, na_info))
#'
#' @keywords internal
preserve_na <- function(x) {
    na_info <- list(
        x_valid = x[!is.na(x)],
        x_length = length(x),
        na_idx = is.na(x)
    )
    return(na_info)
}


#' Preserve and Restore NA Information Within a Vector
#'
#' `restore_na()` restores `NA` values to their original vector positions
#' after processing valid non-`NA` values returned from `preserve_na()`.
#'
#' @param y A vector of valid non-`NA` values returned from `preserve_na()`.
#' @param na_info A list returned from `preserve_na()`.
#'
#' @rdname preserve_na
#' @keywords internal
restore_na <- function(y, na_info) {
    if (all(!na_info$na_idx)) {
        return(y)
    }
    ## fill original length of NAs
    result <- rep(NA, na_info$x_length)
    if (all(na_info$na_idx)) {
        return(result)
    }
    ## replace non-NA with processed output values
    result[!na_info$na_idx] <- y
    return(result)
}
