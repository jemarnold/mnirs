#' Computes vectorised rolling local values
#'
#' `compute_local_windows()`: Helper function to return a list of rolling
#' sample indices `idx` along a time variable `t`, defined by either `width`
#' in samples or `span` in units of `t`.
#'
#' @inheritParams replace_invalid
#' @inheritParams replace_mnirs
#' @param idx A numeric vector of indices of `t` at which to calculate local
#'   windows. All indices of `t` by *default*, or can be used to only calculate
#'   for known indicies, such as invalid values of `x`.
#'
#' @returns
#' `compute_local_windows()`: A list the same length as `idx` and the same or
#'   shorter length as `t` with numeric vectors of sample indices of length
#'   `width` samples or `span` units of time `t`.
#'
#' @details
#' Local rolling calculations are made within a window defined by either
#'   `width` as the number of samples centred on `idx` between
#'   `[idx - floor(width/2), idx + floor(width/2)]`, or `span` as the
#'   timespan in units of `time_channel` centred on `idx` between
#'   `[t - span/2, t + span/2]`. A partial moving average will be calculated
#'   at the edges of the data.
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
#' (is.outlier <- mnirs:::compute_outliers(x, window_idx, local_medians, outlier_cutoff = 3))
#'
#' ## a list of numeric vectors of local windows of valid values of `x` neighbouring `NA`s.
#' x <- c(1, 2, 3, NA, NA, 6)
#' (window_idx <- mnirs:::compute_window_of_valid_neighbours(x, width = 2))
#'
#' (local_medians <- mnirs:::compute_local_fun(x, window_idx, median))
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
    verbose = TRUE
) {
    validate_width_span(width, span, verbose)

    n <- length(t)
    if (!is.null(width)) {
        half_width <- floor(width * 0.5)
        start_idx <- pmax(1L, seq_len(n) - half_width)
        end_idx <- pmin(n, seq_len(n) + half_width)
    } else {
        half_span <- span * 0.5
        start_idx <- findInterval(t - half_span, t, left.open = TRUE) + 1L
        end_idx <- findInterval(t + half_span, t, left.open = FALSE)
    }

    lapply(idx, \(.i) {
        start_idx[.i]:end_idx[.i] ## inclusive of x[i] for detect outliers
        # setdiff(start_idx[.i]:end_idx[.i], .i) ## exclusive of x[i]
    })
    ## TODO implement faster two-element range idx vectorisation
    # cbind(start = start_idx[idx], end = end_idx[idx]) ## start/end indices
}


#' @description
#' `compute_local_fun()`: Helper function to return a vector of local values
#' calculated from `x` by a function `fn` within a list of rolling sample
#' windows.
#'
#' @param window_idx A list the same length as `window_idx` and the same or
#'   shorter length as `x` with numeric vectors for the sample indices of
#'   local rolling windows.
#' @param fn A function to pass through for local rolling calculation.
#'   Currently used functions are `c(median, mean)`.
#'
#' @returns
#' `compute_local_fun()`: A numeric vector the same length as `x`.
#'
#' @rdname compute_helpers
#' @keywords internal
compute_local_fun <- function(x, window_idx, fn) {
    n <- length(window_idx)
    vapply(seq_len(n), \(.i) {
        ## TODO implement faster two-element range idx vectorisation
        # window <- window_idx[.i,]
        # idx <- window[1L]:window[2L]
        # fn(x[window[idx]], na.rm = TRUE)
        fn(x[window_idx[[.i]]], na.rm = TRUE)
    }, numeric(1))
}


#' @description
#' `compute_outliers()`: Helper function to return a vector of logicals
#' indicating local outliers of `x` within a list of rolling sample windows
#' `window_idx`.
#'
#' @param local_medians A numeric vector the same length as `x` of local
#'   median values.
#'
#' @returns
#' `compute_outliers()`: A logical vector the same length as `x`.
#'
#' @rdname compute_helpers
#' @keywords internal
compute_outliers <- function(x, window_idx, local_medians, outlier_cutoff) {
    validate_numeric(
        outlier_cutoff, 1, c(0, Inf), integer = TRUE, 
        msg1 = "one-element positive"
    )

    n <- length(x)
    L <- 1.4826 ## 1 / qnorm(0.75): MAD at the 75% percentile of |Z|

    ## median of absolute local residuals from the local median
    local_outliers <- vapply(seq_len(n), \(.i) {
        median(abs(x[window_idx[[.i]]] - local_medians[.i]), na.rm = TRUE)
    }, numeric(1))

    ## robust variance threshold based on minimum sample difference
    ## TODO need to verify behaviour in edge cases
    abs_diffs <- abs(diff(x[!is.na(x)]))
    smallest_var <- suppressWarnings(min(abs_diffs[abs_diffs > 1e-5]))

    ## logical outlier positions
    is_outlier <- abs(x - local_medians) > smallest_var &
        abs(x - local_medians) > (L * outlier_cutoff * local_outliers)
    ## edge case to handle NAs if not handled before.
    ## TODO need to verify behaviour
    is_outlier[is.na(is_outlier)] <- FALSE
    return(is_outlier)
}


#' @description
#' `compute_window_of_valid_neighbours()`: Helper function to return a list of
#' sample indices `idx` along valid values of `x` to either side of `NA`s,
#' defined by either `width` in samples or `span` in units of `t`.
#'
#' @inheritParams replace_invalid
#' @inheritParams replace_mnirs
#'
#' @returns
#' `compute_window_of_valid_neighbours()`: A list the same length as `NA`
#'   values in `x` with numeric vectors of sample indices of length
#'   `width` samples or `span` units of time `t` for valid values neighbouring
#'   split to either side of the invalid `NA`s.
#'
#' @rdname compute_helpers
#' @keywords internal
compute_window_of_valid_neighbours <- function(
    x,
    t = seq_along(x),
    width = NULL,
    span = NULL,
    verbose = TRUE
) {
    validate_width_span(width, span, verbose)

    na_idx <- which(is.na(x))
    valid_idx <- which(!is.na(x))
    n_valid <- length(valid_idx)
    n_na <- length(na_idx)
    
    if (!is.null(width)) {
        ## Find position to the left of each NA in valid_idx sequence
        pos <- findInterval(na_idx, valid_idx)
        half_width <- floor(width * 0.5)

        window_idx <- vector("list", n_na)
        for (i in seq_len(n_na)) {
            ## Extract width samples before and after
            left <- max(1L, pos[i] - half_width + 1L):pos[i]
            right <- min(n_valid, pos[i] + 1L):min(n_valid, pos[i] + half_width)
            window_idx[[i]] <- valid_idx[sort(unique(c(left, right)))]
        }
    } else if (!is.null(span)) {
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
    }

    ## window of valid values exclusive around `x`
    return(window_idx)
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
