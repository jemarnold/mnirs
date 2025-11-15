#' Replace outliers, invalid values, and missing values
#'
#' @description
#' `replace_mnirs()` detects and removes local outliers and specified invalid
#' values in `nirs_channels` within an *"mnirs"* data frame, and replaces
#' missing `NA` values via interpolation methods.
#'
#' @param invalid_values A numeric vector of invalid values to be replaced,
#'   e.g. `invalid_values = c(0, 100, 102.3)`. The *default* `NULL` will not
#'   replace invalid values.
#' @param outlier_cutoff An integer for the local outlier threshold, as
#'   number of standard deviations above and below the local median. The
#'   *default* `outlier_cutoff = NULL` will not replace outliers.
#'   `outlier_cutoff = 3` is the standard replacement threshold following
#'   Pearson's rule.
#' @param width An integer defining the window in number of samples around
#'   `idx` in which to detect local outliers and perform median replacement,
#'   between `[idx - width, idx + width]`.
#' @param span A numeric value defining window timespan around `idx` in which
#'   to detect local outliers and perform median replacement. In units of
#'   `time_channel` or `t`, between `[t - span, t + span]`.
#' @param method A character string indicating how to handle replacement
#'   (see *Details* for more on each method):
#'   \describe{
#'      \item{`"linear"`}{Replaces `NA`s via linear interpolation (the
#'      *default*) using [stats::approx()].}
#'      \item{`"median"`}{Replaces `NA`s with the local median of valid values
#'      within a centred window defined by either `width` or `span`.}
#'      \item{`"locf"`}{(*"Last observation carried forward"*). Replaces
#'      `NA`s with the most recent valid non-`NA` value to the left for
#'      trailing `NA`s or to the right for leading `NA`s, using
#'      [stats::approx()].}
#'      \item{`"NA"`}{Returns `NA`s without replacement.}
#'   }
#' @inheritParams validate_mnirs
#'
#' @details
#' `replace_mnirs()` is a wrapper function expanding the vectorised `replace_*`
#'   functions to operate on a data frame.
#'
#' `nirs_channels` and `time_channel` can be retrieved automatically from
#'   `data` of class *"mnirs"* which has been processed with `{mnirs}`,
#'   if not defined explicitly.
#'
#' Channels (columns) in `data` not explicitly defined in `nirs_channels`
#'   will be passed through untouched to the output data frame.
#'
#' @returns
#' `replace_mnirs()` returns a [tibble][tibble::tibble-package] of class
#'   *"mnirs"* with metadata available with `attributes()`.
#'
#' @seealso [pracma::hampel()] [stats::approx()]
#'
#' @examples
#' ## vectorised operation
#' x <- c(1, 999, 3, 4, 999, 6)
#' replace_invalid(x, invalid_values = 999, width = 1, method = "median")
#'
#' x_na <- replace_outliers(x, outlier_cutoff = 3, width = 1, method = "NA")
#' x_na
#'
#' replace_missing(x_na, method = "linear")
#'
#' ## read example data
#' data <- read_mnirs(
#'     file_path = example_mnirs("moxy_ramp"),
#'     nirs_channels = c(smo2 = "SmO2 Live"),
#'     time_channel = c(time = "hh:mm:ss"),
#'     verbose = FALSE
#' )
#'
#' ## clean data
#' data_clean <- replace_mnirs(
#'     data,
#'     nirs_channels = NULL,       ## default to all nirs_channels in metadata
#'     time_channel = NULL,        ## default to time_channel in metadata
#'     invalid_values = c(0, 100), ## known invalid values in the data
#'     outlier_cutoff = 3,         ## recommended default value
#'     width = 7,                  ## local window to detect local outliers and replace missing values
#'     method = "linear",          ## linear interpolation over `NA`s
#' )
#'
#' \dontrun{
#' ## plot original and and show where values have been replaced
#' plot(data, display_mmss = TRUE) +
#'     scale_colour_manual(
#'         breaks = c("smo2", "replaced"),
#'         values = palette_mnirs(2)
#'     ) +
#'     geom_point(
#'         data = data[data_clean$smo2 != data$smo2, ],
#'         aes(y = smo2, colour = "replaced")
#'     ) +
#'     geom_line(
#'         data = {
#'             data_clean[!is.na(data$smo2), "smo2"] <- NA
#'             data_clean
#'         },
#'         aes(y = smo2, colour = "replaced"), linewidth = 1
#'     )
#' }
#'
#' @rdname replace_mnirs
#' @order 1
#' @export
replace_mnirs <- function(
        data,
        nirs_channels = NULL,
        time_channel = NULL,
        invalid_values = NULL,
        outlier_cutoff = NULL,
        width = NULL,
        span = NULL,
        method = c("linear", "median", "locf", "NA"),
        verbose = TRUE
) {
    ## validation ====================================
    method <- match.arg(method)
    ## do nothing condition
    if (is.null(c(invalid_values, width, span)) && method == "NA") {
        cli_abort(
            "{.arg invalid_values}, {.arg width}, {.arg span}, or \\
            {.arg method} must be defined."
        )
    }

    validate_mnirs_data(data)
    metadata <- attributes(data)
    ## verbose = FALSE because grouping irrelevant
    nirs_channels <- validate_nirs_channels(data, nirs_channels, verbose = FALSE)
    nirs_channels <- unlist(nirs_channels, use.names = FALSE)

    ## passthrough if only replacing missing but no `NA` in any nirs_channels
    if (
        !any(is.na(data[nirs_channels])) && method != "NA" &&
        is.null(c(invalid_values, width, span))
    ) {
        if (verbose) {
            cli_bullets(c(
                "i" = "No invalid or missing values detected in {.arg data}."
            ))
        }

        return(data) ## TODO verify has metadata
    }

    time_channel <- validate_time_channel(data, time_channel)
    validate_numeric(invalid_values)
    time_vec <- round(data[[time_channel]], 6)

    ## remove invalid ========================================
    if (!is.null(invalid_values)) {
        valid_values_list <- lapply(data[nirs_channels], \(.x) {
            replace_invalid(
                x = .x,
                t = time_vec,
                invalid_values = invalid_values,
                method = "NA"
            )
        })

        data[names(valid_values_list)] <- valid_values_list
    }

    ## remove outliers ======================================
    if (!is.null(outlier_cutoff)) {
        outliers_removed_list <- lapply(data[nirs_channels], \(.x) {
            replace_outliers(
                x = .x,
                t = time_vec,
                width = width,
                span = span,
                method = "NA",
                outlier_cutoff = outlier_cutoff
            )
        })

        data[names(outliers_removed_list)] <- outliers_removed_list
    }

    ## replace missing NA ===============================================
    if (method != "NA") {
        interpolated <- lapply(data[nirs_channels], \(.x) {
            replace_missing(
                x = .x,
                t = time_vec,
                width = width,
                span = span,
                method = method
            )
        })

        data[names(interpolated)] <- interpolated
    }

    ## Metadata =================================
    metadata$nirs_channels <- unique(c(metadata$nirs_channels, nirs_channels))
    metadata$time_channel <- time_channel
    metadata$verbose <- verbose

    return(create_mnirs_data(data, metadata))
}



#' Replace Invalid Values
#'
#' `replace_invalid()` detects specified invalid values in vector data and
#' replaces them with the local median value or `NA`.
#'
#' @param x A numeric vector.
#' @param t An *optional* numeric vector of time or sample number.
#' @inheritParams replace_mnirs
#'
#' @details
#' `replace_invalid()` can be used to overwrite known invalid values in
#'   exported data, such as `c(0, 100, 102.3)`.
#'
#' - `<under development>`: *allow for overwriting all values greater or less
#'   than specified values.*
#'
#' @returns
#' Vectorised `replace_*()` return a numeric vector the same length as `x`.
#'
#' @rdname replace_mnirs
#' @order 2
#' @export
replace_invalid <- function(
        x,
        t = seq_along(x),
        invalid_values,
        width = NULL,
        span = NULL,
        method = c("median", "NA"),
        verbose = TRUE
) {
    ## validate ===============================================
    validate_numeric(x)
    validate_numeric(t)
    if (length(x) != length(t)) {
        cli_abort(
            "{.arg x} and {.arg t} must be {.cls numeric} vectors of the \\
            same length."
        )
    }
    validate_numeric(invalid_values)
    method <- match.arg(method) == "median" ## into logical

    ## process ========================================================
    x <- round(x, 6) ## avoid floating point precision issues
    y <- x
    invalid_idx <- which(y %in% invalid_values)
    ## fill invalid values with NA
    y[invalid_idx] <- NA_real_

    if (!method) { ## if method = "NA"
        return(y)
    }

    if (method) { ## if method = "median"
        ## invalid_values removed to NA first,
        ## so returns local median excluding idx
        window_idx <- compute_local_windows(
            t, invalid_idx, width, span, verbose = verbose
        )
        local_medians <- compute_local_fun(y, window_idx, median)
        y[invalid_idx] <- local_medians
        return(y)
    }
}




#' Replace Local Outliers
#'
#' `replace_outliers()` detects local outliers in vector data with a Hampel
#' filter and replaces with the local median value or `NA`.
#'
#' @inheritParams replace_invalid
#'
#' @details
#' `replace_outliers()` will compute local rolling median values across `x`,
#'   defined by either `width` number of samples, or `span` timespan in units
#'   of `t`.
#'
#' - Outliers are detected with robust median absolute deviation (MAD) method
#'   adapted from [pracma::hampel()]. Outliers equal to or less than the
#'   smallest absolute time series difference in `x` will be excluded, to
#'   avoid detecting negligible differences as outliers where local data have
#'   minimal or zero variation.
#'
#' - Values of `x` outside local bounds defined by `outlier_cutoff` are
#'   identified as local outliers and either removed if `method = "NA"`, or
#'   replaced with the local median value (`method = "median"`, the *default*).
#'
#' - This function will NOT replace `NA` values already existing in the `x`.
#'   They will be passed along in the returned vector. See `replace_missing()`.
#'
#' - A high `outlier_cutoff` threshold makes the Hampel filter more forgiving.
#'   A low `outlier_cutoff` will declare more points to be outliers.
#'   `outlier_cutoff = 3` corresponds to Pearson's 3 sigma edit rule.
#'   `outlier_cutoff = 0` corresponds to Tukey's median filter.
#'
#' @rdname replace_mnirs
#' @order 3
#' @export
replace_outliers <- function(
        x,
        t = seq_along(x),
        outlier_cutoff = 3,
        width = NULL,
        span = NULL,
        method = c("median", "NA"),
        verbose = TRUE
) {
    ## validate ===============================================
    validate_numeric(x)
    validate_numeric(t)
    if (length(x) != length(t)) {
        cli_abort(
            "{.arg x} and {.arg t} must be {.cls numeric} vectors of the \\
            same length."
        )
    }
    method <- match.arg(method) == "median" ## into logical

    ## handle NAs ==================================================
    ## TODO obsolete functionality?
    handle_na <- FALSE #any(is.na(x))
    if (handle_na) {
        na_info <- preserve_na(x)
        x <- na_info$x_valid
        t <- t[!na_info$na_idx]
    }

    ## process =====================================================
    window_idx <- compute_local_windows(
        t, width = width, span = span, verbose = verbose
    )
    local_medians <- compute_local_fun(x, window_idx, median)
    is_outlier <- compute_outliers(x, window_idx, local_medians, outlier_cutoff)

    ## fill outliers with median or NA
    y <- x
    y[is_outlier] <- if (method) {
        local_medians[is_outlier]
    } else {
        NA_real_
    }
    ## return y to original x length with NAs if handled
    if (handle_na) {
        return(restore_na(y, na_info))
    } else {
        return(y)
    }
}




#' Replace Missing Values
#'
#' `replace_missing()` detects missing values in vector data and replaces
#' via interpolation methods.
#'
#' @param ... Additional arguments.
#' @inheritParams replace_invalid
#'
#' @details
#' `replace_missing()` will interpolate across missing values (`NA`s) as
#'   specified by `method`.
#'
#' - Leading and trailing `NA`s are replaced by *"nocb"* (*"next observation*
#'   *carried backward"*) and *"locf"*, respectively, for both `method = `
#'   `"linear"` and `"locf"`, by applying `rule = 2` (see [stats::approx()]).
#'
#' - `method = "median"` will calculate the local median of valid (non-`NA`)
#'   values to either side of `NA`s within a window defined by `width` number
#'   of samples, or a timespan defined by `span` in units of `t` (time). Such
#'   that sequential `NA`s will all be replaced by the same median value.
#'
#' - If there are no valid values within `span` to one side of the `NA`
#'   value(s), it will be replaced with the median of the other side (i.e. for
#'   leading and trailing `NA`s). If there are no valid values within either
#'   side of `span`, the first valid sample on either side will be used (i.e.
#'   equivalent to `replace_missing(x, width = 1)`).
#'
#' @rdname replace_mnirs
#' @order 4
#' @export
replace_missing <- function(
        x,
        t = seq_along(x),
        width = NULL,
        span = NULL,
        method = c("linear", "median", "locf"),
        ...
) {
    ## validate ===============================================
    validate_numeric(x)
    validate_numeric(t)
    if (length(x) != length(t)) {
        cli_abort(
            "{.arg x} and {.arg t} must be {.cls numeric} vectors of the \\
            same length."
        )
    }
    ## passthrough if no `NA`
    ## TODO can't use this if want to use for resampling with no NAs?
    # if (!any(is.na(x))) {
    #     return(x)
    # }
    method <- match.arg(method)
    if (method == "locf") {
        method <- "constant" ## swap for approx method arg
    }

    ## process ==============================================
    if (method %in% c("linear", "constant")) {
        y <- approx(
            x = t,
            y = x,
            xout = list(...)$xout %||% t, ## = t unless explicitly specified by hidden option
            method = method, ## c("linear", "constant")
            rule = 2, ## fill leading and trailing `NA`s
            f = 0, ## locf if method = "constant"
            ties = list("ordered", mean) ## assume ordered, take mean of ties
        )$y
    } else if (method == "median") {
        ## median of width or span VALID values to either side of sequential NAs
        y <- x
        na_idx <- which(is.na(x))
        window_idx <- compute_window_of_valid_neighbours(
            x = x,
            t = t,
            width = width,
            span = span
        )
        local_medians <- compute_local_fun(x, window_idx, median)
        y[na_idx] <- local_medians
    }

    return(y)
}
