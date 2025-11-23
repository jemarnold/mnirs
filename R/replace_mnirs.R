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
#' @param invalid_above,invalid_below Numeric values each specifying cutoff
#'   values, above or below which (respectively) will be replaced, *inclusive*
#'   of the specified cutoff values.
#' @param outlier_cutoff An integer for the local outlier threshold, as
#'   number of standard deviations above and below the local median. The
#'   *default* `outlier_cutoff = NULL` will not replace outliers.
#'   `outlier_cutoff = 3` is the standard replacement threshold following
#'   Pearson's rule.
#' @param width An integer defining the local window in number of samples
#'   around `idx` in which to perform the operation., between
#'   `[idx - floor(width/2), idx + floor(width/2)]`.
#' @param span A numeric value defining the local window timespan around `idx`
#'   in which to perform the operation. In units of `time_channel` or `t`,
#'   between `[t - span/2, t + span/2]`.
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
#' `replace_outliers` and `replace_missing` when `method = "median"` require 
#'   defining a local rolling window in which to perform outlier detection and 
#'   median interpolation. This window can be specified by either `width` as 
#'   the number of samples centred on `idx` between
#'   `[idx - floor(width/2), idx + floor(width/2)]`, or `span` as the timespan 
#'   in units of `time_channel` centred on `idx` between 
#'   `[t - span/2, t + span/2]`. A partial moving average will be calculated
#'   at the edges of the data.
#'
#' @returns
#' `replace_mnirs()` returns a [tibble][tibble::tibble-package] of class
#'   *"mnirs"* with metadata available with `attributes()`.
#'
#' @seealso [pracma::hampel()] [stats::approx()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(ggplot2)
#'
#' ## vectorised operation
#' x <- c(1, 999, 3, 4, 999, 6)
#' replace_invalid(x, invalid_values = 999, width = 2, method = "median")
#'
#' (x_na <- replace_outliers(x, outlier_cutoff = 3, width = 2, method = "NA"))
#'
#' replace_missing(x_na, method = "linear")
#'
#' ## read example data
#' data <- read_mnirs(
#'     file_path = example_mnirs("moxy_ramp"),
#'     nirs_channels = c(smo2 = "SmO2 Live"),
#'     time_channel = c(time = "hh:mm:ss"),
#'     inform = FALSE
#' )
#'
#' ## clean data
#' data_clean <- replace_mnirs(
#'     data,
#'     nirs_channels = NULL,       ## default to all nirs_channels in metadata
#'     time_channel = NULL,        ## default to time_channel in metadata
#'     invalid_values = c(0, 100), ## known invalid values in the data
#'     outlier_cutoff = 3,         ## recommended default value
#'     width = 15,                 ## local window to detect local outliers and replace missing values
#'     method = "linear",          ## linear interpolation over `NA`s
#' )
#'
#' ## plot original and and show where values have been replaced
#' plot(data, label_time = TRUE) +
#'     scale_colour_manual(
#'         name = NULL,
#'         breaks = c("smo2", "replaced"),
#'         values = palette_mnirs(2)
#'     ) +
#'     geom_point(
#'         data = data[data_clean$smo2 != data$smo2, ],
#'         aes(y = smo2, colour = "replaced"), na.rm = TRUE
#'     ) +
#'     geom_line(
#'         data = {
#'             data_clean[!is.na(data$smo2), "smo2"] <- NA
#'             data_clean
#'         },
#'         aes(y = smo2, colour = "replaced"), linewidth = 1, na.rm = TRUE
#'     )
#'
#' @rdname replace_mnirs
#' @order 1
#' @export
replace_mnirs <- function(
        data,
        nirs_channels = NULL,
        time_channel = NULL,
        invalid_values = NULL,
        invalid_above = NULL,
        invalid_below = NULL,
        outlier_cutoff = NULL,
        width = NULL,
        span = NULL,
        method = c("linear", "median", "locf", "NA"),
        inform = TRUE
) {
    ## validation ====================================
    method <- match.arg(method)
    check_conditions <- c(
        !is.null(c(invalid_values, invalid_above, invalid_below)),
        !is.null(outlier_cutoff),
        method != "NA"
    )
    ## do nothing condition
    if (!any(check_conditions)) {
        cli_abort(
            "At least one of {.arg invalid_values}, {.arg invalid_above}, \\
            {.arg invalid_below}, {.arg outlier_cutoff}, or {.arg method} \\
            must be specified."
        )
    }
    if (missing(inform)) {
        inform <- getOption("mnirs.inform", default = TRUE)
    }

    validate_mnirs_data(data)
    metadata <- attributes(data)
    ## inform = FALSE because grouping irrelevant
    nirs_channels <- validate_nirs_channels(data, nirs_channels, inform = FALSE)
    nirs_channels <- unlist(nirs_channels, use.names = FALSE)
    time_channel <- validate_time_channel(data, time_channel)
    validate_numeric(invalid_values)
    validate_numeric(invalid_above, 1, msg = "one-element")
    validate_numeric(invalid_below, 1, msg = "one-element")
    time_vec <- round(data[[time_channel]], 6)

    ## remove invalid, outliers, and NA ==============================
    data[nirs_channels] <- lapply(data[nirs_channels], \(.x) {
        if (check_conditions[1L]) {
            .x <- replace_invalid(
                x = .x,
                t = time_vec,
                invalid_values = invalid_values,
                invalid_above = invalid_above,
                invalid_below = invalid_below,
                method = "NA"
            )
        }
        if (check_conditions[2L]) {
            .x <- replace_outliers(
                x = .x,
                t = time_vec,
                width = width,
                span = span,
                method = "NA",
                outlier_cutoff = outlier_cutoff
            )
        }
        if (check_conditions[3L]) {
            .x <- replace_missing(
                x = .x,
                t = time_vec,
                width = width,
                span = span,
                method = method
            )
        }
        .x
    })

    ## Metadata =================================
    metadata$nirs_channels <- unique(c(metadata$nirs_channels, nirs_channels))
    metadata$time_channel <- time_channel

    return(create_mnirs_data(data, metadata))
}



#' Replace Invalid Values
#'
#' `replace_invalid()` detects specified invalid values or cutoff values in 
#' vector data and replaces them with the local median value or `NA`.
#'
#' @param x A numeric vector.
#' @param t An *optional* numeric vector of time or sample number.
#' @inheritParams replace_mnirs
#'
#' @details
#' `replace_invalid()` can be used to overwrite known invalid values in
#'   exported data.
#' 
#' - Specific `invalid_values` can be replaced, such as `c(0, 100, 102.3)`.
#'   Data ranges can be replaced with cutoff values specified by `invalid_above`
#'   and `invalid_below`, where any values higher or lower than the specified
#'   cutoff values (respectively) will be replaced, *inclusive* of the cutoff
#'   values themselves.
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
        invalid_values = NULL,
        invalid_above = NULL,
        invalid_below = NULL,
        width = NULL,
        span = NULL,
        method = c("median", "NA"),
        inform = TRUE
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
  
    if (is.null(c(invalid_values, invalid_above, invalid_below))) {
        cli_abort(
            "At least one of {.arg invalid_values}, {.arg invalid_above}, \\
            or {.arg invalid_below} must be specified."
        )
    }
  
    validate_numeric(invalid_values)
    validate_numeric(invalid_above, 1, msg = "one-element")
    validate_numeric(invalid_below, 1, msg = "one-element")
    method <- match.arg(method) == "median" ## into logical
    if (missing(inform)) {
        inform <- getOption("mnirs.inform", default = TRUE)
    }

    ## process ========================================================
    x <- round(x, 6) ## avoid floating point precision issues
    y <- x
    ## fill invalid indices with NA
    invalid_idx <- c(
        which(x %in% invalid_values),
        which(x >= invalid_above),
        which(x <= invalid_below)
    )
    y[invalid_idx] <- NA_real_

    if (!method) { ## if method = "NA"
        return(y)
    }

    if (method) { ## if method = "median"
        ## invalid_values removed to NA first,
        ## so returns local median excluding idx
        window_idx <- compute_local_windows(
            t, invalid_idx, width, span, inform = inform
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
#' `replace_outliers()` will compute rolling local median values across `x`,
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
        inform = TRUE
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
    if (missing(inform)) {
        inform <- getOption("mnirs.inform", default = TRUE)
    }

    ## process =====================================================
    window_idx <- compute_local_windows(
        t, width = width, span = span, inform = inform
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
    return(y)
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
