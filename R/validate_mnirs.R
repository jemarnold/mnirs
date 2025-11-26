#' Validate `{mnirs}` parameters
#'
#' Passes through manually defined parameters, or defines them from metadata
#' if present, and validates relevant data quality checks.
#'
#' @param data A data frame of class *"mnirs"* containing at least one
#'   column with numeric time or sample values, and one column with numeric
#'   mNIRS values, along with metadata.
#' @param nirs_channels A character vector of mNIRS channel names. Must match
#'   column names in `data` exactly. Will be taken from metadata if not
#'   defined explicitly.
#' @param time_channel A character string indicating the time or sample channel
#'   name. Must match column names in `data` exactly. Will be taken from
#'   metadata if not defined explicitly.
#' @param event_channel A character string indicating the event or lap channel
#'   name. Must match column names in `data` exactly. Will be taken from
#'   metadata if not defined explicitly.
#' @param require A logical to specify whether `event_channel` is required
#'   (the *default*) or optional (`event_channel` returned as `NULL`).
#' @param x A numeric vector.
#' @param sample_rate A numeric value for the sample rate in Hz. Will be taken
#'   from metadata or estimated from `time_channel` if not defined explicitly.
#' @param elements A numeric value for the number of numeric elements in `arg`.
#' @param range A two-element numeric vector for the range of valid numeric
#'   values in `arg`.
#' @param inclusive A character vector to specify which of `left` and/or
#'   `right` boundary values should be included in the range, or both
#'   (the *default*), or excluded if `FALSE`.
#' @param integer A logical indicating whether to test `arg` as an integer
#'   using `rlang::is_integerish()`, rather than a numeric (`FALSE` by
#'   *default*).
#' @param msg A character string detailing the `cli_abort` error message
#'   returned for invalid numeric values passed to `arg`.
#' @inheritParams read_mnirs
#'
#' @returns The validated object or an error message with [cli::cli_abort()].
#'
#' @name validate_mnirs
#' @keywords internal
NULL


#' @rdname validate_mnirs
validate_numeric <- function(
    x,
    elements = Inf,
    range = NULL,
    inclusive = c("left", "right"),
    integer = FALSE,
    msg = ""
) {
    if (is.null(x)) {
        ## pass through not defined
        return(invisible(NULL))
    }
    not_all_na <- !all(is.na(x))
    name <- substitute(x)
    valid <- !is.na(x) & !is.nan(x)

    ## check conditions
    element_ok <- if (is.finite(elements)) {
        sum(valid) == elements
    } else {
        sum(valid) > 0
    }

    range_ok <- is.null(range) ||
        all(between(x[valid], range[1L], range[2L], inclusive))

    integer_ok <- !integer || rlang::is_integerish(x[valid])

    ## abort message if fails any
    if (
        !is.numeric(x) || !not_all_na || !element_ok || !range_ok || !integer_ok
    ) {
        type <- if (integer) {
            "integer"
        } else {
            "numeric"
        }
        cli_abort("{.arg {name}} must be a valid {msg} {.cls {type}}.")
    }

    return(invisible())
}


#' @rdname validate_mnirs
validate_mnirs_data <- function(data, ncol = 2L) {
    ## validate is a data frame with at least two columns
    if (!is.data.frame(data) || length(data) < ncol) {
        cli_abort(c(
            "x" = "{.arg data} should be a data frame with at least one \\
            {.cls numeric} {.arg time_channel} column and one \\
            {.cls numeric} {.arg nirs_channels} column."
        ))
    }

    return(invisible())
}


#' @rdname validate_mnirs
validate_nirs_channels <- function(data, nirs_channels, inform = TRUE) {
    ## if not defined, check metadata
    if (is.null(nirs_channels) || length(nirs_channels) == 0) {
        nirs_channels <- attr(data, "nirs_channels") ## vector

        if (inform && !is.null(nirs_channels)) {
            cli_warn(
                "All {.arg nirs_channels} are grouped together. Overwrite \\
                this by grouping {.arg nirs_channels} explicitly"
            )
        }
    }

    ## if still not defined, return error
    if (is.null(nirs_channels)) {
        cli_abort(
            "{.arg nirs_channels} not found in metadata. Please check your \\
            data attributes or define {.arg nirs_channels} explicitly."
        )
    }

    nirs_unlisted <- unlist(nirs_channels)

    ## validate exists in data
    if (!is.character(nirs_unlisted) || !all(nirs_unlisted %in% names(data))) {
        cli_abort(
            "{.arg nirs_channels} not detected in {.arg data}. \\
            Channel names are case sensitive and should match exactly."
        )
    }

    ## validate is numeric and has >=2 valid values
    invalid_channels <- !vapply(data[nirs_unlisted], is.numeric, logical(1)) |
        vapply(data[nirs_unlisted], \(.x) sum(is.finite(.x)) < 2, logical(1))

    if (sum(invalid_channels) > 0) {
        cli_abort(
            "All specified {.arg nirs_channels} must be {.cls numeric} \\
            vector(s) with at least two valid values."
        )
    }

    return(nirs_channels)
}


#' @rdname validate_mnirs
validate_time_channel <- function(data, time_channel) {
    ## if not defined, check metadata
    if (is.null(time_channel)) {
        time_channel <- attr(data, "time_channel")
    }

    ## if still not defined, return error
    if (is.null(time_channel)) {
        cli_abort(
            "{.arg time_channel} not found in metadata. Please check your \\
            data attributes or define {.arg time_channel} explicitly."
        )
    }

    ## validate exists in data
    if (!is.character(time_channel) || !time_channel %in% names(data)) {
        cli_abort(
            "{.arg time_channel} not detected in {.arg data}. \\
            Channel names are case sensitive and should match exactly."
        )
    }

    ## validate is numeric and has >=2 valid values
    if (
        !is.numeric(data[[time_channel]]) ||
            sum(is.finite(data[[time_channel]])) < 2
    ) {
        cli_abort(
            "{.arg time_channel} must be a {.cls numeric} vector with \\
            at least two valid values."
        )
    }

    return(time_channel)
}


#' @rdname validate_mnirs
validate_event_channel <- function(data, event_channel, require = TRUE) {
    ## if not defined, check metadata
    if (is.null(event_channel)) {
        event_channel <- attr(data, "event_channel")
    }

    ## if still not defined, return error
    if (is.null(event_channel) && require) {
        cli_abort(
            "{.arg event_channel} not found in metadata. Please check your \\
            data attributes or define {.arg event_channel} explicitly."
        )
    } else if (is.null(event_channel) && !require) {
        ## return event_channel = NULL if not required
        return(event_channel)
    }

    ## validate exists in data
    if (!is.character(event_channel) || !event_channel %in% names(data)) {
        cli_abort(
            "{.arg event_channel} not detected in {.arg data}. \\
            Channel names are case sensitive and should match exactly."
        )
    }

    ## check for empty column
    valid_values <- !is.na(data[[event_channel]]) & data[[event_channel]] != ""
    if (sum(valid_values) == 0) {
        cli_abort(
            "{.arg event_channel} must contain at least one valid value."
        )
    }

    return(event_channel)
}


#' @rdname validate_mnirs
estimate_sample_rate <- function(x) {
    ## estimate samples per second
    sample_rate_raw <- 1 / median(diff(x), na.rm = TRUE)

    (\(x) {
        mags <- 10^floor(log10(x))
        vals <- x / mags
        pretty_base <- c(1, 2, 5, 10)
        rounded <- sapply(vals, \(.val) {
            pretty_base[which.min(abs(pretty_base - .val))]
        })
        rounded * mags
    })(sample_rate_raw)
}


#' @rdname validate_mnirs
validate_sample_rate <- function(
    data,
    time_channel,
    sample_rate,
    inform = TRUE
) {
    ## if not defined, check metadata
    if (is.null(sample_rate)) {
        sample_rate <- attr(data, "sample_rate")
    }

    ## estimate sample_rate from time_channel
    ## time_channel MUST be validated before this
    time_vec <- round(as.numeric(data[[time_channel]]), 6)
    sample_rate_est <- estimate_sample_rate(time_vec)

    ## if still not defined, use estimated sample_rate
    if (is.null(sample_rate)) {
        sample_rate <- sample_rate_est
        if (inform) {
            cli_bullets(c(
                "!" = "Estimated {.arg sample_rate} = {.val {sample_rate}} Hz.",
                "i" = "Overwrite this with {.arg sample_rate} = {.cls numeric}."
            ))
        }
    }

    ## validate has one numeric value
    validate_numeric(
        sample_rate, 1, c(0, Inf), FALSE, msg = "one-element positive"
    )

    ## if provided sample rate seems off and time_channel doesn't appear
    ## to be integer values, report warning
    if (
        inform &&
            !isTRUE(all.equal(1, sample_rate_est, tolerance = 0.001)) &
            !isTRUE(all.equal(sample_rate_est, sample_rate, tolerance = 0.5))
    ) {
        cli_warn(c(
            "!" = "{.arg sample_rate} = {.val {sample_rate}} appears to be \\
            inconsistent with estimated sample_rate = \\
            {.val {sample_rate_est}}.",
            "i" = "Check that your sample rate and {.arg time_channel} \\
            values are consistent."
        ))
    }

    return(sample_rate)
}


#' Detect if numeric values fall between a range
#'
#' Vectorised inclusive within `x >= left` & `x <= right`, or exclusive
#' between `x > left` & `x < right`. Each side can be specified separately.
#'
#' @param x A numeric vector.
#' @param left,right Numeric boundary values for `x`
#' @param inclusive A character vector to specify which of `left` and/or
#'   `right` boundary values should be included in the range, or both (the
#'   default), or excluded if `FALSE`.
#'
#' @details
#' `inclusive = FALSE` can be used to test for positive non-zero values:
#'   `between(x, 0, Inf, inclusive = FALSE)`.
#'
#' @returns A logical vector the same length as `x`.
#'
#' @seealso [dplyr::between()]
#'
#' @keywords internal
between <- function(x, left, right, inclusive = c("left", "right")) {
    # validate_numeric(x)
    validate_numeric(left, 1L, msg = "one-element")
    validate_numeric(right, 1L, msg = "one-element")
    inclusive <- match.arg(
        as.character(inclusive),
        choices = c("left", "right", "FALSE"),
        several.ok = TRUE
    )

    if ("FALSE" %in% inclusive) {
        return(x > left & x < right)
    }

    left_compare <- if ("left" %in% inclusive) {
        x >= left
    } else {
        x > left
    }
    right_compare <- if ("right" %in% inclusive) {
        x <= right
    } else {
        x < right
    }

    return(left_compare & right_compare)
}
