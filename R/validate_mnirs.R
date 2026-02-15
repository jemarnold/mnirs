#' Validate `{mnirs}` parameters
#'
#' Passes through manually defined parameters, or defines them from metadata
#' if present, and validates relevant data quality checks.
#'
#' @param data A data frame of class *"mnirs"* containing time series data
#'   and metadata.
#' @param nirs_channels A character vector of mNIRS channel names to operate
#'   on. Must match column names in `data` exactly. Retrieved from metadata
#'   if not defined explicitly.
#' @param time_channel A character string indicating the time or sample channel
#'   name. Must match column names in `data` exactly. Retrieved from metadata
#'   if not defined explicitly.
#' @param event_channel A character string indicating the event or lap channel
#'   name. Must match column names in `data` exactly. Retrieved from metadata
#'   if not defined explicitly.
#' @param required A logical specifying whether `event_channel` is required
#'   (the *default*) or optional (`event_channel` returned as `NULL`).
#' @param x A numeric vector.
#' @param sample_rate A numeric value for the exported data sample rate in Hz.
#'   Retrieved from metadata or estimated from `time_channel` if not defined
#'   explicitly.
#' @param elements A numeric value for the number of numeric elements in `arg`.
#' @param range A two-element numeric vector for the range of valid numeric
#'   values in `arg`.
#' @param inclusive A character vector to specify which of `left` and/or
#'   `right` boundary values should be included in the range, or both
#'   (the *default*), or excluded if `FALSE`.
#' @param integer A logical indicating whether to test `arg` as an integer
#'   using `rlang::is_integerish()`, rather than a numeric (`FALSE` by
#'   *default*).
#' @param msg1,msg2 A character string detailing the `cli_abort` error message
#'   returned for invalid numeric values passed to `arg`.
#' @inheritParams read_mnirs
#'
#' @returns The validated object or an error message with [cli::cli_abort()].
#'
#' @name validate_mnirs
#' @keywords internal
NULL

#' validate_numeric abort message construction
#' @keywords internal
abort_validation <- function(name, integer = FALSE, msg1 = "", msg2 = ".") {
    type <- if (integer) "integer" else "numeric"

    cli_abort(c(
        "x" = paste0(
            "{.arg {name}} must be a valid ",
            msg1,
            " {.cls {type}}",
            msg2
        )
    ))
}


#' @rdname validate_mnirs
validate_numeric <- function(
    x,
    elements = Inf,
    range = NULL,
    inclusive = c("left", "right"),
    integer = FALSE,
    invalid = FALSE,
    msg1 = "",
    msg2 = "."
) {
    ## pass through NULL
    if (is.null(x)) {
        return(invisible(NULL))
    }

    name <- substitute(x)

    ## cheap early type check
    if (!is.numeric(x)) {
        abort_validation(name, integer, msg1, msg2)
    }

    valid <- !is.na(x)

    ## valid elements length
    n_valid <- if (!invalid) sum(valid) else length(x)
    if (!invalid && n_valid == 0L) {
        abort_validation(name, integer, msg1, msg2)
    }

    ## elements check
    if (is.finite(elements) && n_valid != elements) {
        abort_validation(name, integer, msg1, msg2)
    }
    ## range check
    if (!is.null(range) && !all(within(x[valid], range, inclusive))) {
        abort_validation(name, integer, msg1, msg2)
    }
    ## expensive integer check
    if (integer && !rlang::is_integerish(x[valid])) {
        abort_validation(name, integer, msg1, msg2)
    }

    return(invisible())
}


#' @rdname validate_mnirs
validate_mnirs_data <- function(data, ncol = 2L) {
    ## validate is a data frame with at least two columns
    if (!is.data.frame(data) || length(data) < ncol) {
        cli_abort(c(
            "x" = "{.arg data} must be a data frame with at least \\
            {.val {ncol}} column{?s}."
        ))
    }

    return(invisible())
}

#' Parse channel expressions for NSE
#'
#' Converts quosures to character vectors, handling bare symbols, character
#' strings, lists, and tidyselect expressions.
#'
#' @param channel A quosure from `rlang::enquo()`.
#' @param data A data frame for tidyselect context.
#' @param env Environment for symbol evaluation.
#'
#' @returns A character vector, list of character vectors, or `NULL`.
#'
#' @keywords internal
parse_channel_name <- function(channel, data, env = rlang::caller_env()) {
    if (rlang::quo_is_null(channel)) {
        return(NULL)
    }

    channel_raw <- rlang::quo_get_expr(channel)

    ## already-evaluated list or character
    if (is.list(channel_raw) || is.character(channel_raw)) {
        return(channel_raw)
    }

    ## bare symbol: check if column name, otherwise evaluate
    if (rlang::quo_is_symbol(channel)) {
        sym_name <- rlang::as_name(channel)
        if (sym_name %in% names(data)) {
            return(sym_name)
        }

        ## external object: evaluate and return directly
        result <- tryCatch(
            rlang::eval_tidy(channel, env = env),
            error = \(e) NULL
        )
        if (is.list(result) || is.character(result)) {
            return(result)
        }
        return(sym_name)
    }

    ## list() call: recurse on each element
    if (rlang::is_call(channel_raw, "list")) {
        result <- lapply(rlang::call_args(channel_raw), \(.arg) {
            parse_channel_name(rlang::new_quosure(.arg, env = env), data, env)
        })
        return(unname(result))
    }

    ## evaluate: tidyselect first, then fallback to direct evaluation
    ## handles c(), tidyselect helpers, symbols, and external objects
    tryCatch(
        unname(names(tidyselect::eval_select(channel, data))),
        error = \(e) {
            result <- rlang::eval_tidy(channel, env = env)
            if (is.list(result) || is.character(result)) {
                result
            } else {
                NULL
            }
        }
    )
}


#' @rdname validate_mnirs
validate_nirs_channels <- function(
    nirs_channels,
    data,
    verbose = TRUE,
    env = rlang::caller_env()
) {
    ## parse NSE input
    if (rlang::is_quosure(nirs_channels)) {
        nirs_channels <- parse_channel_name(nirs_channels, data, env)
    }
    nirs_unlisted <- unlist(nirs_channels)

    ## if not defined, check metadata
    if (is.null(nirs_unlisted) || length(nirs_unlisted) == 0) {
        nirs_channels <- attr(data, "nirs_channels") ## should be vector
        nirs_unlisted <- nirs_channels
        if (verbose && !is.null(nirs_unlisted)) {
            cli_inform(c(
                "i" = "{.arg nirs_channels} grouped together by default."
            ))
        }
    }

    ## if still not defined, return error
    if (is.null(nirs_unlisted)) {
        cli_abort(c(
            "x" = "{.arg nirs_channels} not detected in metadata.",
            "i" = "Check your data attributes or define \\
            {.arg nirs_channels} explicitly."
        ))
    }

    ## validate exists in data
    if (!is.character(nirs_unlisted) || !all(nirs_unlisted %in% names(data))) {
        cli_abort(c(
            "x" = "{.arg nirs_channels} not detected in {.arg data}.",
            "i" = "Channel names are case-sensitive and must match exactly."
        ))
    }

    ## validate is numeric and has >=2 valid values
    invalid_channels <- !vapply(data[nirs_unlisted], is.numeric, logical(1)) |
        vapply(data[nirs_unlisted], \(.x) sum(is.finite(.x)) < 2, logical(1))

    if (sum(invalid_channels) > 0) {
        cli_abort(c(
            "x" = "{.arg nirs_channels} must contain valid {.cls numeric} data."
        ))
    }

    return(nirs_channels)
}


#' @rdname validate_mnirs
validate_time_channel <- function(
    time_channel,
    data,
    env = rlang::caller_env()
) {
    ## parse NSE input
    if (rlang::is_quosure(time_channel)) {
        time_channel <- parse_channel_name(time_channel, data, env)
    }

    ## if not defined, check metadata
    if (is.null(time_channel)) {
        time_channel <- attr(data, "time_channel")
    }

    ## if still not defined, return error
    if (is.null(time_channel)) {
        cli_abort(c(
            "x" = "{.arg time_channel} not detected in metadata.",
            "i" = "Check your data attributes or define \\
            {.arg time_channel} explicitly."
        ))
    }

    ## validate exists in data
    if (!is.character(time_channel) || !time_channel %in% names(data)) {
        cli_abort(c(
            "x" = "{.arg time_channel} not detected in {.arg data}.",
            "i" = "Channel names are case-sensitive and must match exactly."
        ))
    }

    ## validate is numeric and has >=2 valid values
    if (
        !is.numeric(data[[time_channel]]) ||
            sum(is.finite(data[[time_channel]])) < 2
    ) {
        cli_abort(c(
            "x" = "{.arg time_channel} must contain valid {.cls numeric} data."
        ))
    }

    return(time_channel)
}


#' @rdname validate_mnirs
validate_event_channel <- function(
    event_channel,
    data,
    required = TRUE,
    env = rlang::caller_env()
) {
    ## parse NSE input
    if (rlang::is_quosure(event_channel)) {
        event_channel <- parse_channel_name(event_channel, data, env)
    }
    ## if not defined, check metadata
    if (is.null(event_channel)) {
        event_channel <- attr(data, "event_channel")
    }

    ## if still not defined, return error
    if (is.null(event_channel) && required) {
        cli_abort(c(
            "x" = "{.arg event_channel} not detected in metadata.",
            "i" = "Check your data attributes or define \\
            {.arg event_channel} explicitly."
        ))
    } else if (is.null(event_channel) && !required) {
        ## return event_channel = NULL if not required
        return(event_channel)
    }

    ## validate exists in data
    if (!is.character(event_channel) || !event_channel %in% names(data)) {
        cli_abort(c(
            "x" = "{.arg event_channel} not detected in {.arg data}.",
            "i" = "Channel names are case-sensitive and must match exactly."
        ))
    }

    ## check for empty column
    valid_values <- !is.na(data[[event_channel]]) & data[[event_channel]] != ""
    if (sum(valid_values) == 0) {
        cli_abort(c(
            "x" = "{.arg event_channel} must contain valid {.cls numeric} \\
            or {.cls character} data."
        ))
    }

    return(event_channel)
}


#' @rdname validate_mnirs
estimate_sample_rate <- function(x) {
    ## estimate samples per second
    sample_rate_raw <- 1 / median(diff(x), na.rm = TRUE)
    if (!is.finite(sample_rate_raw) || sample_rate_raw == 0) {
        cli_abort(c(
            "x" = "Unable to estimate {.arg sample_rate}.",
            "i" = "Check that {.arg time_channel} values are consistent.",
            "i" = "Set {.arg sample_rate} = {.cls numeric}."
        ))
    }

    mags <- 10^floor(log10(sample_rate_raw))
    vals <- sample_rate_raw / mags
    pretty_base <- c(1, 2, 5, 10)
    rounded <- vapply(vals, \(.val) {
        pretty_base[which.min(abs(pretty_base - .val))]
    }, numeric(1))
    return(rounded * mags)
}


#' @rdname validate_mnirs
validate_sample_rate <- function(
    data,
    time_channel,
    sample_rate,
    verbose = TRUE
) {
    ## if not defined, check metadata
    if (is.null(sample_rate)) {
        sample_rate <- attr(data, "sample_rate")
    }

    ## estimate sample_rate from time_channel
    ## time_channel MUST be validated before this
    time_vec <- as.numeric(data[[time_channel]])
    ## will error on unable to estimate sample_rate
    sample_rate_est <- estimate_sample_rate(time_vec)

    ## if still not defined, use estimated sample_rate
    if (is.null(sample_rate)) {
        sample_rate <- sample_rate_est
        if (verbose) {
            cli_inform(c(
                "!" = "Estimated {.arg sample_rate} = {.val {sample_rate}} Hz.",
                "i" = "Define {.arg sample_rate} explicitly to override."
            ))
        }
    }

    ## validate has one numeric value
    validate_numeric(
        sample_rate, 1, c(0, Inf), FALSE, msg1 = "one-element positive"
    )

    ## if provided sample rate seems off and time_channel doesn't appear
    ## to be integer values, report warning
    if (
        verbose &&
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

#' @rdname validate_mnirs
validate_width_span <- function(width = NULL, span = NULL, verbose = TRUE) {
    if (is.null(c(width, span))) {
        cli_abort(c(
            "x" = "Window size undefined",
            "i" = "One of {.arg width} or {.arg span} must be defined."
        ))
    }
    validate_numeric(
        width, 1, c(1, Inf), integer = TRUE, msg1 = "one-element positive"
    )
    validate_numeric(span, 1, c(0, Inf), msg1 = "one-element positive")
    if (verbose && !is.null(width) && !is.null(span)) {
        cli_inform(c(
            "i" = "{.arg width} = {.val {width}} overrides {.arg span}."
        ))
    }
}


#' @rdname validate_mnirs
validate_x_t <- function(x, t, invalid = FALSE) {
    ## exclude NULL by defaulting to invalid character
    x <- x %||% character()
    t <- t %||% character()
    validate_numeric(x, invalid = invalid)
    validate_numeric(t, invalid = invalid)
    if (length(x) != length(t)) {
        cli_abort(c(
            "x" = "{.arg x} and {.arg t} must be {.cls numeric} vectors \\
            of equal length."
        ))
    }
}


#' Validate if an item is a list
#' @keywords internal
make_list <- function(x) {
    if (is.list(x)) {
        return(x)
    } else {
        return(list(x))
    }
}
