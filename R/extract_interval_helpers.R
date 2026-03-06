#' Specify interval boundaries by time, sample, or label
#'
#' Helper functions to define interval start or end boundaries for
#' [extract_intervals()].
#'
#' @param ... Specify start or end boundaries.
#'   \describe{
#'     \item{`by_time(...)`}{Numeric time values in units of `time_channel`.}
#'     \item{`by_sample(...)`}{Integer sample indices (row numbers).}
#'     \item{`by_label(...)`}{Character strings to match in `event_channel`.
#'     All matching occurrences are returned.}
#'   }
#'
#' @returns An object of class `"mnirs_interval"` for use with the `start`
#'   and `end` arguments of [extract_intervals()].
#'
#' @examples
#' ## start and end by time
#' extract_intervals(data, start = by_time(30), end = by_time(60))
#'
#' ## start by label, end by time
#' extract_intervals(data, start = by_label("exercise_start"), end = by_time(300))
#'
#' ## multiple intervals by sample index
#' extract_intervals(data, start = by_sample(100, 500), end = by_sample(200, 600))
#'
#' @export
by_time <- function(...) {
    by_time <- c(...)
    validate_numeric(by_time)
    structure(
        list(type = "time", by_time = by_time),
        class = "mnirs_interval"
    )
}


#' @rdname by_time
#' @export
by_sample <- function(...) {
    by_sample <- c(...)
    validate_numeric(by_sample, range = c(1, Inf), integer = TRUE)
    structure(
        list(type = "sample", by_sample = as.integer(by_sample)),
        class = "mnirs_interval"
    )
}


#' @rdname by_time
#' @export
by_label <- function(...) {
    by_label <- c(...)
    if (!is.character(by_label) || length(by_label) == 0L) {
        cli_abort("{.fn by_label} must be a valid {.cls character} vector.")
    }
    structure(
        list(type = "label", by_label = by_label),
        class = "mnirs_interval"
    )
}


## resolve a single mnirs_interval object to integer row indices
## @keywords internal
resolve_interval_indices <- function(
    interval,
    time_vec,
    event_vec = NULL
) {
    switch(
        interval$type,
        ## lowest index >= each time value
        time = findInterval(interval$by_time, time_vec, left.open = TRUE) + 1L,
        sample = interval$by_sample,
        label = {
            pattern <- paste(interval$by_label, collapse = "|")
            matches <- which(grepl(pattern, event_vec))
            if (length(matches) == 0L) {
                cli_abort(c(
                    "x" = "No events detected matching \\
                    {.val {interval$by_label}}.",
                    "i" = "Must match contents of {.arg event_channel} exactly."
                ))
            }
            matches
        }
    )
}


## resolve start/end into raw index vectors (no span applied)
## @keywords internal
resolve_interval <- function(
    start_interval,
    end_interval,
    time_vec,
    event_vec = NULL
) {
    has_start <- !is.null(start_interval)
    has_end <- !is.null(end_interval)

    if (has_start) {
        start_idx <- resolve_interval_indices(
            start_interval, time_vec, event_vec
        )
    }
    if (has_end) {
        end_idx <- resolve_interval_indices(
            end_interval, time_vec, event_vec
        )
    }

    if (has_start && has_end) {
        n_start <- length(start_idx)
        n_end <- length(end_idx)
        ## warn and truncate if lengths differ — only paired intervals are valid
        if (n_start != n_end) {
            n_intervals <- min(n_start, n_end)
            cli_warn(c(
                "!" = "{.arg start} ({col_blue(n_start)}) and {.arg end} \\
                ({col_blue(n_end)}) have unequal lengths.",
                "i" = "Returning {col_blue(n_intervals)} paired interval{?s}."
            ))
            start_idx <- start_idx[seq_len(n_intervals)]
            end_idx <- end_idx[seq_len(n_intervals)]
        }
    } else {
        ## start-only or end-only
        start_idx <- if (has_start) start_idx else end_idx
        end_idx <- if (has_end) end_idx else NULL
    }

    return(
        list(
            start_idx = start_idx,
            end_idx = end_idx,
            has_start = has_start,
            has_end = has_end
        )
    )
}
