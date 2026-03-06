#' Extract intervals from *{mnirs}* data
#'
#' Extract intervals from *"mnirs"* time series data, specifying interval
#' start and end boundaries by time, sample index, or event label using
#' [by_time()], [by_sample()], or [by_label()] helpers.
#'
#' @param data A data frame of class *"mnirs"* containing time series data and
#'   metadata.
#'
#' @param nirs_channels A character vector or a `list()` of character vectors
#'   of mNIRS channel names to operate on within each interval (see *Details*).
#'   Names must match column names in `data` exactly.
#'   - Only needs to be specified when `event_groups` contains *"ensemble"*-
#'     averaged intervals. If `event_groups = "distinct"` no channel processing
#'     occurs.
#'   - If `NULL` (default), channels are retrieved from *"mnirs"* metadata.
#'
#' @param event_channel An *optional* character string giving the name of an
#'   event/marker column. Required when using [by_label()] for `start` or
#'   `end`. Retrieved from metadata if not defined explicitly.
#'
#' @param sample_rate An *optional* numeric sample rate (Hz) used to bin time
#'   values for ensemble-averaging. If `NULL`, will be estimated from
#'   `time_channel` (see *Details*).
#'
#' @param start An interval specification created by [by_time()],
#'   [by_sample()], or [by_label()] indicating where intervals begin.
#'
#' @param end An interval specification created by [by_time()],
#'   [by_sample()], or [by_label()] indicating where intervals end.
#'
#' @param event_groups Either a character string or a `list()` of integer
#'   vectors specifying how to group intervals (see *Details*).
#'   \describe{
#'     \item{`"distinct"`}{The default. Extract each interval as an independent
#'     data frame.}
#'     \item{`"ensemble"`}{Ensemble-average each specified `nirs_channel` across
#'     all detected intervals, returning a single data frame.}
#'     \item{`list(c(1, 2), c(3, 4))`}{Ensemble-average each specified
#'     `nirs_channel` within each group and return one data frame per group.}
#'   }
#'
#' @param span A `list()` of two-element numeric vectors `c(before, after)` in
#'   units of `time_channel`. Applied additively to interval boundaries:
#'   - When both `start` and `end` are specified: `span[1]` shifts start times,
#'     `span[2]` shifts end times.
#'   - When only `start` or only `end` is specified: both `span[1]` and
#'     `span[2]` apply to the specified boundary (like a window around an
#'     event).
#'
#' @param zero_time Logical. Default is `FALSE`. If `TRUE`, re-calculates
#'   numeric `time_channel` values to start from zero within each interval
#'   data frame.
#'
#' @inheritParams validate_mnirs
#'
#' @details
#' ## Interval specification
#'
#' Interval boundaries are specified using helper functions:
#'
#' \describe{
#'   \item{[by_time()]}{Numeric time values in units of `time_channel`.}
#'   \item{[by_sample()]}{Integer sample indices (row numbers).}
#'   \item{[by_label()]}{Character strings to match in `event_channel`.
#'   All matching occurrences are returned.}
#' }
#'
#' `start` and `end` can use different specification types (e.g., start by
#' label, end by time). When lengths differ, the shorter is recycled.
#'
#' ## The `span` window
#'
#' `span` applies an additive time shift to interval boundaries:
#'
#' - **`start` + `end`**: `span[1]` shifts starts, `span[2]` shifts ends.
#'   For example, `start = by_time(30), end = by_time(60), span = c(-5, 10)`
#'   gives an interval of `[25, 70]`.
#' - **`start` only** or **`end` only**: both span values apply to the single
#'   boundary, like a window around an event. For example,
#'   `start = by_time(30), span = c(-5, 60)` gives `[25, 90]`.
#'
#' ## Per-interval `nirs_channels` for ensemble-averaging
#'
#' When `event_groups = "ensemble"` or a list of numeric grouped intervals,
#' `nirs_channels` can be specified as a list of column names to override
#' ensemble-averaging across interval. For example, to exclude a bad channel
#' in one interval:
#'
#' ```r
#' nirs_channels = list(
#'   c(A, B, C),
#'   c(A, C) ## channel "B" is excluded
#' )
#' ```
#'
#' If all grouped intervals can include all `nirs_channels`, or if
#' `event_groups = "distinct"`, a single `nirs_channels` character vector can
#' be supplied and recycled to all groups, or left as `NULL` for channels to
#' be taken from *"mnirs"* metadata.
#'
#' ## Grouping intervals
#'
#' `event_groups` controls whether extracted intervals are returned as distinct
#' data frames or ensemble-averaged.
#'
#' \describe{
#'    \item{`"distinct"`}{The default. Extract each interval and return a
#'    list of independent data frames.}
#'    \item{`"ensemble"`}{Ensemble-average each specified `nirs_channel` across
#'    all detected intervals and return a one-item list with a single data
#'    frame.}
#'    \item{`list(c(1, 2), c(3, 4))`}{Ensemble-average each specified
#'    `nirs_channel` within each group and return a list with one data frame
#'    for each group. Any intervals detected but not specified in
#'    `event_groups` are returned as distinct.}
#' }
#'
#' `event_groups` lists can be named (e.g.
#' `list(low = c(1, 2), high = c(3, 4))`) and will pass those names to the
#' returned list of data frames.
#'
#' When `event_groups` is a list of numeric interval numbers, list items in
#' `nirs_channels` and `span` are recycled to the number of groups. If lists
#' are only partially specified, the final item is recycled forward as needed.
#' Extra items are ignored.
#'
#' @returns A named `list()` of [tibbles][tibble::tibble-package] of class
#'   *"mnirs"*, with metadata available via `attributes()`.
#'
#' @examples
#' ## read example data
#' data <- read_mnirs(
#'     example_mnirs("train.red"),
#'     nirs_channels = c(
#'         smo2_left = "SmO2 unfiltered",
#'         smo2_right = "SmO2 unfiltered"
#'     ),
#'     time_channel = c(time = "Timestamp (seconds passed)"),
#'     zero_time = TRUE,
#'     verbose = FALSE
#' ) |>
#'     resample_mnirs(verbose = FALSE) ## avoid issues ensemble-averaging irregular samples
#'
#' ## ensemble-average across multiple intervals
#' interval_list <- extract_intervals(
#'     data,                       ## channels recycled to all intervals by default
#'     nirs_channels = c(smo2_left, smo2_right),
#'     start = by_time(368, 1093), ## manually identified interval start times
#'     event_groups = "ensemble",  ## ensemble-average across two intervals
#'     span = c(-20, 90),          ## include the last 180-sec of each interval (recycled)
#'     zero_time = TRUE            ## re-calculate common time to start from `0`
#' )
#'
#' interval_list[[1L]]
#'
#' \donttest{
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     plot(interval_list[[1L]], time_labels = TRUE) +
#'       ggplot2::geom_vline(xintercept = 0, linetype = "dotted")
#'   }
#' }
#'
#' @export
extract_intervals <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    event_channel = NULL,
    sample_rate = NULL,
    start = NULL,
    end = NULL,
    event_groups = list("distinct", "ensemble"),
    span = list(c(-60, 60)),
    zero_time = FALSE,
    verbose = TRUE
) {
    ## validation ==============================================
    validate_mnirs_data(data)
    metadata <- attributes(data)
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }
    nirs_channels <- validate_nirs_channels(enquo(nirs_channels), data, verbose)
    time_channel <- validate_time_channel(enquo(time_channel), data)
    ## avoid floating point precision issues downstream with findIntervals()
    time_vec <- round(data[[time_channel]], 6)
    ## estimate sample_rate for appropriate time binning
    sample_rate <- validate_sample_rate(
        data, time_channel, sample_rate, verbose
    )

    ## validate interval specs
    if (is.null(c(start, end))) {
        cli_abort(c(
            "x" = "No interval specification provided.",
            "i" = "Specify {.arg start} and/or {.arg end} using \\
            {.fn by_time}, {.fn by_sample}, or {.fn by_label}."
        ))
    }
    if (!is.null(start) && !inherits(start, "mnirs_interval")) {
        cli_abort(c(
            "x" = "{.arg start} must be created with {.fn by_time}, \\
            {.fn by_sample}, or {.fn by_label}."
        ))
    }
    if (!is.null(end) && !inherits(end, "mnirs_interval")) {
        cli_abort(c(
            "x" = "{.arg end} must be created with {.fn by_time}, \\
            {.fn by_sample}, or {.fn by_label}."
        ))
    }

    ## resolve event_channel if by_label is used
    uses_label <- (inherits(start, "mnirs_interval") &&
        start$type == "label") ||
        (inherits(end, "mnirs_interval") && end$type == "label")
    if (uses_label) {
        event_channel <- tryCatch(
            validate_event_channel(event_channel, data, required = TRUE),
            error = function(e) {
                cli_abort(c(
                    "x" = "{.arg event_channel} is required when using \\
                    {.fn by_label}.",
                    "i" = "Specify column name containing event labels."
                ))
            }
        )
        event_vec <- data[[event_channel]]
    } else {
        event_channel <- validate_event_channel(
            event_channel, data, required = FALSE
        )
        event_vec <- NULL
    }

    ## expand parameters ====================================
    event_groups <- make_list(event_groups)
    span <- make_list(span)

    ## resolve raw interval indices ============================
    interval_idx <- resolve_interval(start, end, time_vec, event_vec)

    n_events <- length(interval_idx$start_idx)

    ## recycle params to match number of intervals
    nirs_channels <- recycle_param(
        nirs_channels,
        n_events,
        event_groups,
        verbose
    )
    span <- recycle_param(span, n_events, event_groups, verbose)

    ## apply span and build interval spec ======================
    interval_spec <- apply_span_to_indices(
        interval_idx,
        time_vec,
        span,
        verbose
    )

    ## extract interval data ===================================
    interval_list <- extract_interval_list(
        data,
        interval_spec,
        nirs_channels
    )

    ## apply grouping logic ====================================
    result <- group_intervals(
        interval_list,
        nirs_channels,
        metadata,
        event_groups,
        zero_time,
        verbose
    )

    return(result)
}
