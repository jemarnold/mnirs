#' Extract intervals from *{mnirs}* data
#'
#' Detect and extract intervals around specified events from *"mnirs"* time
#' series data.
#'
#' @param data A data frame of class *"mnirs"* containing time series data and
#'   metadata.
#'
#' @param nirs_channels A character vector or a `list()` of character vectors 
#'   of mNIRS channel names to operate on within each interval (see *Details*). 
#'   Names must match column names in `data` exactly.
#'   - If `NULL` (default), channels are retrieved from *"mnirs"* metadata.
#'   - Use multiple list items to include or exclude specific `nirs_channels`
#'     per interval.
#'
#' @param event_channel An *optional* character string giving the name of an
#'   event/marker column to import. Required to specify `event_labels`. Must
#'   match column names in `data` exactly. Retrieved from metadata if not
#'   defined explicitly.
#'
#' @param sample_rate An *optional* numeric sample rate (Hz) used to bin time
#'   values for ensemble-averaging. If `NULL`, will be estimated from
#'   `time_channel` (see *Details*).
#'
#' @param event_times A numeric vector of `time_channel` values indicating event
#'   start times (see *Details*).
#'
#' @param event_labels A character vector of strings to match in
#'   `event_channel`, indicating event starts. Matching is case-sensitive and
#'   must match exactly.
#'
#' @param event_samples an integer vector of sample indices (row numbers)
#'   indicating event starts.
#'
#' @param span A `list()` of two-element numeric vectors specifying the window
#'   around each event as `c(before, after)`, in units of `time_channel`.
#'
#' @param group_events Either a character string or a `list()` of integer
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
#' @param zero_time Logical. Default is `FALSE`. If `TRUE`, re-calculates
#'   numeric `time_channel` values to start from zero within each interval
#'   data frame.
#'
#' @inheritParams validate_mnirs
#'
#' @details
#' ## Event specification
#'
#' Interval events can be identified in three ways, in combination:
#'
#' \describe{
#'   \item{`event_times`}{Numeric time valuess in units of `time_channel`.}
#'   \item{`event_samples`}{Integer sample indices (row numbers).}
#'   \item{`event_labels`}{Character strings to match exactly in
#'   `event_channel`.}
#' }
#'
#' Events can be specified in any order, and will always be returned in the
#' order in which they appear in `data`.
#'
#' ## Per-interval `nirs_channels` for ensemble-averaging
#'
#' When `group_events = "ensemble"` or a list of numeric grouped intervals,
#' `nirs_channels` can be specified as a list of column names to override
#' ensemble-averaging across interval. For example, to exclude a bad channel
#' in one interval:
#'
#' ```r
#' nirs_channels = list(
#'   c("A", "B", "C"),
#'   c("A", "C") ## channel "B" is excluded
#' )
#' ```
#' 
#' If all grouped intervals can include all `nirs_channels`, or if 
#' `group_events = "distinct"`, a single `nirs_channels` character vector can 
#' be supplied and recycled to all groups, or left as `NULL` for channels to
#' be taken from *"mnirs"* metadata.
#'
#' ## Interval time `span` windows
#'
#' Each interval is defined relative to its event time in units of
#' `time_channel` as `[event_times + before, event_times + after]`.
#'
#' - `before` is typically a negative value (window can extend before the
#'   event).
#' - `after` is typically a positive value (window can extend after the event).
#' - Both values can be either positive or negative to reference an interval
#'   window either completely before, or completely after the indicated event,
#'   respectively.
#'
#' If an interval time span is partially out of bounds, available in-bounds
#' data are returned with a warning. Interval time spans entirely out of bounds
#' returns an error.
#'
#' ## Grouping (`group_events`)
#'
#' `group_events` controls whether extracted intervals are returned as distinct
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
#'    `group_events` are returned as distinct.}
#' }
#' 
#' `group_events` lists canned be named (e.g. 
#' `list(low = c(1, 2), high = c(3, 4))`) and will pass those names to the 
#' returned list of data frames. Otherwise, the return list will be named
#' `c("interval_1", "interval_2")` etc. for distinct intervals; `"ensemble"`
#' for ensemble-averaged; or `c("group_1_2", "group_3_4")` etc. for custom
#' grouping structure.
#'
#' When `group_events` is a list of numeric interval numbers, list items in
#' `nirs_channels` and `span` are recycled to the number of groups. If lists
#' are only partially specified (if there are more intervals or groups detected
#' than there are argument list items) The final argument item is recycled
#' forward as needed. Extra argument items are ignored.
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
#' ## extract intervals as a list of data frames
#' extract_intervals(
#'     data,
#'     nirs_channels = list(c(smo2_left, smo2_right)),
#'     event_times = c(368, 1093), ## specify interval events
#'     span = list(c(-20, 90)),    ## specify the event start-end timespans
#'     group_events = "distinct",  ## return all unique intervals
#'     zero_time = TRUE,           ## start time from zero
#'     verbose = FALSE
#' )
#'
#' ## ensemble-average across multiple intervals
#' interval_list <- extract_intervals(
#'     data,
#'     nirs_channels = list(c(smo2_left, smo2_right)),
#'     event_times = c(368, 1093),
#'     span = list(c(-20, 90)),
#'     group_events = "ensemble", ## return ensemble-averaged intervals
#'     zero_time = TRUE,
#'     verbose = FALSE
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
    event_times = NULL,
    event_labels = NULL,
    event_samples = NULL,
    span = list(c(-30, 180)),
    group_events = list("distinct", "ensemble"),
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
    ## validate `event_times` within bounds of `range(time_channel)`
    time_range <- range(time_vec, na.rm = TRUE)
    validate_numeric(
        event_times,
        range = time_range,
        msg2 = paste0(
            " within the range of `time_channel` = ",
            col_blue('[', paste(time_range, collapse = ', '), ']'),
            "."
        )
    )
    ## validate `event_samples` within bounds of `nrow(data)`
    sample_range <- c(1, length(time_vec))
    validate_numeric(
        event_samples,
        integer = TRUE,
        range = sample_range,
        msg2 = paste0(
            " within the nrows of `data` = ",
            col_blue('[', paste(sample_range, collapse = ', '), ']'),
            " samples"
        )
    )

    ## if `event_labels` provided, `event_channel` must be provided
    if (!is.null(event_labels)) {
        event_channel <- tryCatch(
            validate_event_channel(event_channel, data, required = TRUE),
            error = function(e) {
                ## more informative error message when event_labels present
                cli_abort(c(
                    "x" = "{.arg event_channel} is required when using \\
                    {.arg event_labels}.",
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

    ## detect events ===========================================
    ## return indices for each identified event
    event_indices <- detect_events(
        time_vec,
        event_vec,
        event_times,
        event_labels,
        event_samples,
        verbose
    )

    ## expand parameters ====================================
    ## n_events accounts for grouping structure of identified events
    n_events <- length(event_indices)
    group_events <- make_list(group_events)

    ## validate params are lists and expand last to fill any missing events
    nirs_channels <- recycle_param(
        nirs_channels,
        n_events,
        group_events,
        verbose
    )
    span <- recycle_param(span, n_events, group_events, verbose)

    ## specify interval metadata =======================================
    ## return a data frame of metadata for each interval
    interval_spec <- specify_intervals(
        time_vec,
        event_indices,
        span, ## as list
        verbose
    )

    ## extract interval data ===================================
    ## return a list of data frames for each interval
    interval_list <- extract_interval_list(
        data,
        interval_spec,
        nirs_channels ## as list
    )

    ## apply grouping logic ====================================
    ## return a list of data frames according to grouping logic
    ## where grouped intervals will be ensemble-averaged
    result <- group_intervals(
        interval_list,
        nirs_channels,
        metadata,
        group_events,
        zero_time,
        verbose
    )

    return(result)
}


#' Detect event indices
#' @keywords internal
detect_events <- function(
    time_vec,
    event_vec,
    event_times,
    event_labels,
    event_samples,
    verbose = TRUE
) {
    ## validate event_labels and match label indices
    label_indices <- if (!is.null(event_labels)) {
        matches <- grepl(paste(event_labels, collapse = "|"), event_vec)
        ## if no indices returned for any of `event_labels`
        if (verbose && !any(matches)) {
            cli_warn(c(
                "!" = "No events detected matching {.val {event_labels}}",
                "i" = "Must match contents of {.arg event_channel} exactly."
            ))
        }
        which(matches)
    } else {
        NULL
    }

    ## NULL if all NULL; integer(0) if all integer(0); numeric if any numeric
    event_indices <- sort(unique(c(
        event_samples,
        ## lowest idx GTE event_times
        findInterval(event_times, time_vec, left.open = TRUE) + 1L,
        label_indices
    )))

    ## if no indices returned
    if (length(event_indices) == 0) {
        cli_abort(c(
            "x" = "No events detected.",
            "i" = "Provide {.arg event_times}, {.arg event_labels}, or \\
            {.arg event_samples} within {.arg data}."
        ))
    }

    return(event_indices)
}


#' Recycle parameter list to target length
#' @keywords internal
recycle_to_length <- function(
    param,
    n,
    name = c("event", "group"),
    verbose = TRUE
) {
    n_param <- length(param)

    if (n_param == n) {
        return(param)
    }

    if (n_param > n) {
        if (verbose) {
            cli_inform(c(
                "!" = "{.arg {substitute(param)}} exceeds the number of \\
                {name}s by {.val {n_param - n}}.",
                "i" = "Extra values are ignored."
            ))
        }
        return(param[seq_len(n)])
    }

    ## n_param < n:  recycle last element forward
    if (verbose && n_param > 1L) {
        cli_inform(c(
            "i" = "{.arg {substitute(param)}} recycled to meet \\
            {.val {n - n_param}} unspecified {name}{qty(n - n_param)}{?s}."
        ))
    }
    return(param[c(seq_len(n_param), rep(n_param, n - n_param))])
}


#' Recycle parameter to match number of events
#'
#' Recycle an argument vector to a list or repeat the last list item to match
#' the number of events.
#'
#' @keywords internal
recycle_param <- function(param, n_events, group_events, verbose = TRUE) {
    ## flatten nested lists to single-depth list
    param <- if (is.list(param)) {
        lapply(param, \(.x) if (is.list(.x)) unlist(.x) else .x)
    } else {
        list(param)
    }

    ## custom grouping: recycle per group, then map to event order
    if (is.numeric(group_events[[1L]])) {
        n_groups <- length(group_events)
        groups_unlisted <- unlist(group_events)

        ## recycle param to number of groups
        param <- recycle_to_length(param, n_groups, "group", verbose)

        ## create mapping:  event_id -> group_id
        ## rep(1:n_groups, lengths(group_events)) gives group index per event in group_events
        group_for_event <- rep(seq_len(n_groups), lengths(group_events))

        ## build lookup:  position i holds group index for event i (NA if ungrouped)
        event_to_group <- integer(n_events)
        valid_events <- groups_unlisted[groups_unlisted <= n_events]
        valid_groups <- group_for_event[groups_unlisted <= n_events]
        event_to_group[valid_events] <- valid_groups

        ## fill ungrouped (zero) positions with last group index
        event_to_group[event_to_group == 0L] <- n_groups

        ## index into param by group assignment
        return(param[event_to_group])
    }

    ## standard recycling for "distinct" or "ensemble"
    recycle_to_length(param, n_events, "event", verbose)
}


#' Convert event indices and spans to interval specifications
#' @keywords internal
specify_intervals <- function(
    time_vec,
    event_indices,
    span, ## as list
    verbose = TRUE
) {
    n_obs <- length(time_vec)
    span_before <- vapply(span, `[`, numeric(1), 1L)
    span_after <- vapply(span, `[`, numeric(1), 2L)
    event_times <- time_vec[event_indices]
    ## calculate boundary times
    start_times <- event_times + span_before ## negative span specifies before
    end_times <- event_times + span_after
    ## convert to boundary indices
    start_idx <- findInterval(start_times, time_vec) ## greatest idx LTE start_times
    end_idx <- findInterval(end_times, time_vec)

    # Check for entirely out of bounds intervals
    ## TODO also excludes intervals of 1 sample at start/end of data
    entirely_oob <- end_idx <= 1L | start_idx >= n_obs

    if (any(entirely_oob)) {
        oob_ids <- which(entirely_oob)
        n_oob <- qty(length(oob_ids))
        cli_abort(c(
            "x" = "{n_oob} Interval{?s} {.val {oob_ids}} {n_oob} {?is/are} \\
            entirely outside data bounds.",
            "i" = "Intervals must be specified within existing data bounds."
        ))
    }

    ## check for partial bounds, only for alert info
    partial_oob <- start_idx < 1L | end_idx > n_obs
    if (verbose && any(partial_oob)) {
        oob_ids <- which(partial_oob)
        n_oob <- qty(length(oob_ids))
        cli_warn(c(
            "!" = "{n_oob} Interval{?s} {.val {oob_ids}} {n_oob} {?is/are} \\
            partially outside data bounds.",
            "i" = "Returning available data only."
        ))
    }

    # Clip to valid range
    start_idx <- pmax(1L, start_idx)
    end_idx <- pmin(n_obs, end_idx)

    interval_spec <- data.frame(
        event_indices = event_indices,
        event_times = event_times,
        span_before = span_before,
        span_after = span_after,
        start_times = start_times,
        end_times = end_times,
        start_idx = start_idx,
        end_idx = end_idx,
        stringsAsFactors = FALSE
    )

    return(interval_spec)
}


#' Extract interval data by index
#' @keywords internal
extract_interval_list <- function(
    data,
    interval_spec,
    nirs_channels ## as list
) {
    n_vec <- seq_len(nrow(interval_spec))
    interval_list <- lapply(n_vec, \(.i) {
        ## local indices for event
        idx_range <- interval_spec$start_idx[.i]:interval_spec$end_idx[.i]
        interval_data <- data[idx_range, , drop = FALSE]

        ## return interval_data with metadata
        create_mnirs_data(
            interval_data,
            nirs_channels = nirs_channels[[.i]], ## overwrite for interval data
            event_times = interval_spec$event_times[.i],
            interval_span = c(
                interval_spec$span_before[.i],
                interval_spec$span_after[.i]
            )
        )
    })

    names(interval_list) <- sprintf("interval_%d", n_vec)
    return(interval_list)
}


#' Recalculate time_channel values with zero offset at event time (t0)
#' @keywords internal
zero_offset_data <- function(data, time_channel, t0) {
    ## zero time channel to event_time `t0`
    data[[time_channel]] <- data[[time_channel]] - t0
    return(data)
}


#' Ensemble average multiple intervals
#' @keywords internal
ensemble_intervals <- function(
    interval_list,
    nirs_channels,
    metadata,
    verbose = TRUE
) {
    time_channel <- metadata$time_channel
    sample_rate <- metadata$sample_rate
    ## extract data & metadata from interval_list
    interval_data <- lapply(interval_list, \(.df) {
        event_time <- attr(.df, "event_times")
        time_channel <- attr(.df, "time_channel")
        ## return data & metadata from each interval
        list(
            ## ensemble-average time values makes no sense, so return zero-offset
            data = zero_offset_data(.df, time_channel, event_time),
            event_time = event_time,
            interval_span = attr(.df, "interval_span")
        )
    })

    ## stack interval data frames
    df_long <- do.call(rbind, lapply(interval_data, `[[`, "data"))
    ## resample times to nearest estimated sample rate for binned ensembling
    time_resampled <- round(df_long[[time_channel]] * sample_rate) / sample_rate
    ## split row indices by unique time
    time_groups <- split(seq_len(nrow(df_long)), time_resampled)
    unique_times <- as.numeric(names(time_groups))

    ## warn if any time samples have only one value, implying irregular
    ## samples and may result in alternating samples instead of ensemble-means
    if (verbose && min(lengths(time_groups), na.rm = TRUE) < 2) {
        cli_warn(c(
            "!" = "Duplicate or irregular {.arg time_channel} samples \\
            detected after ensemble-averaging.",
            "i" = "Check your resulting data for inconsistent results.",
            "i" = "Re-sample with {.fn mnirs::resample_mnirs}."
        ))
    }

    col_n <- length(nirs_channels)
    ## nirs_channel-wise means per unique time matrix operation
    ## nirs_channel must be vectorised and exist in the interval_data
    channel_matrix <- as.matrix(df_long[, nirs_channels, drop = FALSE])
    result_matrix <- vapply(time_groups, \(.idx) {
        colMeans(channel_matrix[.idx, , drop = FALSE], na.rm = TRUE)
    }, numeric(col_n))

    result <- data.frame(
        setNames(list(unique_times), time_channel),
        setNames(
            as.data.frame(if (col_n == 1L) result_matrix else t(result_matrix)),
            nirs_channels
        )
    )

    ## add metadata
    result <- create_mnirs_data(
        result,
        nirs_device = attr(df_long, "nirs_device"),
        # nirs_channels = unique(c(metadata$nirs_channels, nirs_channels)),
        nirs_channels = unique(nirs_channels),
        time_channel = time_channel,
        event_channel = attr(df_long, "event_channel"),
        sample_rate = sample_rate,
        event_times = lapply(interval_data, `[[`, "event_time"),
        interval_span = lapply(interval_data, `[[`, "interval_span")
    )

    return(result)
}


#' Apply grouping to intervals
#' @keywords internal
group_intervals <- function(
    interval_list,
    nirs_channels,
    metadata,
    group_events,
    zero_time = TRUE,
    verbose = TRUE
) {
    time_channel <- metadata$time_channel
    n_intervals <- length(interval_list)

    ## return distinct intervals
    if (n_intervals == 1L || group_events[[1L]][1L] == "distinct") {
        result <- lapply(interval_list, \(.df) {
            if (zero_time) {
                event_time <- attr(.df, "event_times")
                .df <- zero_offset_data(.df, time_channel, event_time)
            }

            create_mnirs_data(
                .df,
                nirs_device = metadata$nirs_device,
                # nirs_channels = unique(c(
                #     metadata$nirs_channels,
                #     attr(.df, "nirs_channels")
                # )),
                nirs_channels = unique(attr(.df, "nirs_channels")),
                time_channel = time_channel,
                event_channel = metadata$event_channel,
                sample_rate = metadata$sample_rate,
                event_times = attr(.df, "event_time"),
                interval_span = attr(.df, "interval_span")
            )
        })

        return(result)
    }

    ## return ensembled intervals
    if (group_events[[1L]][1L] == "ensemble") {
        all_nirs <- unique(unlist(nirs_channels))
        result <- list(
            ensemble = ensemble_intervals(
                interval_list,
                nirs_channels = all_nirs,
                metadata,
                verbose
            )
        )
        # attributes(result[[1]])
        return(result)
    }

    ## custom grouping ===================================
    ## find ungrouped intervals
    grouped_ids <- unlist(group_events)
    ungrouped_ids <- setdiff(seq_len(n_intervals), grouped_ids)

    ## add ungrouped ids as individual groups and fuzzy sort list
    if (length(ungrouped_ids) > 0) {
        group_events <- c(group_events, as.list(ungrouped_ids))
        group_events <- group_events[
            order(vapply(group_events, \(.x) {
                median(.x, na.rm = TRUE)
            }, FUN.VALUE = numeric(1))) ## TODO confirm length == 0 always
        ]
        if (verbose) {
            cli_inform(c(
                "!" = "Intervals detected not in {.arg group_events}.",
                "i" = "Ungrouped intervals included as discrete."
            ))
        }
    }

    ## check and warn for duplicated intervals in groups
    dup <- grouped_ids[duplicated(grouped_ids)]
    if (verbose && length(dup) > 0) {
        cli_warn(c(
            "!" = "Duplicates detected of {qty(length(dup))} \\
            interval{?s} {.val {dup}}.",
            "i" = "Re-specify {.arg group_events} to remove duplicates."
        ))
    }

    ## process by group
    result <- lapply(group_events, \(.g) {
        if (length(.g) == 1L) {
            ## single interval return as-is
            df <- interval_list[[.g]]
            if (zero_time) {
                event_time <- attr(df, "event_times")
                df <- zero_offset_data(df, time_channel, event_time)
            }
            df
        } else {
            ## return ensembled intervals
            group_nirs <- unique(unlist(nirs_channels[.g]))
            ensemble_intervals(
                interval_list[.g],
                nirs_channels = group_nirs,
                metadata,
                verbose
            )
        }
    })

    ## TODO do I want to name by interval or by group?
    names(result) <- vapply(group_events, \(.g) {
        paste0("interval_", paste(.g, collapse = "_"))
    }, character(1))

    return(result)
}
