#' Extract intervals from *{mnirs}* data
#'
#' Detects and extracts intervals around specified events from time series
#' data for analysis.
#'
#' @param nirs_channels A `list()` of character vectors indicating mNIRS
#'   channel names to operate on within each interval (see *Details*). Must
#'   match column names in `data` exactly. Retrieved from metadata if not
#'   defined explicitly.
#' @param time_channel A character string indicating the time or sample channel
#'   name. Must match column names in `data` exactly. Retrieved from metadata
#'   if not defined explicitly.
#' @param event_channel An *optional* character string indicating the event or
#'   lap channel name. Required if `event_labels` are specified. Must
#'   match column names in `data` exactly. Retrieved from metadata if not
#'   defined explicitly.
#' @param event_times A numeric vector of `time_channel` values indicating
#'   event starts (see *Details*).
#' @param event_labels A character vector of strings to match from
#'   `event_channel`, indicating event starts.
#' @param event_samples An integer vector with sample indices (row numbers)
#'   indicating event starts.
#' @param span A `list()` of two-element numeric vectors specifying the
#'   interval around each event as `c(before, after)`, in units of
#'   `time_channel` (see *Details*).
#' @param group_events Either a character string or a `list()` of numeric
#'   vectors specifying how to group intervals. Interval numbers are sorted
#'   in order of appearance within `data`.
#'   \describe{
#'      \item{`"distinct"`}{Will extract each interval as an independent
#'      data frame (the *default*).}
#'      \item{`"ensemble"`}{Will perform ensemble-averaging across all
#'      detected intervals for each `nirs_channel`, and return a single
#'      data frame.}
#'      \item{`list(c(1, 2), c(3, 4))`}{Will perform ensemble-averaging across
#'      intervals within each group and return a data frame for each group.}
#'   }
#' @inheritParams read_mnirs
#' @inheritParams validate_mnirs
#'
#' @details
#' `nirs_channels = list()` can be used to specify unique data channels
#'   (column names) to operate on within intervals, according to `group_events`
#'   (see below).
#'
#' - Channels can be specified in seperate list items (e.g.
#'   `list(c("A", "B", "C"), "B", c("A", "C"))`) to include or exclude from
#'   specific intervals. In this way, bad data channels in a single interval
#'   can be excluded from contaminating the ensemble-averaging.
#'
#' `event_*` arguments can be used to identify intervals of interest in `data`,
#'   and can be specified three ways (methods can be combined):
#'
#' \describe{
#'   \item{`event_times`}{Numeric time values in units of `time_channel`.}
#'   \item{`event_samples`}{Integer sample indices (row numbers).}
#'   \item{`event_labels`}{Character patterns to match in `event_channel`,
#'   case-sensitive and must match exactly.}
#' }
#'
#' - Every unique event detected in `data` will be extracted according to the
#'   `span` interval window around it (see below). Events can be specified in
#'   any order. The detected intervals will be extracted and returned in the
#'   order in which they appear.
#'
#' `span = list()` will accept two-element numeric vectors indicating the
#'   interval time window around each detected event, in units of
#'   `time_channel`, e.g. seconds.
#'
#' - `span = c(before, after)` will typically have a *negative* `before` value,
#'   extending the interval window *before* the target event, and a *positive*
#'   `after` value, extending the interval window *after* the target event,
#'   resulting in an interval window between
#'   `[event - span[1], event + span[2]]`. However, `span` will accept any
#'   positive or negative values with reference to the target event, as long
#'   as the range is contained within the bounds of the available `data`.
#'
#' - Example for three intervals:
#'   \describe{
#'      \item{`event_times = list(60, 120, 300)`}{}
#'      \item{`span = list(c(-30, 30), c(30, 90), c(-60, 0))`}{
#'      - Interval 1 would range from `30` to `90` seconds.
#'      - Interval 2 would range from `150` to `210` seconds.
#'      - Interval 3 would range from `240` to `300` seconds.}
#'   }
#'
#' `group_events = list()` can be used to specify the grouping structure of
#'   intervals and return either discrete or ensemble-averaged intervals.
#'
#' \describe{
#'   \item{`"distinct"`}{Will extract each interval as an independent
#'   data frame.}
#'   \item{`"ensemble"`}{Will perform ensemble-averaging across all
#'   detected intervals for each `nirs_channel`, and return a single
#'   data frame.}
#'   \item{`list(c(1, 2), c(3, 4))`}{Will perform ensemble-averaging across
#'   intervals within each group and return a data frame for each group.}
#' }
#'
#' - Any interval numbers omitted from `group_events` will be extracted as a
#'   distinct data frame. Interval numbers beyond the number of detected
#'   intervals will be ignored.
#'
#' - `group_events` will accept a named list and pass on those names to the
#'   list of returned data frames `<under development>`, e.g.
#'   \describe{
#'      \item{`group_events = list(low = c(1, 2), high = c(3, 4))`}{}
#'   }
#'   Otherwise, default names will be returned as `"interval_1"`,
#'   `"interval_2"`, etc. for distinct intervals; `"ensemble"` for
#'   ensemble-averaging across all intervals; or `"group_1_2"` etc. for
#'   custom grouping structure.
#'
#' List items or a single vector (e.g. `nirs_channels = list(c("A", "B"))` or
#'   `span = c(-30, 30)`) will be recycled forward for all intervals. If the
#'   number of intervals detected exceeds the number of list items, the last
#'   specified list item will be recycled forward to the additional intervals.
#'   List items beyond the number of detected intervals will be ignored.
#'
#' `zero_time = TRUE` will re-calculate numeric `time_channel` values
#'   to start from zero at the target event, for each interval. Ensemble-
#'   averaged interval times will always be zeroed. Effectively, this works best
#'   when the event marker indicates the start of the interval. This can
#'   return unexpected time values when the event marker itself is not
#'   included in the interval range (e.g. with `span = c(30, 60)`).
#'
#' @returns A named `list()` of [tibbles][tibble::tibble-package] of class
#'   *"mnirs"* with metadata available with `attributes()`.
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#'
#' options(mnirs.verbose = FALSE)
#'
#' ## read example data
#' data <- read_mnirs(
#'     example_mnirs("train.red"),
#'     nirs_channels = c(
#'         smo2_left = "SmO2 unfiltered",
#'         smo2_right = "SmO2 unfiltered"
#'     ),
#'     time_channel = c(time = "Timestamp (seconds passed)"),
#'     zero_time = TRUE
#' ) |>
#'     resample_mnirs() ## avoid issues ensemble-averaging irregular samples
#'
#' ## extract intervals as a list of data frames
#' extract_intervals(
#'     data,
#'     nirs_channels = list(c(smo2_left, smo2_right)),
#'     event_times = c(368, 1093), ## specify interval events
#'     span = list(c(-20, 90)), ## specify the event start-end timespans
#'     group_events = "distinct", ## return all unique intervals
#'     zero_time = TRUE ## start time from zero
#' )
#'
#' ## ensemble-average across multiple intervals
#' interval_list <- extract_intervals(
#'     data,
#'     nirs_channels = list(c(smo2_left, smo2_right)),
#'     event_times = c(368, 1093),
#'     span = list(c(-20, 90)),
#'     group_events = "ensemble", ## return ensemble-averaged intervals
#'     zero_time = TRUE
#' )
#'
#' library(ggplot2)
#' plot(interval_list[[1L]], label_time = TRUE) +
#'     geom_vline(xintercept = 0, linetype = "dotted")
#'
#' @export
extract_intervals <- function(
    data,
    nirs_channels = list(NULL),
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
    ## TODO can I be more efficient with metadata?
    validate_mnirs_data(data)
    metadata <- attributes(data)
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }
    nirs_channels <- validate_nirs_channels(
        enquo(nirs_channels), data, verbose
    )
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

    ## TODO validate event_labels as numeric or character?
    ## if `event_labels` provided, `event_channel` must be provided
    if (!is.null(event_labels)) {
        event_channel <- validate_event_channel(
            event_channel, data, required = TRUE
        )
        ## TODO redundant check but better error message
        # cli_abort(c(
        #     "x" = "{.arg event_channel} is required when using \\
        #     {.arg event_labels}.",
        #     "i" = "Specify column name containing event labels."
        # ))
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


#' Recalculate time_channel values with zero offset at event time (x0)
#' @keywords internal
zero_offset_data <- function(data, time_channel, x0) {
    ## zero time channel to event_time `x0`
    data[[time_channel]] <- data[[time_channel]] - x0
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
        stats::setNames(list(unique_times), time_channel),
        stats::setNames(
            as.data.frame(if (col_n == 1L) result_matrix else t(result_matrix)),
            nirs_channels
        )
    )

    ## add metadata
    ## TODO necessary to sort by time?
    result <- create_mnirs_data(
        result,
        nirs_device = attr(df_long, "nirs_device"),
        nirs_channels = unique(c(metadata$nirs_channels, nirs_channels)),
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
                nirs_channels = unique(c(
                    metadata$nirs_channels,
                    attr(.df, "nirs_channels")
                )),
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
