#' Specify interval boundaries by time, label, lap, or sample
#'
#' Helper functions to define interval start or end boundaries for
#' [extract_intervals()].
#'
#' @param ... Specify start or end boundaries.
#'   \describe{
#'     \item{`by_time(...)`}{Numeric time values in units of `time_channel`.}
#'     \item{`by_label(...)`}{Character strings to match in `event_channel`.
#'     All matching occurrences are returned.}
#'     \item{`by_lap(...)`}{Integer lap numbers to match in `event_channel`.
#'     For `start`, resolves to the first sample of each lap. For `end`,
#'     resolves to the last sample.}
#'     \item{`by_sample(...)`}{Integer sample indices (row numbers).}
#'   }
#'
#' @details
#' These helpers can be used explicitly for arguments `start`/`end`, or raw
#' values can be passed directly:
#'   - Numeric → [by_time()]
#'   - Character → [by_label()], 
#'   - Explicit integer (e.g. `2L`) → [by_lap()]. 
#'   - Use [by_sample()] explicitly for sample indices.
#'
#' @returns An object of class `"mnirs_interval"` for use with the `start`
#'   and `end` arguments of [extract_intervals()].
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
#'     event_channel = c(lap = "Lap/Event"),
#'     zero_time = TRUE,
#'     verbose = FALSE
#' )
#'
#' ## start and end by time
#' extract_intervals(data, start = by_time(66), end = by_time(357))
#'
#' ## start by lap
#' extract_intervals(data, start = by_lap(2, 4), span = 0)
#'
#' ## introduce event_channel with "start" string
#' data$event <- NA_character_
#' data$event[1000] <- "start"
#' data <- create_mnirs_data(data, event_channel = "event")
#' 
#' ## start by label, end by time
#' extract_intervals(data, start = by_label("start"), end = by_time(1500))
#' 
#' ## multiple intervals by sample index
#' extract_intervals(data, start = by_sample(1000, 1500), end = by_sample(2000, 2600))
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


#' @rdname by_time
#' @export
by_lap <- function(...) {
    by_lap <- c(...)
    validate_numeric(by_lap, range = c(1, Inf), integer = TRUE)
    structure(
        list(type = "lap", by_lap = as.integer(by_lap)),
        class = "mnirs_interval"
    )
}


#' coerce raw values to mnirs_interval objects
#' @param x A raw value or mnirs_interval object.
#' @param arg Name of the argument for error messages.
#' @keywords internal
as_mnirs_interval <- function(x, arg = "start") {
    if (is.null(x) || inherits(x, "mnirs_interval")) {
        return(x)
    }
    ## integer before numeric — integers are also numeric in R
    if (is.integer(x)) {
        return(by_lap(x))
    }
    if (is.numeric(x)) {
        return(by_time(x))
    }
    if (is.character(x)) {
        return(by_label(x))
    }
    cli_abort(
        "{.arg {arg}} must be {.cls numeric}, {.cls integer}, \\
        {.cls character}, or a {.fn by_time}, {.fn by_sample}, \\
        {.fn by_label}, {.fn by_lap} specification."
    )
}


#' recycle a single-element span to c(before, after)
#' positive → c(0, x), negative → c(x, 0)
#' @keywords internal
recycle_span <- function(span) {
    if (is.numeric(span) && length(span) == 1L) {
        if (span >= 0) {
            return(c(0, span))
        } else {
            return(c(span, 0))
        }
    }
    if (!is.numeric(span) || length(span) != 2L) {
        cli_abort(c(
            "x" = "{.arg span} must be a one- or two-element {.cls numeric} \\
            vector."
        ))
    }
    return(span)
}


#' resolve a single mnirs_interval object to integer row indices
#' @keywords internal
resolve_interval_indices <- function(
    interval,
    time_vec,
    event_vec = NULL,
    position = c("first", "last")
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
        },
        lap = {
            vapply(interval$by_lap, \(lap_val) {
                event_vec <- as.integer(event_vec)
                matches <- which(event_vec == lap_val)
                if (length(matches) == 0L) {
                    cli_abort(c(
                        "x" = "No samples found for lap {.val {lap_val}}.",
                        "i" = "Check that {.arg event_channel} contains \\
                        lap numbers."
                    ))
                }
                if (position == "first") {
                    matches[1L]
                } else {
                    matches[length(matches)]
                }
            }, integer(1))
        }
    )
}


#' resolve start/end into raw index vectors (no span applied)
#' @keywords internal
resolve_interval <- function(
    start_interval,
    end_interval,
    time_vec,
    event_vec = NULL
) {
    has_start <- !is.null(start_interval)
    has_end <- !is.null(end_interval)
    interval <- start_interval %||% end_interval

    ## single-boundary lap: resolve to full lap (first + last)
    if (interval$type == "lap" && xor(has_start, has_end)) {
        start_idx <- resolve_interval_indices(
            interval, time_vec, event_vec, position = "first"
        )
        end_idx <- resolve_interval_indices(
            interval, time_vec, event_vec, position = "last"
        )
        return(list(
            start_idx = start_idx,
            end_idx = end_idx,
            has_start = TRUE,
            has_end = TRUE
        ))
    }

    ## single-boundary non-lap: reference point for span window
    if (xor(has_start, has_end)) {
        ref_idx <- resolve_interval_indices(
            interval, time_vec, event_vec,
            position = if (has_start) "first" else "last"
        )
        return(list(
            start_idx = ref_idx,
            end_idx = NULL,
            has_start = has_start,
            has_end = has_end
        ))
    }

    ## both boundaries specified
    start_idx <- resolve_interval_indices(
        start_interval, time_vec, event_vec, position = "first"
    )
    end_idx <- resolve_interval_indices(
        end_interval, time_vec, event_vec, position = "last"
    )

    ## warn and truncate if lengths differ — only paired intervals
    n_start <- length(start_idx)
    n_end <- length(end_idx)
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

    return(list(
        start_idx = start_idx,
        end_idx = end_idx,
        has_start = TRUE,
        has_end = TRUE
    ))
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
recycle_param <- function(param, n_events, event_groups, verbose = TRUE) {
    ## flatten nested lists to single-depth list
    param <- if (is.list(param)) {
        lapply(param, \(.x) if (is.list(.x)) unlist(.x) else .x)
    } else {
        list(param)
    }

    ## custom grouping: recycle per group, then map to event order
    if (is.numeric(event_groups[[1L]])) {
        n_groups <- length(event_groups)
        groups_unlisted <- unlist(event_groups)

        ## recycle param to number of groups
        param <- recycle_to_length(param, n_groups, "group", verbose)

        ## create mapping:  event_id -> group_id
        ## rep(1:n_groups, lengths(event_groups)) gives group index per event in event_groups
        group_for_event <- rep(seq_len(n_groups), lengths(event_groups))

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
    return(recycle_to_length(param, n_events, "event", verbose))
}


#' apply span to raw indices and build interval_spec data frame
#' @keywords internal
apply_span_to_indices <- function(
    interval_idx,
    time_vec,
    span,
    verbose = TRUE
) {
    n_obs <- length(time_vec)
    n_intervals <- length(interval_idx$start_idx)

    ## extract span values (already recycled to n_intervals by recycle_param)
    span_before <- vapply(span, `[`, numeric(1), 1L)
    span_after <- vapply(span, `[`, numeric(1), 2L)
    event_times <- time_vec[pmin(interval_idx$start_idx, n_obs)]

    if (interval_idx$has_start && interval_idx$has_end) {
        ## span[1] shifts starts, span[2] shifts ends
        start_times <- time_vec[pmin(interval_idx$start_idx, n_obs)] +
            span_before
        end_times <- time_vec[pmin(interval_idx$end_idx, n_obs)] + span_after
        ## two-element c(start, end) when both boundaries defined
        interval_times <- Map(
            c, 
            time_vec[interval_idx$start_idx], 
            time_vec[interval_idx$end_idx]
        )
    } else {
        ## start-only or end-only: both span values apply to the reference
        start_times <- event_times + span_before
        end_times <- event_times + span_after
        ## stored as a list-column to allow variable length per row
        interval_times <- as.list(event_times)
    }

    ## convert boundary times to boundary indices
    ## greatest idx LTE start_times
    start_idx <- findInterval(start_times, time_vec)
    end_idx <- findInterval(end_times, time_vec)

    ## check for entirely out of bounds intervals
    entirely_oob <- end_idx <= 0L | start_idx >= n_obs
    if (any(entirely_oob)) {
        oob_ids <- which(entirely_oob)
        n_oob <- qty(length(oob_ids))
        cli_abort(c(
            "x" = "{n_oob} Interval{?s} {.val {oob_ids}} {n_oob} {?is/are} \\
            entirely outside data bounds.",
            "i" = "Intervals must be specified within existing data bounds."
        ))
    }

    ## check for partial out of bounds
    partial_oob <- start_idx < 1L | end_idx > n_obs
    ## reduce to valid range
    if (any(partial_oob)) {
        start_idx <- pmax(1L, start_idx)
        end_idx <- pmin(n_obs, end_idx)

        if (verbose) {
            oob_ids <- which(partial_oob)
            n_oob <- qty(length(oob_ids))
            cli_warn(c(
                "!" = "{n_oob} Interval{?s} {.val {oob_ids}} {n_oob} \\
                {?is/are} partially outside data bounds.",
                "i" = "Returning available data only."
            ))
        }
    }

    result <- data.frame(
        span_before = span_before,
        span_after = span_after,
        start_times = start_times,
        end_times = end_times,
        start_idx = start_idx,
        end_idx = end_idx,
        stringsAsFactors = FALSE
    )
    result$interval_times <- interval_times
    return(result)
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
            interval_times = interval_spec$interval_times[[.i]],
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
    ## zero time channel to start_time `t0`
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
        interval_times <- attr(.df, "interval_times")
        time_channel <- attr(.df, "time_channel")
        ## zero-offset to start time (first element of interval_times)
        t0 <- interval_times[[1L]]
        ## return data & metadata from each interval
        list(
            ## ensemble-average time values makes no sense, so return zero-offset
            data = zero_offset_data(.df, time_channel, t0),
            interval_times = interval_times - t0, ## zero_time recalc
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
            ## janky solution to different default dimensions as single...
            ## vs multiple column matrix
            as.data.frame(if (col_n == 1L) result_matrix else t(result_matrix)),
            nirs_channels
        )
    )

    ## return with metadata
    return(
        create_mnirs_data(
            result,
            nirs_device = metadata$nirs_device,
            nirs_channels = unique(nirs_channels),
            time_channel = time_channel,
            event_channel = metadata$event_channel,
            sample_rate = sample_rate,
            start_timestamp = metadata$start_timestamp,
            interval_times = lapply(interval_data, `[[`, "interval_times"),
            interval_span = lapply(interval_data, `[[`, "interval_span")
        )
    )
}


#' Apply grouping to intervals
#' @keywords internal
group_intervals <- function(
    interval_list,
    nirs_channels,
    metadata,
    event_groups,
    zero_time = FALSE,
    verbose = TRUE
) {
    time_channel <- metadata$time_channel
    n_intervals <- length(interval_list)

    ## return distinct intervals
    if (n_intervals == 1L || event_groups[[1L]][1L] == "distinct") {
        result <- lapply(interval_list, \(.df) {
            if (zero_time) {
                interval_times <- attr(.df, "interval_times")
                t0 <- interval_times[[1L]]
                .df <- zero_offset_data(.df, time_channel, t0)
                ## also zero_time recalc `interval_times`
                attr(.df, "interval_times") <- interval_times - t0
            }

            create_mnirs_data(
                .df,
                nirs_device = metadata$nirs_device,
                nirs_channels = unique(attr(.df, "nirs_channels")),
                time_channel = time_channel,
                event_channel = metadata$event_channel,
                sample_rate = metadata$sample_rate,
                start_timestamp = metadata$start_timestamp,
                interval_times = attr(.df, "interval_times"),
                interval_span = attr(.df, "interval_span")
            )
        })

        return(result)
    }

    ## return ensembled intervals
    if (event_groups[[1L]][1L] == "ensemble") {
        all_nirs <- unique(unlist(nirs_channels))
        result <- list(
            ensemble = ensemble_intervals(
                interval_list,
                nirs_channels = all_nirs,
                metadata,
                verbose
            )
        )

        return(result)
    }

    ## custom grouping ===================================
    ## find ungrouped intervals
    grouped_ids <- unlist(event_groups)
    ungrouped_ids <- setdiff(seq_len(n_intervals), grouped_ids)

    ## add ungrouped ids as individual groups and fuzzy sort list
    if (length(ungrouped_ids) > 0) {
        event_groups <- c(event_groups, as.list(ungrouped_ids))
        event_groups <- event_groups[
            order(vapply(event_groups, \(.x) {
                median(.x, na.rm = TRUE)
            }, numeric(1))) ## TODO confirm length == 0 always
        ]
        if (verbose) {
            cli_inform(c(
                "!" = "Intervals detected not specified in \\
                {.arg event_groups}.",
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
            "i" = "Re-specify {.arg event_groups} to remove duplicates."
        ))
    }

    ## process by group
    result <- lapply(event_groups, \(.g) {
        if (length(.g) == 1L) {
            ## single interval return as-is
            df <- interval_list[[.g]]
            if (zero_time) {
                interval_times <- attr(df, "interval_times")
                t0 <- interval_times[[1L]]
                df <- zero_offset_data(df, time_channel, t0)
                df <- create_mnirs_data(
                    df,
                    interval_times = interval_times - t0 ## zero_time recalc
                )
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

    names(result) <- vapply(event_groups, \(.g) {
        paste0("interval_", paste(.g, collapse = "_"))
    }, character(1))

    return(result)
}
