#' Shift data range
#'
#' Move the range of data channels in a data frame up or down, while preserving
#' the absolute amplitude/dynamic range of each channel, and the relative
#' scaling across channels. e.g. shift the minimum data value to zero for all
#' positive values, or shift the mean of the first time span in a recording
#' to zero.
#'
#' @param nirs_channels A `list()` of character vectors indicating the column
#'   names for data channels to be shifted (see *Details*).
#'   \describe{
#'      \item{`list("A", "B", "C")`}{Will shift each channel independently,
#'      losing the relative scaling between channels.}
#'      \item{`list(c("A", "B", "C"))`}{Will shift all channels together,
#'      preserving the relative scaling between channels.}
#'      \item{`list(c("A", "B"), c("C", "D"))`}{Will shift channels `A` and `B`
#'      in one group, and channels `C` and `D` in another group, preserving
#'      relative scaling within, but not between groups.}
#'   }
#'   Must match column names in data exactly. Will be taken from metadata if
#'   not defined explicitly.
#' @param to A numeric value in units of `nirs_channels` to which the data
#'   channels will be shifted, e.g. shift the minimum value to zero.
#' @param by A numeric value in units of `nirs_channels` by which the data
#'   channels will be shifted, e.g. shift all values up by 10 units.
#' @param width An integer value defining the number of samples centred on
#'   `idx` over which the operation will be performed.
#' @param span A numeric value in units of `time_channel` defining the timespan
#'   centred on `idx` over which the operation will be performed.
#' @param position Indicates where the reference values will be shifted from.
#'   \describe{
#'      \item{`"min"`}{(The *default*) will shift the minimum value(s) `to`
#'      or `by` the specified value.}
#'      \item{`"max"`}{Will shift the maximum value(s) `to` or `by` the
#'      specified values.}
#'      \item{`"first"`}{Will shift first value(s) `to` or `by` the specified
#'      values.}
#'   }
#' @inheritParams validate_mnirs
#'
#' @details
#' `nirs_channels = list()` can be used to group data channels to preserve
#'   absolute or relative scaling.
#'
#' - Channels grouped together in a list item will be shifted to a common
#'   value, and the relative scaling within that group will be preserved.
#'
#' - Channels grouped in separate list items will be shifted independently,
#'   and relative scaling between groups will be lost.
#'
#' - Channels (columns) in `data` not explicitly defined in `nirs_channels`
#'   will be passed through untouched to the output data frame.
#'
#' `nirs_channels` and `time_channel` can be retrieved automatically from
#'   `data` of class *"mnirs"* which has been processed with `{mnirs}`,
#'   if not defined explicitly. This will default to returning all
#'   `nirs_channels` grouped together, and should be defined explicitly
#'   for other grouping arrangements.
#'
#' Only one of either `to` or `by` and one of either `width` or `span` should
#'   be defined. If both of either pairing are defined, `to` will be preferred
#'   over `by`, and `width` will be preferred over `span`.
#'
#' @returns
#' A [tibble][tibble::tibble-package] of class *"mnirs"* with metadata
#'   available with `attributes()`.
#'
#' @examples
#' ## read example data
#' data <- read_mnirs(
#'     file_path = example_mnirs("moxy_ramp"),
#'     nirs_channels = c(smo2 = "SmO2 Live"),
#'     time_channel = c(time = "hh:mm:ss"),
#'     verbose = FALSE
#' ) |>
#'     resample_mnirs(verbose = FALSE) |>
#'     replace_mnirs(
#'         invalid_values = c(0, 100),
#'         outlier_cutoff = 3,
#'         width = 7,
#'         verbose = FALSE
#'     ) |>
#'     filter_mnirs(na.rm = TRUE, verbose = FALSE)
#'
#' data_shifted <- shift_mnirs(
#'     data,
#'     # nirs_channels = NULL, ## taken from metadata
#'     to = 0,                 ## NIRS values will be shifted to zero
#'     span = 120,             ## shift the first 120 sec of data to zero
#'     position = "first",
#'     verbose = FALSE
#' )
#'
#' \dontrun{
#' plot(data_shifted, display_timestamp = TRUE) +
#'     geom_hline(yintercept = 0, linetype = "dotted")
#' }
#'
#' @export
shift_mnirs <- function(
        data,
        nirs_channels = list(),
        time_channel = NULL,
        to = NULL,
        by = NULL,
        width = NULL,
        span = NULL,
        position = c("min", "max", "first"),
        verbose = TRUE
) {
    ## TODO convert sym(nirs_channels) to strings?
    ## TODO need to fix edges where only half width/span included

    ## validation =============================================
    ## do nothing condition
    if (is.null(c(to, by))) {
        cli_abort("One of either {.arg to} or {.arg by} must be defined.")
    }

    validate_mnirs_data(data)
    metadata <- attributes(data)
    nirs_channels <- validate_nirs_channels(data, nirs_channels, verbose)
    time_channel <- validate_time_channel(data, time_channel)
    validate_numeric(to, 1, msg = "one-element")
    validate_numeric(by, 1, msg = "one-element")
    if (!is.null(to) && !is.null(by)) {
        by <- NULL
        if (verbose) {
            cli_warn(c(
                "Either {.arg to} or {.arg by} should be defined, \\
                not both.",
                "i" = "Defaulting to {.arg to} = {.val {to}}"
            ))
        }
    }
    position <- match.arg(position)

    nirs_listed <- if (is.list(nirs_channels)) {
        nirs_channels
    } else {
        list(nirs_channels)
    }
    nirs_unlisted <- unlist(nirs_listed, use.names = FALSE)

    ## Metadata =================================
    metadata$nirs_channels <- unique(c(metadata$nirs_channels, nirs_unlisted))
    metadata$time_channel <- time_channel
    metadata$verbose <- verbose

    ## shift_by ====================================================
    ## shift_by does not require grouping or calculating positions
    if (!is.null(by) && is.null(to)) {
        data[nirs_unlisted] <- data[nirs_unlisted] + by
        return(create_mnirs_data(data, metadata))
    }

    ## calculate shift_to values ====================================
    time_vec <- round(data[[time_channel]], 6)

    if (position == "first") {
        ## take data <= first time_channel value + span, assuming sorted
        head_idx <- time_vec <= (time_vec[1L] + span)
        ## drop = FALSE to avoid reducing to vector with one `nirs_unlisted`
        shift_values <- colMeans(
            data[head_idx, nirs_unlisted, drop = FALSE],
            na.rm = TRUE
        )
    } else if (position %in% c("min", "max")) {
        ## find local windows within width/span centred around idx
        ## TODO need to fix edges where only half width/span included
        window_idx <- compute_local_windows(
            t = time_vec,
            width = width,
            span = span,
            method = "centred"
        )
        shift_fun <- get(position)
        ## compute min or max along local means
        ## return named vec of min/max for each nirs_channel
        shift_values <- vapply(data[nirs_unlisted], \(.x) {
            shift_fun(compute_local_fun(.x, window_idx, mean), na.rm = TRUE)
        }, numeric(1))
    }

    ## apply shifts ==============================================
    ## find the value to shift to per nirs_channel group
    group_shift_values <- vapply(nirs_listed, \(.cols) {
        switch(
            position,
            min = min(shift_values[.cols]),
            max = max(shift_values[.cols]),
            first = mean(shift_values[.cols])
        )
    }, numeric(1))

    ## expand grouped shift values for each nirs_channel
    channel_shifts <- rep(group_shift_values, lengths(nirs_listed))
    ## move the shift_value to the `to` value for each nirs_channel
    data[nirs_unlisted] <- lapply(seq_along(nirs_unlisted), \(.i) {
        data[[nirs_unlisted[.i]]] - channel_shifts[.i] + to
    })

    return(create_mnirs_data(data, metadata))
}
