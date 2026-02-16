#' Analyse mNIRS kinetics across intervals
#' 
#' Perform kinetics analysis with parametric curve fitting or non-parametric
#' estimation for `nirs_channels` within an *"mnirs"* data frame or a list of 
#' data frames.
#' 
#' @export
analyse_kinetics <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    method = c("peak_slope"),
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    method <- match.arg(method)
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }

    ## create lightweight dispatch object
    data <- structure(
        # list(data_list = data_list),
        data,
        class = c(method, "mnirs_kinetics")
    )
    
    UseMethod("analyse_kinetics", data)
}


#' @rdname analyse_kinetics
#' @usage NULL
#' @export
analyse_kinetics.peak_slope <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    method = c("peak_slope"),
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    args <- list(...)
    nirs_channels <- enquo(nirs_channels)
    time_channel <- enquo(time_channel)

    ## normalise input to named list of data frames
    data_list <- as_data_list(data)
    interval_names <- names(data_list)

    ## iterate over each data frame in the list
    result_list <- lapply(seq_along(data_list), \(.i) {
        id <- interval_names[[.i]]
        df <- data_list[[.i]]

        result <- analyse_peak_slope(
            data = df,
            nirs_channels = !!nirs_channels,
            time_channel = !!time_channel,
            width = args$width %||% NULL,
            span = args$span %||% NULL,
            align = args$align %||% "centre",
            direction = args$direction %||% "auto",
            partial = args$partial %||% FALSE,
            channel_args = channel_args,
            verbose = verbose
        )
        result$interval <- id
        result$t0 <- attr(df, "event_times")

        ## convert each row's channel_args list to a 1-row data frame
        ## replace NULL values with NA to keep consistent columns
        result$channel_args <- Map(\(.nirs, .arg) {
            .arg <- lapply(.arg, \(.x) if (is.null(.x)) NA else .x)
            data.frame(interval = id, nirs_channels = .nirs, .arg)
        }, result$nirs_channels, result$channel_args)

        ## append `*_fitted` cols to data for `nirs_channels` at `window_idx`
        fitted_cols <- Map(\(.nirs, .fitted, .idx) {
            fitted_vec <- rep(NA_real_, nrow(df))
            fitted_vec[.idx] <- .fitted
            fitted_vec
        }, result$nirs_channels, result$fitted, result$window_idx)
        names(fitted_cols) <- paste0(result$nirs_channels, "_fitted")
        augmented_df <- cbind(df, as.data.frame(fitted_cols))

        ## store as single-element list; attach only to the first row
        result$data <- vector("list", nrow(result))
        result$data[[1L]] <- augmented_df

        ## remove lists from results
        result[c("fitted", "window_idx")] <- NULL

        ## return 1-row tibble of results for each data_list
        result
    })

    ## combine output
    results <- do.call(rbind, result_list)

    ## extract channel_args into single data frame
    channel_args_df <- do.call(
        rbind,
        c(results$channel_args, make.row.names = FALSE)
    )

    ## extract data_list from the first item in each `interval`
    data_list <- Filter(Negate(is.null), results$data)
    names(data_list) <- interval_names

    ## extract data frame of `event_times` 
    t0_df <- as.data.frame(results[c("interval", "nirs_channels", "t0")])

    ## remove lists from results & relocate interval col
    results[c("channel_args", "data", "t0")] <- NULL
    results <- results[, c("interval", setdiff(names(results), "interval"))]

    ## ! add fit diagnostics
    ## ! implement find_first_extreme
    ## ! add test_that

    ## return
    structure(
        list(
            method = method,   ## method = "peak_slope"
            results = results, ## tibble of scalar results
            data = data_list,  ## df of data frames
            t0 = t0_df,        ## df of `event_times` `t` values
            channel_args = channel_args_df, ## df of channel args provided to `analyse_slope`
            call = match.call()
        ),
        class = "mnirs_kinetics"
    )
}


#' Coerce `data` input to a named list of data frames
#' @keywords internal
as_data_list <- function(data) {
    ## grouped data frame → split by groups
    if (inherits(data, "grouped_df")) {
        if (!requireNamespace("dplyr", quietly = TRUE)) {
            cli_abort(c(
                "x" = "{.pkg dplyr} is required for grouped data frame input.",
                "i" = "Install with {.code install.packages(\"dplyr\")}."
            ))
        }
        key_labels <- do.call(paste, c(
            dplyr::group_keys(data), list(sep = "_")
        ))
        data_list <- dplyr::group_split(data, .keep = TRUE)
        data_list <- lapply(data_list, \(.df) {
            create_mnirs_data(
                .df,
    ## TODO need to test when a list has been rbinded and has vectors from original list
                attributes(data)[c(
                    "nirs_channels",
                    "time_channel",
                    "event_channel",
                    "sample_rate",
                    "event_times",
                    "interval_span"
                )]
            )
        })
        names(data_list) <- key_labels
        
        return(data_list)
    }

    ## single data frame → length-1 list
    if (is.data.frame(data)) {
        return(stats::setNames(list(data), "interval_1"))
    }

    ## list of data frames — validate
    if (!is.list(data) || !all(vapply(data, is.data.frame, logical(1)))) {
        cli_abort(
            "{.arg data} must be a list of data frames, or a single grouped \\
            or ungrouped data frame."
        )
    }

    if (is.null(names(data))) {
        names(data) <- paste0("interval_", seq_along(data))
    }

    return(data)
}

