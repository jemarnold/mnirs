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
    names <- names(data_list)

    ## iterate over each data frame in the list
    result_list <- lapply(seq_along(data_list), \(.i) {
        id <- names[[.i]]
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
        result$x0 <- attr(df, "event_times")

        ## return 1-row tibble of results for each data_list
        result
    })

    ## combine output
    results <- do.call(rbind, result_list)

    ## remove lists from results
    fitted_list <- results$fitted
    window_idx_list <- results$window_idx
    channel_args_list <- results$channel_args
    x0_list <- results$x0
    results[c("fitted", "window_idx", "channel_args", "x0")] <- NULL

    ## relocate cols
    results <- results[, c("interval", setdiff(names(results), "interval"))]

    ## ! add fitted to data_list data frames
    ## ! add data.frame of channel_args
    ## ! add fit diagnostics
    ## ! implement find_first_extreme
    ## ! add test_that
    ## add fitted data to data frames in data_list
    # data_list <- lapply(seq_along(data_list), \(.i) {
    #     df <- data_list[[.i]]
    #     interval_name <- names(data_list)[[.i]]
        
    #     ## get all rows for this interval from results
    #     interval_results <- results[results$interval == interval_name, ]
        
    #     ## create list of fitted_cols for all nirs_channels
    #     fitted_cols <- lapply(seq_len(nrow(interval_results)), \(.j) {
    #         nirs_col <- interval_results$nirs_channels[.j]
    #         idx <- which(
    #             results$interval == interval_name &
    #                 results$nirs_channels == nirs_col
    #         )
            
    #         ## initialize fitted column with NAs
    #         fitted_vec <- rep(NA_real_, nrow(df))
            
    #         ## assign fitted values at window indices
    #         fitted_vec[window_idx_list[[idx]]] <- fitted_list[[idx]]
            
    #         fitted_vec
    #     })
        
    #     ## name the columns `*_fitted` and bind to data frame
    #     names(fitted_cols) <- paste0(interval_results$nirs_channels, "_fitted")
    #     cbind(df, as.data.frame(fitted_cols))
    # })
    
    structure(
        list(
            method = method,             ## method = "peak_slope"
            results = results,           ## tibble of scalar results
            # data = data_list,            ## list of data frames
            # x0 = x0_list,                ## vec of `event_times` `t` values
            # channel_args = channel_args_list, ## list of channel args provided to `analyse_slope`
            call = match.call()
        ),
        class = "mnirs_kinetics"
    )
    # return(results)
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

