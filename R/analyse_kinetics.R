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

        ## convert each row's diagnostics into a 1-row data frame
        result$diagnostics <- Map(\(.nirs, .diag) {
            data.frame(interval = id, nirs_channels = .nirs, .diag)
        }, result$nirs_channels, result$diagnostics)

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
    
    
    ## extract diagnostics into single data frame
    diagnostics_df <- do.call(
        rbind,
        c(results$diagnostics, make.row.names = FALSE)
    )

    ## extract data_list from the first item in each `interval`
    data_list <- Filter(Negate(is.null), results$data)
    names(data_list) <- interval_names

    ## extract data frame of `event_times` 
    t0_df <- as.data.frame(results[c("interval", "nirs_channels", "t0")])

    ## remove lists from results & relocate interval col
    results[c("channel_args", "data", "t0", "diagnostics")] <- NULL
    results <- results[, c("interval", setdiff(names(results), "interval"))]

    ## ! implement find_first_extreme
    ## ! rename `t` to `time_channel`

    ## return
    structure(
        list(
            method = method,   ## method = "peak_slope"
            results = results, ## tibble of scalar results
            data = data_list,  ## df of data frames
            t0 = t0_df,        ## df of `event_times` `t` values
            diagnostics = diagnostics_df,   ## df of model diagnostics
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

        ## refactor grouping variables to order of appearance
        ## TODO should I convert this to Base R if dplyr is required 
        ## TODO for grouping anyway?
        group_vars <- dplyr::group_vars(data)
        df_grp <- data |>
            dplyr::ungroup() |>
            dplyr::mutate(
                dplyr::across(dplyr::all_of(group_vars), \(.x) {
                    factor(.x, levels = unique(.x))
                })
            ) |>
            dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))
        keys <- do.call(paste, c(dplyr::group_keys(df_grp), list(sep = "_")))
        data_list <- dplyr::group_split(df_grp, .keep = TRUE)

        ## copy mnirs metadata down to each df in the list
        ## TODO need to test when a list has been rbinded and has vectors from original list
        data_list <- lapply(data_list, \(.df) {
            create_mnirs_data(
                .df,
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
        names(data_list) <- keys

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


#' Compute model diagnostics
#'
#' @param fitted A numeric vector of the predicted values.
#' @inheritParams peak_slope
#'
#' @returns A named list with `n_obs`, `r2`, `adj_r2`, and `rmse`.
#'
#' @keywords internal
compute_diagnostics <- function(x, t, fitted, verbose = TRUE) {
    complete_cases <- which(is.finite(x) & is.finite(t))
    x <- x[complete_cases]
    t <- t[complete_cases]
    fitted <- fitted[is.finite(fitted)]
    n_obs <- length(fitted)
    
    return_na <- list(
        n_obs = n_obs,
        r2 = NA_real_,
        adj_r2 = NA_real_,
        rmse = NA_real_
    )

    if (length(x) != length(t) || length(x) != n_obs) {
        if (verbose) {
            cli_warn(c(
                "!" = "{.arg x}, {.arg t}, and {.arg fitted} must be \\
                {.cls numeric} vectors of equal lengths to return model \\ 
                diagnostics."
            ))
        }
        return(return_na)
    }

    if (n_obs < 2L) {
        return(return_na)
    }

    resid <- x - fitted
    ss_res <- sum(resid^2)
    ss_tot <- sum((x - mean(x))^2)

    r2 <- if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot
    adj_r2 <- if (is.na(r2) || n_obs <= 2L) {
        NA_real_
    } else {
        1 - (1 - r2) * (n_obs - 1) / (n_obs - 2) ## TODO check for multivariate models
    }

    rmse <- sqrt(mean(resid^2))

    list(
        n_obs = n_obs,
        r2 = r2,
        adj_r2 = adj_r2,
        rmse = rmse
    )
}