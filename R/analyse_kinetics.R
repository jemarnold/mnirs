#' Analyse mNIRS kinetics across intervals
#'
#' Perform kinetics analysis with parametric curve fitting or non-parametric
#' estimation for `nirs_channels` within an *"mnirs"* data frame or a list of
#' data frames.
#'
#' @param data A data frame of class *"mnirs"* containing time series data and
#'   metadata, a list of data frames, or a grouped data frame (see *Details*).
#' @param method A character string specifying the kinetics analysis method
#'   `<under development>`. Additional arguments must be specified for each
#'   method. See *Details*.
#'   \describe{
#'      \item{`"half_time"`}{`<under development>`.}
#'      \item{`"peak_slope"`}{Peak local linear regression slope. Additional
#'      arguments: `width` or `span`, `align`, `direction`, `partial`.}
#'      \item{`"monoexponential"`}{`<under development>`.}
#'      \item{`"sigmoidal"`}{`<under development>`.}
#'   }
#' @param channel_args An *optional* `list()` with names corresponding to
#'   `nirs_channels` for unique per-channel arguments to override global
#'   default arguments (see *Details*).
#' @param ... Additional arguments passed to the underlying method function
#'   (e.g. [peak_slope()]). See *Details*.
#' @inheritParams validate_mnirs
#'
#' @details
#' ## data input formats
#'
#' `analyse_kinetics()` can accept `data` in multiple formats:
#'
#' - A **single *"mnirs"* data frame** will be processed as a single interval.
#' - A **list of *"mnirs"* data frames** — each interval data frame will be
#'   processed seperately
#' - A **grouped *"mnirs"* data frame**, e.g. with `dplyr::group_by()` —
#'   the data frame will be split by grouping levels and processed as
#'   separate intervals.
#'
#' ## kinetics analysis method
#'
#' #### `method = "half_time"`
#'
#' `<under development>`
#'
#' #### `method = "peak_slope"`
#'
#' The `"peak_slope"` method identifies the maximum local linear slope within
#' each `nirs_channel` using rolling least-squares regression. The local window
#' is defined by `width` (number of samples) or by `span` (time units of
#' `time_channel`). See [peak_slope()] for details.
#'
#' Additional arguments (`...`) accepted when `method = "peak_slope"`:
#'
#' \describe{
#'   \item{`width` or `span`}{Either the number of samples (integer), or the
#'       time duration in units of `time_channel` (numeric) in the local
#'       rolling window. One of either `width` or `span` must be specified.}
#'   \item{`align`}{Character; window alignment — `"centre"` (default),
#'       `"left"`, or `"right"`.}
#'   \item{`direction`}{Character; slope direction to detect — `"auto"`
#'       (default), `"positive"`, or `"negative"`. See [peak_slope()].}
#'   \item{`partial`}{Logical; `FALSE` by default, only returns slope values
#'       where all samples are valid (no `NA`s and no fewer samples than
#'       `width` or `span` in the local window). If `TRUE`, allows slope
#'       calculation over partial windows with at least 2 valid samples.}
#' }
#'
#' #### `method = "monoexponential"`
#'
#' `<under development>`
#'
#' #### `method = "sigmoidal"`
#'
#' `<under development>`
#'
#' ## channel_args per nirs_channel
#'
#' Arguments in `analyse_kinetics()` apply to all `nirs_channels` by default.
#' `channel_args` allows overriding defaults with unique values per channel,
#' e.g.:
#'
#' ```r
#' analyse_kinetics(
#'     data,
#'     nirs_channels = c(hhb, smo2),
#'     span = 3,
#'     direction = "positive",
#'     channel_args = list(
#'         hhb  = list(span = 5),
#'         smo2 = list(direction = "negative")
#'     )
#' )
#' ```
#'
#' @returns A formatted table of printed results, with individual elements
#'   accessable as a list of class *"mnirs_kinetics"* containing:
#'
#'   \item{`method`}{The method used, e.g. `"half_time"`.}
#'   \item{`results`}{A [tibble][tibble::tibble-package] of results with
#'       one row per `nirs_channel` and per interval, containing columns
#'       `interval`, `nirs_channels`, and individual method parameters.}
#'   \item{`data`}{A list of the original input data frames augmented with a
#'       `*_fitted` column of model predicted values for each `nirs_channel`.}
#'   \item{`event_times`}{A data frame of event times (`t0`) for each
#'       `nirs_channel`
#'       per interval, sourced from `event_times` supplied from
#'       `extract_intervals`, if present in the metadata.}
#'   \item{`diagnostics`}{A data frame of model diagnostics (`n_obs`, `r2`,
#'       `adj_r2`, `pseudo_r2`, `rmse`, `snr`, `cv_rmse`) with one row per
#'       `nirs_channel` and interval.}
#'   \item{`channel_args`}{A data frame of the resolved arguments used for
#'       each `nirs_channel` with one row per `nirs_channel` and interval.}
#'   \item{`call`}{The matched call.}
#'
#' @seealso [extract_intervals()], [monoexponential()], [peak_slope()]
#'
#' @examples
#' result <- read_mnirs(
#'     example_mnirs("train.red"),
#'     nirs_channels = c(
#'         smo2_left = "SmO2 unfiltered",
#'         smo2_right = "SmO2 unfiltered"
#'     ),
#'     time_channel = c(time = "Timestamp (seconds passed)"),
#'     zero_time = TRUE,
#'     verbose = FALSE
#' ) |>
#'     resample_mnirs(verbose = FALSE) |>
#'     extract_intervals(
#'         event_times = c(368, 1093),
#'         event_groups = "distinct",
#'         span = c(-20, 90),
#'         zero_time = TRUE,
#'         verbose = FALSE
#'     ) |>
#'     analyse_kinetics(
#'         nirs_channels = c(smo2_left, smo2_right),
#'         method = "peak_slope",
#'         span = 10, ## 10-second rolling window
#'         direction = "auto", ## auto-detect slope direction
#'         verbose = FALSE
#'     )
#'
#' ## formatted table of results
#' result
#'
#' ## results are accessible from the results list
#' result$results
#'
#' ## along with diagnostics and other results
#' result$diagnostics
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

        ## convert each row's channel_args list to a 1-row data frame
        ## replace NULL values with NA to keep consistent columns
        result$channel_args <- Map(\(.nirs, .arg) {
            .arg <- lapply(.arg, \(.x) if (is.null(.x)) NA else .x)
            data.frame(interval = id, nirs_channels = .nirs, .arg)
        }, result$nirs_channels, result$channel_args)

        ## convert each row's diagnostics into a 1-row data frame
        result$diagnostics <- Map(\(.nirs, .diag) {
            cbind(data.frame(interval = id, nirs_channels = .nirs), .diag)
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
        
        ## add mnirs metadata back to result$data
        result$data[[1L]] <- create_mnirs_data(
            result$data[[1L]],
            attributes(df)
        )

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
    data_list <- Filter(\(.x) !is.null(.x), results$data)
    names(data_list) <- interval_names

    ## extract event_times as list-column (one row per interval)
    event_times_df <- data.frame(interval = interval_names)
    event_times_df$event_times <- lapply(data_list, \(.df) {
        et <- attr(.df, "event_times")
        if (is.null(et)) NA_real_ else unlist(et)
    })

    ## remove lists from results & relocate interval col
    results[c("channel_args", "data", "diagnostics")] <- NULL
    results <- results[, c("interval", setdiff(names(results), "interval"))]

    ## ! implement find_first_extreme
    ## ! rename `t` to `time_channel`

    ## return
    structure(
        list(
            method = method, ## method = "peak_slope"
            results = results, ## tibble of scalar results
            data = data_list, ## df of data frames
            event_times = event_times_df, ## df of `event_times` `t` values
            diagnostics = diagnostics_df, ## df of model diagnostics
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
#' @param n_params Integer; number of estimated parameters in the model,
#'   excluding the intercept (default `1L`). Used to compute `adj_r2`.
#'   For non-linear models (`"monoexponential"`, `"sigmoidal"`), pass the
#'   number of free parameters fit by the solver.
#' @inheritParams peak_slope
#'
#' @returns A 1-row `data.frame` with columns `n_obs`, `r2`, `adj_r2`,
#'   `pseudo_r2`, `rmse`, `snr`, and `cv_rmse`.
#'
#'   - `adj_r2`: adjusted $R^2$ penalised by `n_params`. Appropriate for
#'     OLS linear models; interpret with caution for non-linear fits.
#'   - `pseudo_r2`: squared Pearson correlation between observed and fitted
#'     values, $\rho^2 = \left[\text{cor}(x, \hat{x})\right]^2$. Equivalent
#'     to $R^2$ for OLS but well-defined for non-linear and multivariate
#'     models. Preferred for `"monoexponential"` and `"sigmoidal"` methods.
#'
#' @keywords internal
compute_diagnostics <- function(x, t, fitted, n_params = 1L, verbose = TRUE) {
    complete_cases <- which(is.finite(x) & is.finite(t))
    x <- x[complete_cases]
    t <- t[complete_cases]
    fitted <- fitted[is.finite(fitted)]
    n_obs <- length(fitted)

    return_na <- data.frame(
        n_obs     = n_obs,
        r2        = NA_real_,
        adj_r2    = NA_real_,
        pseudo_r2 = NA_real_,
        rmse      = NA_real_,
        snr       = NA_real_,
        cv_rmse   = NA_real_
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

    ## residuals
    resid <- x - fitted
    ss_res <- sum(resid^2)
    ss_tot <- sum((x - mean(x))^2)

    ## R²
    r2 <- if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot

    ## adjusted R²: penalised by n_params; valid for OLS linear models
    adj_r2 <- if (is.na(r2) || n_obs <= (n_params + 1L)) {
        NA_real_
    } else {
        1 - (1 - r2) * (n_obs - 1L) / (n_obs - n_params - 1L)
    }

    ## pseudo-R²: cor(observed, fitted)² — valid for linear and non-linear
    ## models; equals R² for OLS, preferred for monoexponential/sigmoidal
    pseudo_r2 <- if (stats::sd(x) == 0 || stats::sd(fitted) == 0) {
        NA_real_
    } else {
        stats::cor(x, fitted)^2
    }

    ## RMSE
    rmse <- sqrt(mean(resid^2))

    ## SNR: signal variance to residual variance, in dB
    var_signal <- ss_tot / (n_obs - 1L)
    var_resid  <- ss_res / (n_obs - 1L)
    snr <- if (is.na(var_signal) || var_resid == 0) {
        NA_real_
    } else {
        10 * log10(var_signal / var_resid)
    }

    ## CV-RMSE: RMSE normalised by the absolute mean of observed values
    x_mean <- mean(x)
    cv_rmse <- if (x_mean == 0) NA_real_ else rmse / abs(x_mean)

    data.frame(
        n_obs     = n_obs,
        r2        = r2,
        adj_r2    = adj_r2,
        pseudo_r2 = pseudo_r2,
        rmse      = rmse,
        snr       = snr,
        cv_rmse   = cv_rmse
    )
}
