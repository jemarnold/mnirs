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
#'      arguments: `width` or `span`, `align`, `direction`, `partial`, `na.rm`.}
#'      \item{`"monoexponential"`}{Monoexponential curve fit via
#'      [stats::nls()] with arguments: `monoexp_params` for [SS_monoexp3()] or
#'      [SS_monoexp4()]; `algorithm`; `control`.}
#'      \item{`"sigmoidal"`}{`<under development>`.}
#'   }
#' @param channel_args An *optional* `list()` with names corresponding to
#'   `nirs_channels` for unique per-channel arguments to override global
#'   default arguments (see *Details*).
#' @param ... Additional arguments passed to the underlying method function.
#'   See *Details*.
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
#'   \item{`partial`}{Logical; default is `FALSE`, requires local windows 
#'       to have complete number of samples specified by `width` or `span`. 
#'       If `TRUE`, processes available samples within the local window.}
#'   \item{`na.rm`}{Logical; default is `TRUE`, ignores `NA`s and processes 
#'       available valid samples within the local window. If `TRUE`, 
#'       propagates any `NA`s to the returned vector.}
#' }
#'
#' #### `method = "monoexponential"`
#'
#' The `"monoexponential"` method fits a self-starting monoexponential
#' curve to each `nirs_channel` using [stats::nls()] with [SS_monoexp3()]
#' (3-parameter: A, B, tau) or [SS_monoexp4()] (4-parameter: A, B, tau,
#' TD). See [monoexponential()] for model equations.
#'
#' Additional arguments (`...`) accepted when `method = "monoexponential"`:
#'
#' \describe{
#'   \item{`monoexp_params`}{Integer; `3L` (*default*) for [SS_monoexp3()] or
#'       `4L` for [SS_monoexp4()].}
#'   \item{`algorithm`}{Character; [stats::nls()] algorithm — `"default"`
#'       (*default*), `"plinear"`, or `"port"`.}
#'   \item{`control`}{A named list of control parameters passed to
#'       [stats::nls.control()], e.g. `list(maxiter = 100)`.}
#' }
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
#'   \item{`interval_times`}{A data frame of interval times for each
#'       `nirs_channel` per interval, supplied from `extract_intervals`, if
#'       present in the metadata.}
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
#'         start = by_time(368, 1093),
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
    method = c("peak_slope", "monoexponential"),
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    method <- match.arg(method)
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }

    UseMethod(
        "analyse_kinetics",
        structure(data, class = c(method, "mnirs_kinetics"))
    )
}


#' @rdname analyse_kinetics
#' @usage NULL
#' @export
analyse_kinetics.peak_slope <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    method = c("peak_slope", "monoexponential"),
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

    ## iterate over each interval
    result_list <- lapply(seq_along(data_list), \(.i) {
        result <- analyse_peak_slope(
            data = data_list[[.i]],
            nirs_channels = !!nirs_channels,
            time_channel = !!time_channel,
            width = args$width %||% NULL,
            span = args$span %||% NULL,
            align = args$align %||% "centre",
            direction = args$direction %||% "auto",
            partial = args$partial %||% FALSE,
            na.rm = args$na.rm %||% TRUE, ## TODO do I want FALSE?
            channel_args = channel_args,
            verbose = verbose
        )
        result$interval <- interval_names[[.i]]
        result
    })

    ## collate into mnirs_kinetics fields
    gathered <- gather_kinetics(data_list, result_list, interval_names)

    ## ! implement find_first_extreme

    return(structure(
        list(
            method = method,
            results = gathered$results,
            data = gathered$data,
            interval_times = gathered$interval_times,
            diagnostics = gathered$diagnostics,
            channel_args = gathered$channel_args,
            call = match.call()
        ),
        class = "mnirs_kinetics"
    ))
}


#' @rdname analyse_kinetics
#' @usage NULL
#' @export
analyse_kinetics.monoexponential <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    method = c("peak_slope", "monoexponential"),
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

    ## iterate over each interval
    result_list <- lapply(seq_along(data_list), \(.i) {
        result <- analyse_monoexponential(
            data = data_list[[.i]],
            nirs_channels = !!nirs_channels,
            time_channel = !!time_channel,
            monoexp_params = args$monoexp_params %||% 3L,
            algorithm = args$algorithm %||% "default",
            control = args$control %||% list(),
            channel_args = channel_args,
            verbose = verbose,
            interval_names = interval_names
        )
        result$interval <- interval_names[[.i]]
        result
    })

    ## collate into mnirs_kinetics fields
    gathered <- gather_kinetics(data_list, result_list, interval_names)

    return(structure(
        list(
            method = method,
            results = gathered$results,
            data = gathered$data,
            interval_times = gathered$interval_times,
            diagnostics = gathered$diagnostics,
            channel_args = gathered$channel_args,
            call = match.call()
        ),
        class = "mnirs_kinetics"
    ))
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
                    "interval_times",
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


#' Gather per-interval `mnirs_kinetics` into results structure
#'
#' Shared helper for `analyse_kinetics.*` methods. Takes a list of
#' per-interval (per-data frame) kinetics results data frames (each carrying
#' `"predicted"`, `"channel_args"`, and `"diagnostics"` attributes), the
#' original `data_list`, and interval names.
#'
#' @param data_list Named list of original interval data frames.
#' @param result_list List of per-interval result data frames with attributes.
#'
#' @returns A named list with: `results`, `data`, `interval_times`,
#'   `diagnostics`, `channel_args`.
#' @keywords internal
gather_kinetics <- function(
    data_list,
    result_list,
    interval_names
) {
    ## extract per channel attrs; bind to df; add "interval" col; drop rownames
    flatten_attr <- function(attr_name) {
        df <- do.call(rbind, lapply(result_list, attr, attr_name))
        df$interval <- interval_names
        df <- df[, c("interval", setdiff(names(df), "interval"))]
        rownames(df) <- NULL
        df
    }

    channel_args <- flatten_attr("channel_args")
    diagnostics <- flatten_attr("diagnostics")

    ## augment `data_list` dfs with `<nirs_channels>_fitted` columns
    fitted_data_list <- Map(\(.df, .result) {
        ## extract fitted columns from "predicted" per `nirs_channel`
        predicted <- attr(.result, "predicted")
        fitted_cols <- lapply(predicted, \(.pred) {
            fitted_vec <- rep(NA_real_, nrow(.df))
            fitted_vec[.pred$window_idx] <- .pred$fitted
            fitted_vec
        })
        names(fitted_cols) <- paste0(names(predicted), "_fitted")
        ## agument `<nirs_channels>_fitted` columns to df
        augmented <- cbind(.df, as.data.frame(fitted_cols))
        ## preserve metadata to augmented data frames
        create_mnirs_data(augmented, attributes(.df))
    }, data_list, result_list)
    names(fitted_data_list) <- interval_names

    ## extract interval_times from each data_list attributes, if exist
    interval_times_df <- data.frame(interval = interval_names)
    interval_times_df$interval_times <- lapply(data_list, \(.df) {
        interval_times <- attr(.df, "interval_times")
        if (is.null(interval_times)) NA_real_ else unlist(interval_times)
    })

    ## combine scalar results & relocate interval col to col[1]
    results <- do.call(rbind, result_list)
    results <- results[, c("interval", setdiff(names(results), "interval"))]
    rownames(results) <- NULL

    return(list(
        results = results,
        data = fitted_data_list,
        interval_times = interval_times_df,
        diagnostics = diagnostics,
        channel_args = channel_args
    ))
}


#' Serialise per-channel arguments to a 1-row data frame
#'
#' Converts `NULL` values to `NA` and `list` values (e.g. `control`) to
#' their `deparse()` representation so the result can be stored in a flat
#' data frame.
#'
#' @param nirs_channel Character; column name of the channel.
#' @param all_args Named list of resolved arguments.
#'
#' @returns A 1-row `data.frame`.
#'
#' @keywords internal
safe_channel_args <- function(nirs_channel, all_args) {
    safe <- lapply(all_args, \(.x) {
        if (is.null(.x)) {
            NA
        } else if (is.list(.x)) {
            deparse(.x)
        } else {
            .x
        }
    })
    return(data.frame(nirs_channels = nirs_channel, safe))
}


#' Build a standardised NA result for a failed channel
#'
#' Returns the 4-element list (`scalar`, `predicted`, `diagnostics`,
#' `channel_args`) expected by [gather_kinetics()], populated with `NA` values.
#'
#' @param nirs_channel Character; column name of the channel.
#' @param na_scalar A template 1-row `data.frame` with all `NA` values matching
#'   the method's scalar columns.
#' @param all_args Named list of resolved arguments (passed to
#'   [safe_channel_args()]).
#' @param n_params Integer; number of model parameters (passed to
#'   [compute_diagnostics()]).
#'
#' @returns A named list with elements `scalar`, `predicted`,
#'   `diagnostics`, and `channel_args`.
#'
#' @keywords internal
build_na_reults <- function(
    nirs_channel,
    na_scalar,
    all_args,
    n_params = 1L
) {
    na_diag <- compute_diagnostics(
        x = numeric(0),
        t = numeric(0),
        fitted = numeric(0),
        n_params = n_params,
        verbose = FALSE
    )
    na_scalar$nirs_channels <- nirs_channel
    return(list(
        scalar = na_scalar,
        predicted = data.frame(window_idx = NA_integer_, fitted = NA_real_),
        diagnostics = cbind(data.frame(nirs_channels = nirs_channel), na_diag),
        channel_args = safe_channel_args(nirs_channel, all_args)
    ))
}


#' Assemble per-channel results into an attributed data frame
#'
#' Combines the list of per-channel result lists (each with
#' `scalar`, `predicted`, `diagnostics`, `channel_args`) into
#' the single attributed data frame that [gather_kinetics()]
#' expects.
#'
#' @param results Named list of per-channel result lists.
#'
#' @returns A `data.frame` with attributes `"predicted"`,
#'   `"diagnostics"`, and `"channel_args"`.
#' @keywords internal
build_channel_results <- function(results) {
    return(structure(
        do.call(rbind, lapply(results, `[[`, "scalar")),
        predicted = lapply(results, `[[`, "predicted"),
        diagnostics = do.call(rbind, lapply(results, `[[`, "diagnostics")),
        channel_args = do.call(rbind, lapply(results, `[[`, "channel_args"))
    ))
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
#'   - `adj_r2`: adjusted `R^2` penalised by `n_params`. Appropriate for
#'     OLS linear models; interpret with caution for non-linear fits.
#'   - `pseudo_r2`: squared Pearson correlation between observed and fitted
#'     values. Equivalent to `R^2` for OLS but well-defined for non-linear
#'     and multivariate models. Preferred for `"monoexponential"` and
#'     `"sigmoidal"` methods.
#'
#' @keywords internal
compute_diagnostics <- function(
    x,
    t,
    fitted,
    n_params = 1L,
    verbose = TRUE
) {
    complete_cases <- which(is.finite(x) & is.finite(t))
    x <- x[complete_cases]
    t <- t[complete_cases]
    fitted <- fitted[is.finite(fitted)]
    n_obs <- length(fitted)

    return_na <- data.frame(
        n_obs = n_obs,
        r2 = NA_real_,
        adj_r2 = NA_real_,
        pseudo_r2 = NA_real_,
        rmse = NA_real_,
        snr = NA_real_,
        cv_rmse = NA_real_
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
    var_resid <- ss_res / (n_obs - 1L)
    snr <- if (is.na(var_signal) || var_resid == 0) {
        NA_real_
    } else {
        10 * log10(var_signal / var_resid)
    }

    ## CV-RMSE: RMSE normalised by the absolute mean of observed values
    x_mean <- mean(x)
    cv_rmse <- if (x_mean == 0) NA_real_ else rmse / abs(x_mean)

    data.frame(
        n_obs = n_obs,
        r2 = r2,
        adj_r2 = adj_r2,
        pseudo_r2 = pseudo_r2,
        rmse = rmse,
        snr = snr,
        cv_rmse = cv_rmse
    )
}
