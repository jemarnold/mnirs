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
#'      \item{`"HRT"`}{`<under development>`.}
#'      \item{`"peak_slope"`}{Peak local linear regression slope. Additional
#'      arguments: `width` or `span`, `align`, `direction`, `partial`, `na.rm`.}
#'      \item{`"monoexponential"`}{Monoexponential curve fit via
#'      [stats::nls()] with arguments: `time_delay`.}
#'      \item{`"sigmoidal"`}{`<under development>`.}
#'   }
#' @param direction A character string specifying the kinetics direction to
#'   detect — `"auto"` (*default*), `"positive"`, or `"negative"`. See
#'   *Details*.
#' @param end_fit_span A numeric value in units of `time_channel` specifying 
#'   the forward-looking window used to check for subsequent greater/lesser 
#'   values than the candidate extreme.
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
#' #### `method = "HRT"`
#' 
#' Aliases: `c("half response time", "half time")`
#'
#' `<under development>`
#'
#' #### `method = "peak_slope"`
#' 
#' Aliases: `c("peak slope", "slope")`
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
#' Aliases: `c("monoexp", "exponential", "MRT", "tau")`
#'
#' The `"monoexponential"` method fits a self-starting monoexponential
#' curve to each `nirs_channel` using [stats::nls()] with [SS_monoexp3()]
#' (3-parameter: A, B, tau) or [SS_monoexp4()] (4-parameter: A, B, tau,
#' TD). See [monoexponential()] for model equations.
#'
#' Additional arguments (`...`) accepted when `method = "monoexponential"`:
#'
#' \describe{
#'   \item{`time_delay`}{Logical; default is `TRUE` to attempt to fit a
#'       4-parameter [SS_monoexp4()] model (A, B, tau, TD) with a time delay.
#'       If the 4-parameter fit fails, or if `time_delay = FALSE`, fits a
#'       reduced 3-parameter [SS_monoexp3()] model (A, B, tau).}
#'   \item{`stats::nls()`}{Other arguments can be passed to [stats::nls()]}
#' }
#'
#' #### `method = "sigmoidal"`
#' 
#' Aliases: `c("logistic", "xmid")`
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
#'   \item{`method`}{The method used, e.g. `"HRT"`.}
#'   \item{`model`}{A named list of model objects (per interval,
#'       per `nirs_channel`). For `"peak_slope"`, each element is an
#'       [lm][stats::lm] object; for `"monoexponential"`, an
#'       [nls][stats::nls] object. `NULL` for channels where fitting
#'       failed.}
#'   \item{`coefficients`}{A [tibble][tibble::tibble-package] of coefficients
#'       with one row per `nirs_channel` and per interval, containing columns
#'       `interval`, `nirs_channels`, and individual method parameters.}
#'   \item{`data`}{A list of the original input data frames augmented with a
#'       `*_fitted` column of model fitted data values for each `nirs_channel`.}
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
#' ## coefficients are accessible from the result list
#' result$coefficients
#'
#' ## along with diagnostics and other returned objects
#' result$diagnostics
#'
#' @export
analyse_kinetics <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    method = c("HRT", "peak_slope", "monoexponential", "sigmoidal"),
    direction = c("auto", "positive", "negative"),
    end_fit_span = 20,
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    ## normalise method aliases before matching
    method <- gsub(
        "^half[ _-]response[ _-]time$|^half[ _-]time$",
        "HRT",
        method,
        ignore.case = TRUE
    )
    method <- gsub(
        "^peak[ _-]slope$|^slope$",
        "peak_slope",
        method,
        ignore.case = TRUE
    )
    method <- gsub(
        "^monoexp$|^exponential$|^MRT$|^tau$",
        "monoexponential",
        method,
        ignore.case = TRUE
    )
    method <- gsub(
        "^logistic$|^xmid$",
        "sigmoidal",
        method,
        ignore.case = TRUE
    )
    method <- match.arg(method)
    direction <- match.arg(direction)
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
analyse_kinetics.HRT <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    method,
    direction = c("auto", "positive", "negative"),
    end_fit_span = 20,
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    args <- list(...)
    ## normalise input to named list of data frames
    data_list <- as_data_list(data)

    ## iterate over each interval
    result_list <- lapply(seq_along(data_list), \(.i) {
        ## ! IMPLEMENT analyse_HRT()
        result <- analyse_HRT()

        result$interval <- names(data_list)[[.i]]
        result
    })

    ## collate and return mnirs_kinetics object
    return(build_kinetics_results(
        data_list,
        result_list,
        names(data_list),
        method,
        match.call()
    ))
}


#' @rdname analyse_kinetics
#' @usage NULL
#' @export
analyse_kinetics.peak_slope <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    method,
    direction = c("auto", "positive", "negative"),
    end_fit_span = 20,
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    args <- list(...)
    ## normalise input to named list of data frames
    data_list <- as_data_list(data)

    ## iterate over each interval
    result_list <- lapply(seq_along(data_list), \(.i) {
        .df <- data_list[[.i]]
        metadata <- attributes(.df)
        
        result <- analyse_peak_slope(
            data = .df,
            nirs_channels = !!enquo(nirs_channels),
            time_channel = !!enquo(time_channel),
            width = args$width %||% NULL,
            span = args$span %||% NULL,
            align = args$align %||% "centre",
            direction = direction,
            end_fit_span = end_fit_span,
            partial = args$partial %||% FALSE,
            na.rm = args$na.rm %||% TRUE, ## TODO do I want less opinionated?
            channel_args = channel_args,
            verbose = verbose
        )

        result$interval <- names(data_list)[[.i]]
        result
    })

    ## collate and return mnirs_kinetics object
    return(build_kinetics_results(
        data_list,
        result_list,
        names(data_list),
        method,
        match.call()
    ))
}


#' @rdname analyse_kinetics
#' @usage NULL
#' @export
analyse_kinetics.monoexponential <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    method,
    direction = c("auto", "positive", "negative"),
    end_fit_span = 20,
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    ## ! implement stats::nls() additional args
    ## ! implement `direction`
    args <- list(...)
    ## normalise input to named list of data frames
    data_list <- as_data_list(data)

    ## iterate over each interval
    result_list <- lapply(seq_along(data_list), \(.i) {
        result <- analyse_monoexponential(
            data = data_list[[.i]],
            nirs_channels = !!enquo(nirs_channels),
            time_channel = !!enquo(time_channel),
            time_delay = args$time_delay %||% TRUE,
            end_fit_span = end_fit_span,
            channel_args = channel_args,
            verbose = verbose,
            interval_names = names(data_list) ## ! is this needed?
        )

        result$interval <- names(data_list)[[.i]]
        result
    })

    ## collate and return mnirs_kinetics object
    return(build_kinetics_results(
        data_list,
        result_list,
        names(data_list),
        method,
        match.call()
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
            create_mnirs_data(.df, attributes(data)[mnirs_metadata])
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
#' @details
#' 
#' ## `adj_r2`
#' 
#' Adjusted `R^2` penalised by `n_params`. Appropriate for OLS linear models;
#'   interpret with caution for non-linear fits.
#' 
#' ## `pseudo_r2`
#' 
#' Squared Pearson correlation between observed and fitted values. Equivalent
#'   to `R^2` for OLS but well-defined for non-linear and multivariate models.
#'   Preferred for `"monoexponential"` and `"sigmoidal"` methods.
#' 
#' @returns A 1-row `data.frame` with columns `n_obs`, `r2`, `adj_r2`,
#'   `pseudo_r2`, `rmse`, `snr`, and `cv_rmse`.
#'
#' @keywords internal
compute_diagnostics <- function(
    x,
    t,
    fitted,
    n_params = 1L,
    verbose = TRUE
) {
    ## ! check redundant validity check
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

    if (n_params < 1L || n_obs < 2L) {
        return(return_na)
    }

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
    ## ! confirm redundant r2 and pseudo-r2 for lm and nls?

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

    return(data.frame(
        n_obs = n_obs,
        r2 = r2,
        adj_r2 = adj_r2,
        pseudo_r2 = pseudo_r2,
        rmse = rmse,
        snr = snr,
        cv_rmse = cv_rmse
    ))
}
