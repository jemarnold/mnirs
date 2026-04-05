#' Detect the direction of a response signal
#'
#' Resolves whether a signal is predominantly increasing (`"positive"`) or
#' decreasing (`"negative"`) by computing the net slope of `x` over `t`.
#' Used internally to disambiguate peak (maximum) from trough (minimum)
#' detection when `direction = "auto"`.
#'
#' @param fallback A numeric vector (*defaults* to `x`) used to resolve 
#'   direction when the net slope of `x` is zero or `NA`. The absolute 
#'   maximum and minimum of `fallback` are compared; if `abs(max) >= abs(min)`,
#'   `"positive"` is returned.
#' @param direction A character string specifying the kinetics direction to
#'   detect when `"auto"` (*default*). When `"positive"` or `"negative"`
#'   returns unchanged.
#' @inheritParams replace_invalid
#'
#' @returns A character string: `"positive"` or `"negative"`.
#'
#' @keywords internal
detect_direction <- function(
    x,
    t = seq_along(x),
    fallback = x,
    direction = c("auto", "positive", "negative")
) {
    direction <- match.arg(direction)
    if (direction == "auto") {
        if (all(!is.finite(x))) {
            return("positive")
        }
        net_slope <- slope(x, t, na.rm = TRUE, bypass_checks = TRUE)

        direction <- if (is.na(net_slope) || net_slope == 0) {
            ## fallback to abs magnitude comparison when net slope is zero/NA
            max_pos <- abs(max(fallback, na.rm = TRUE))
            min_neg <- abs(min(fallback, na.rm = TRUE))
            if (max_pos >= min_neg) "positive" else "negative"
        } else if (net_slope > 0) {
            "positive"
        } else {
            "negative"
        }
    }

    return(direction)
}


#' Find valid model-fitting indices up to the first extreme
#'
#' Filters `x` and `t` to valid finite values, locates the first valid peak 
#' (maximum) or trough (minimum) where `t >= 0`, and returns the integer 
#' indices of all finite observations up to `end_fit_span` past that extreme.
#'
#' @param end_fit_span A numeric value in units of `t` specifying the
#'   forward-looking window used to check for subsequent greater/lesser
#'   values than the candidate extreme.
#' @param direction A character string specifying the kinetics direction to
#'   detect — `"auto"` (*default*), `"positive"`, or `"negative"`. See
#'   *Details*.
#' @inheritParams replace_invalid
#'
#' @details
#' ## Direction detection
#'
#' When `direction = "auto"`, the net slope across all of `x` is computed
#' to determine the overall trend. If positive, the function searches for
#' a peak (maximum). If negative, it searches for a trough (minimum). When
#' the net slope is zero or `NA`, the direction is determined by comparing
#' `abs(max(x))` to `abs(min(x))`, with ties defaulting to `"positive"`.
#'
#' ## Negative time handling
#'
#' Only samples where `t >= 0` are used for detecting the extreme, allowing
#' pre-baseline (negative time) data to be excluded from the search.
#' However, indices where `t < 0` are included in the returned vector
#' provided they are finite.
#'
#' @returns A named list with three elements:
#'   \describe{
#'     \item{`direction`}{Character; the resolved direction used —
#'       `"positive"` (peak) or `"negative"` (trough).}
#'     \item{`extreme`}{Integer or `NULL`; the index of the
#'       first qualifying peak or trough in original `x`
#'       space, or `NULL` if no qualifying extreme was found
#'       (monotonic, horizontal, or degenerate input).}
#'     \item{`idx`}{Integer vector of all valid finite indices,
#'       truncated at `t[extreme] + end_fit_span`.}
#'   }
#'
#' @keywords internal
find_kinetics_idx <- function(
    x,
    t = seq_along(x),
    end_fit_span = 20,
    direction = c("auto", "positive", "negative"),
    ...
) {
    ## validation =================================================
    args <- list(...)
    n <- length(x)

    if (!(args$bypass_checks %||% FALSE)) {
        validate_x_t(x, t, allow_na = TRUE)
        validate_numeric(
            end_fit_span, 1, c(0, Inf), msg1 = "one-element positive"
        )
        direction <- match.arg(direction)
    }

    ## all finite indices (including t < 0)
    which_valid <- which(is.finite(x) & is.finite(t))
    ## subset where t >= 0 for extreme detection
    which_positive <- which_valid[t[which_valid] >= 0]
    x_valid <- x[which_positive]
    t_valid <- t[which_positive]
    n_valid <- length(x_valid)

    invalid_return <- list(
        direction = "positive",
        extreme = NULL,
        idx = which_valid
    )

    ## early returns for degenerate inputs
    if (n_valid < 2L) {
        return(invalid_return)
    }

    ## direction detection, fallback to abs magnitude
    direction <- detect_direction(x, t, x, direction)
    extreme_fn <- if (direction == "positive") max else min
    compare_fn <- if (direction == "positive") `>=` else `<=`
    which_fn   <- if (direction == "positive") which.max else which.min
    invalid_return$direction <- direction

    ## monotonic: if global extreme is the last x value
    ## horizontal: if all x values equal
    if (which_fn(x_valid) == n_valid || all(x_valid == x_valid[1L])) {
        return(invalid_return)
    }

    ## process ==================================================
    ## bin by end_fit_span, find extreme per bin, then check forward window

    ## ensure end_fit_span covers at least one adjacent sample when
    ## end_fit_span is smaller than the minimum time step
    t_diff <- diff(t_valid)
    mod_span <- max(end_fit_span, min(t_diff[t_diff > 0]))

    ## break t_valid into mod_span-width bins
    bin_breaks <- seq(t_valid[1L], t_valid[n_valid] + mod_span, mod_span)
    bin_idx <- findInterval(t_valid, bin_breaks, rightmost.closed = TRUE)

    ## extreme index per bin (first occurrence for ties)
    bin_extreme_idx <- unname(tapply(seq_len(n_valid), bin_idx, \(.idx) {
        .idx[which_fn(x_valid[.idx])]
    }, simplify = TRUE))

    ## find first bin-extreme where no forward mod_span value exceeds
    ## add tolerance for where mod_span loses floating point precision
    ## this is idx of x_valid, not `x`
    extreme_idx <- Find(\(.idx) {
        in_window <- t_valid >= t_valid[.idx] &
            t_valid <= t_valid[.idx] + mod_span + .Machine$double.eps^0.5
        compare_fn(x_valid[.idx], extreme_fn(x_valid[in_window]))
    }, bin_extreme_idx)

    if (!is.null(extreme_idx)) {
        ## map extreme back to original index space
        orig_extreme <- which_positive[extreme_idx]
        t_cutoff <- t[orig_extreme] + end_fit_span
        truncated <- which_valid[t[which_valid] <= t_cutoff]

        return(list(
            direction = direction,
            extreme = orig_extreme,
            idx = truncated
        ))
    }

    ## fallback: no qualifying extreme found
    return(invalid_return)
}


#' Gather per-interval `mnirs_kinetics` into results structure
#'
#' Shared helper for `analyse_kinetics.*` methods. Takes a list of
#' per-interval (per-data frame) kinetics results data frames (each carrying
#' `"fitted_data"`, `"channel_args"`, and `"diagnostics"` attributes), the
#' original `data_list`, and interval names.
#'
#' @param data_list Named list of original interval data frames.
#' @param result_list List of per-interval result data frames with attributes.
#'
#' @returns A named list with: `coefficients`, `model`, `data`,
#'   `interval_times`, `diagnostics`, `channel_args`.
#' @keywords internal
build_kinetics_results <- function(
    data_list,
    result_list,
    interval_names,
    method,
    call
) {
    ## extract per channel attrs; bind to df; add "interval" col; drop rownames
    flatten_attr <- function(attr_name) {
        df <- do.call(rbind, lapply(result_list, attr, attr_name))
        df$interval <- interval_names
        df <- df[, c("interval", setdiff(names(df), "interval"))]
        rownames(df) <- NULL
        return(df)
    }

    channel_args <- flatten_attr("channel_args")
    diagnostics <- flatten_attr("diagnostics")

    ## augment `data_list` dfs with `<nirs_channels>_fitted` columns
    fitted_data_list <- Map(\(.df, .result) {
        ## extract fitted columns from "fitted_data" per `nirs_channel`
        fitted_data <- attr(.result, "fitted_data")
        fitted_cols <- lapply(fitted_data, \(.pred) {
            fitted_vec <- rep(NA_real_, nrow(.df))
            fitted_vec[.pred$window_idx] <- .pred$fitted
            fitted_vec
        })
        names(fitted_cols) <- paste0(names(fitted_data), "_fitted")
        ## agument `<nirs_channels>_fitted` columns to df
        augmented <- cbind(.df, as.data.frame(fitted_cols))
        
        ## metadata ==================================================
        metadata <- attributes(.df)
        metadata$nirs_channels <- unique(.result$nirs_channels)
        metadata$time_channel <- unique(.result$time_channel)
        create_mnirs_data(augmented, metadata)
    }, data_list, result_list)

    ## extract interval_times from each data_list attributes, if exist
    interval_times_df <- data.frame(interval = interval_names)
    interval_times_df$interval_times <- lapply(data_list, \(.df) {
        interval_times <- attr(.df, "interval_times")
        if (is.null(interval_times)) NA_real_ else unlist(interval_times)
    })

    ## extract per-interval model lists (named by nirs_channel)
    model_list <- lapply(result_list, attr, "model")
    names(model_list) <- interval_names

    ## combine scalar coefficients & relocate interval col to col[1]
    coefs <- do.call(rbind, result_list)
    coefs <- coefs[, c("interval", setdiff(names(coefs), "interval"))]
    rownames(coefs) <- NULL

    return(structure(
        list(
            method = method,
            model = model_list,
            coefficients = coefs,
            data = fitted_data_list,
            interval_times = interval_times_df,
            diagnostics = diagnostics,
            channel_args = channel_args,
            call = call
        ),
        class = "mnirs_kinetics"
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
build_channel_args <- function(nirs_channel, all_args) {
    args_list <- lapply(all_args, \(.x) {
        if (is.null(.x)) {
            NA
        } else if (is.list(.x)) {
            deparse(.x)
        } else {
            .x
        }
    })
    ## omit internal args
    args_list[["verbose"]] <- NULL
    args_list[["bypass_checks"]] <- NULL
    return(data.frame(nirs_channels = nirs_channel, args_list))
}


#' Build a standardised NA result for a failed channel
#'
#' Returns the 5-element list (`coefficients`, `model`, `fitted_data`,
#' `diagnostics`, `channel_args`) expected by [build_kinetics_results()],
#' populated with `NA`/`NULL` values.
#'
#' @param nirs_channel Character; column name of the channel.
#' @param na_coefs A template 1-row `data.frame` with all `NA` values matching
#'   the method's coefficients columns.
#' @param all_args Named list of resolved arguments (passed to
#'   [build_channel_args()]).
#' @param n_params Integer; number of model parameters (passed to
#'   [compute_diagnostics()]).
#'
#' @returns A named list with elements `coefficients`, `model`,
#'   `fitted_data`, `diagnostics`, and `channel_args`.
#'
#' @keywords internal
build_na_results <- function(
    nirs_channel,
    na_coefs,
    all_args,
    n_params = 1L
) {
    na_diag <- data.frame(
        n_obs = 0L,
        r2 = NA_real_,
        adj_r2 = NA_real_,
        pseudo_r2 = NA_real_,
        rmse = NA_real_,
        snr = NA_real_,
        cv_rmse = NA_real_
    )
    na_coefs$nirs_channels <- nirs_channel
    return(list(
        coefficients = na_coefs,
        model = NULL,
        fitted_data = data.frame(window_idx = NA_integer_, fitted = NA_real_),
        diagnostics = cbind(data.frame(nirs_channels = nirs_channel), na_diag),
        channel_args = build_channel_args(nirs_channel, all_args)
    ))
}


#' Assemble per-channel results into an attributed data frame
#'
#' Combines the list of per-channel result lists (each with
#' `coefficients`, `fitted_data`, `diagnostics`, `channel_args`) into
#' the single attributed data frame that [build_kinetics_results()]
#' expects.
#'
#' @param results List of per-channel result lists.
#' @param nirs_channels Names to pass to list items
#'
#' @returns A `data.frame` with attributes `"model"`, `"fitted_data"`,
#'   `"diagnostics"`, and `"channel_args"`.
#' @keywords internal
build_channel_results <- function(results, nirs_channels) {
    return(structure(
        do.call(rbind, lapply(results, `[[`, "coefficients")),
        model = setNames(lapply(results, `[[`, "model"), nirs_channels),
        fitted_data = setNames(
            lapply(results, `[[`, "fitted_data"), nirs_channels
        ),
        diagnostics = do.call(rbind, lapply(results, `[[`, "diagnostics")),
        channel_args = do.call(rbind, lapply(results, `[[`, "channel_args"))
    ))
}
