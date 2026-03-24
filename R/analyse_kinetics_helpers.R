#' Find valid model-fitting indices up to the first extreme
#'
#' Filters `x` and `t` to valid finite values, locates the first valid peak 
#' (maximum) or trough (minimum) where `t >= 0`, and returns the integer 
#' indices of all finite observations up to `end_fit_span` past that extreme.
#'
#' @param end_fit_span A numeric value in units of `t` specifying the
#'   forward-looking window used to check for subsequent greater/lesser
#'   values than the candidate extreme.
#' @inheritParams peak_slope
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
#' @returns An integer vector of indices into `x` (and `t`) where both
#'   values are finite, truncated at the last index where
#'   `t <= t[extreme] + end_fit_span`.
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
        validate_x_t(x, t, invalid = TRUE)
        validate_numeric(
            end_fit_span, 1, c(0, Inf), msg1 = "one-element positive"
        )
        direction <- match.arg(direction)
    }

    ## all finite indices (including t < 0)
    finite_mask <- which(is.finite(x) & is.finite(t))
    ## subset where t >= 0 for extreme detection
    positive_mask <- finite_mask[t[finite_mask] >= 0]
    x_valid <- x[positive_mask]
    t_valid <- t[positive_mask]
    n_valid <- length(x_valid)

    ## early returns for degenerate inputs
    if (n_valid < 2L) {
        return(finite_mask)
    }

    ## direction detection ========================================
    if (direction == "auto") {
        net_slope <- slope(x, t, na.rm = TRUE, bypass_checks = TRUE)

        direction <- if (is.na(net_slope) || net_slope == 0) {
            ## compare abs magnitudes of global max vs global min
            max_pos <- abs(max(x, na.rm = TRUE))
            min_neg <- abs(min(x, na.rm = TRUE))
            if (max_pos >= min_neg) "positive" else "negative"
        } else if (net_slope > 0) {
            "positive"
        } else {
            "negative"
        }
    }

    extreme_fn <- if (direction == "positive") max else min
    compare_fn <- if (direction == "positive") `>=` else `<=`
    which_fn   <- if (direction == "positive") which.max else which.min

    ## monotonic: if global extreme is the last x value
    ## horizontal: if all x values equal
    if (which_fn(x_valid) == n_valid || all(x_valid == x_valid[1L])) {
        return(finite_mask)
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
    bin_extreme_idx <- unname(
        tapply(seq_len(n_valid), bin_idx, \(.idx) {
                .idx[which_fn(x_valid[.idx])]
        }, simplify = TRUE)
    )

    ## find first bin-extreme where no forward mod_span value exceeds
    ## add tolerance for where mod_span loses floating point precision
    ## this is idx of x_valid, not `x`
    extreme_idx <- Find(\(.idx) {
        in_window <- t_valid >= t_valid[.idx] &
            t_valid <= t_valid[.idx] + mod_span + .Machine$double.eps^0.5
        compare_fn(x_valid[.idx], extreme_fn(x_valid[in_window]))
    }, bin_extreme_idx)

    if (!is.null(extreme_idx)) {
        ## map extreme back to original index space via positive_mask
        orig_extreme <- positive_mask[extreme_idx]
        t_cutoff <- t[orig_extreme] + end_fit_span
        return(finite_mask[t[finite_mask] <= t_cutoff])
    }

    ## fallback: no qualifying extreme found
    return(finite_mask)
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
    interval_names
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
        ## preserve metadata to augmented data frames
        create_mnirs_data(augmented, attributes(.df))
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

    return(list(
        coefficients = coefs,
        model = model_list,
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
#' @param results Named list of per-channel result lists.
#'
#' @returns A `data.frame` with attributes `"model"`, `"fitted_data"`,
#'   `"diagnostics"`, and `"channel_args"`.
#' @keywords internal
build_channel_results <- function(results) {
    return(structure(
        do.call(rbind, lapply(results, `[[`, "coefficients")),
        model = lapply(results, `[[`, "model"),
        fitted_data = lapply(results, `[[`, "fitted_data"),
        diagnostics = do.call(rbind, lapply(results, `[[`, "diagnostics")),
        channel_args = do.call(rbind, lapply(results, `[[`, "channel_args"))
    ))
}
