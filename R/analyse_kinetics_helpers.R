#' Find the first extreme peak or trough value in a numeric vector to constrain
#' model fit
#'
#' Locate the first peak (maximum) or trough (minimum) in `x` such that
#' no value within the following `end_fit_span` time span exceeds it.
#'
#' @param end_fit_span A numeric value in units of `t` specifying 
#'   the forward-looking window used to check for subsequent greater/lesser 
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
#' Only samples where `t > 0` are processed, allowing pre-baseline
#' (negative time) data to be excluded from the search.
#'
#' @returns A numeric index at the last index where
#' `t <= t[extreme_idx] + end_fit_span`, constrained by positive
#' values of `t` at the start, and the end range of `x`.
#'
#' @keywords internal
find_first_extreme <- function(
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

    finite_mask <- which(is.finite(x) & t >= 0)
    x_valid <- x[finite_mask]
    t_valid <- t[finite_mask]
    n_valid <- length(x_valid)

    ## early returns for degenerate inputs
    if (n_valid < 2L) {
        return(n)
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
    which_fn <- if (direction == "positive") which.max else which.min

    ## monotonic: if global extreme is the last x value
    ## horizontal: if all x values equal
    if (which_fn(x_valid) == n_valid || all(x_valid == x_valid[1L])) {
        return(n)
    }

    ## process ==================================================
    ## bin by end_fit_span, find extreme per bin, then check forward window

    ## ensure end_fit_span covers at least one adjacent sample when end_fit_span is
    ## smaller than the minimum time step
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
        ## map `extreme_idx` back to original index space
        extreme_idx <- finite_mask[extreme_idx]
        end_idx <- which(t_valid <= t_valid[extreme_idx] + end_fit_span)
        end_idx <- min(max(end_idx), n)
        return(end_idx)
    }

    ## fallback: no qualifying extreme found
    return(n)
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
        return(df)
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

    ## extract interval_times from each data_list attributes, if exist
    interval_times_df <- data.frame(interval = interval_names)
    interval_times_df$interval_times <- lapply(data_list, \(.df) {
        interval_times <- attr(.df, "interval_times")
        if (is.null(interval_times)) NA_real_ else unlist(interval_times)
    })

    ## combine scalar coefficients & relocate interval col to col[1]
    coefs <- do.call(rbind, result_list)
    coefs <- coefs[, c("interval", setdiff(names(coefs), "interval"))]
    rownames(coefs) <- NULL

    return(list(
        coefficients = coefs,
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
#' Returns the 4-element list (`coefficients`, `predicted`, `diagnostics`,
#' `channel_args`) expected by [gather_kinetics()], populated with `NA` values.
#'
#' @param nirs_channel Character; column name of the channel.
#' @param na_coefs A template 1-row `data.frame` with all `NA` values matching
#'   the method's coefficients columns.
#' @param all_args Named list of resolved arguments (passed to
#'   [safe_channel_args()]).
#' @param n_params Integer; number of model parameters (passed to
#'   [compute_diagnostics()]).
#'
#' @returns A named list with elements `coefficients`, `predicted`,
#'   `diagnostics`, and `channel_args`.
#'
#' @keywords internal
build_na_reults <- function(
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
        predicted = data.frame(window_idx = NA_integer_, fitted = NA_real_),
        diagnostics = cbind(data.frame(nirs_channels = nirs_channel), na_diag),
        channel_args = safe_channel_args(nirs_channel, all_args)
    ))
}


#' Assemble per-channel results into an attributed data frame
#'
#' Combines the list of per-channel result lists (each with
#' `coefficients`, `predicted`, `diagnostics`, `channel_args`) into
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
        do.call(rbind, lapply(results, `[[`, "coefficients")),
        predicted = lapply(results, `[[`, "predicted"),
        diagnostics = do.call(rbind, lapply(results, `[[`, "diagnostics")),
        channel_args = do.call(rbind, lapply(results, `[[`, "channel_args"))
    ))
}
