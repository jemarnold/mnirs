#' Calculate fractional kinetics response time
#'
#' Identify the time at which a signal reaches a specified fraction of its
#' total response amplitude relative to a baseline period (e.g. *half-response
#' time* at 50% fractional amplitude).
#'
#' @param t0 A numeric value specifying the start of the kinetics response 
#'   in units of `t`. Observations where `t <= t0` define the baseline window. 
#'   Defaults to `0`.
#' @param fraction A numeric value in `[0, 1]` specifying the fractional
#'   response amplitude to detect. Defaults to `0.5` (50% response, i.e.
#'   half-response time).
#' @param verbose Logical; if `TRUE` (*default*), informational messages are
#'   shown. Controlled by `getOption("mnirs.verbose")`.
#' @param ... Additional arguments.
#' @inheritParams replace_invalid
#' @inheritParams find_kinetics_idx
#' @inheritParams read_mnirs
#'
#' @details
#' ## Method
#'
#' The target response value is computed as:
#'
#' `response_fitted = A + (B - A) * fraction`
#'
#' where `A` is the mean baseline (`t <= t0`) and `B` is the extreme (peak
#' or trough) value after `t0`. The response time is the elapsed time from `t0` 
#' to the first sample where `x` reaches or exceeds the `response_fitted` value
#' (or where it falls to or below, for negative direction).
#'
#' ## Direction
#'
#' When `direction = "auto"`, the net slope across `x` determines the overall
#' trend. If the net slope is zero or `NA`, the direction of greatest absolute
#' change is used. When `direction = "positive"` or `"negative"`, the signal
#' extreme is the maximum or minimum value after `t0`, respectively.
#'
#' ## Baseline
#'
#' When no observations exist where `t <= t0`, the first sample `x[1]` is used 
#' as the baseline. `t0` cannot exceed the maximum of `t`.
#'
#' @returns A named list containing:
#'   \item{`A`}{Mean baseline value (mean of `x` where `t <= t0`).}
#'   \item{`B`}{Extreme value (maximum or minimum) after `t0`.}
#'   \item{`response_time`}{Elapsed time from `t0` to the fractional response,
#'   in units of `t`.}
#'   \item{`response_value`}{The observed signal value at the response index.}
#'   \item{`fitted`}{The predicted fractional response value
#'   (`A + (B - A) * fraction`).}
#'   \item{`baseline_idx`}{Integer indices where `t <= t0`.}
#'   \item{`response_idx`}{Integer index at the `response_value`.}
#'   \item{`extreme_idx`}{Integer index at the extreme value (`B`).}
#'
#' @seealso [analyse_response_time()], [analyse_kinetics()]
#'
#' @examples
#' set.seed(13)
#' t <- 0:60
#' x <- monoexponential(t, A = 20, B = 60, tau = 8, TD = 10) + rnorm(length(t), 0, 1)
#' 
#' ## estimated half-response time
#' HRT <- response_time(x, t, t0 = 10, fraction = 0.5)
#' 
#' ## estimated mean response time (time constant; tau ~= 63.2% amplitude)
#' MRT <- response_time(x, t, t0 = 10, fraction = 0.632)
#' 
#' plot(t, x, type = "l", col = "grey60", xlab = "t", ylab = "x")
#' ## baseline mean across baseline_idx
#' segments(
#'     t[min(HRT$baseline_idx)], HRT$A,
#'     t[max(HRT$baseline_idx)], HRT$A,
#'     col = "red", lwd = 2
#' )
#' ## fraction = 0.5 (red): response_value and extreme
#' points(t[HRT$response_idx], HRT$response_value, col = "red", pch = 19)
#' points(t[HRT$extreme_idx], HRT$B, col = "red", pch = 19)
#' ## fraction = 0.632 (blue): response_value
#' points(t[MRT$response_idx], MRT$response_value, col = "blue", pch = 19)
#'
#' @export
response_time <- function(
    x,
    t = seq_along(x),
    t0 = 0,
    fraction = 0.5,
    end_fit_span = 20,
    direction = c("auto", "positive", "negative"),
    verbose = TRUE,
    ...
) {
    validate_numeric(
        fraction, 1L, c(0, 1), msg2 = "between {col_blue('[0, 1]')}."
    )
    validate_numeric(
        end_fit_span, 1L, c(0, Inf), "left", msg2 = ">= {col_blue('0')}."
    )

    if (!(list(...)$bypass_checks %||% FALSE)) {
        validate_x_t(x, t, allow_na = TRUE)
        if (missing(verbose)) {
            verbose <- getOption("mnirs.verbose", default = TRUE)
        }
        direction <- match.arg(direction)
        ## detect direction from net trend, fallback to abs magnitude
        direction <- detect_direction(x, t, x, direction)
    }

    ## process =====================================================
    compare_fn <- if (direction == "positive") `>=` else `<=`
    baseline_idx <- which(t <= t0)
    if (length(baseline_idx) == 0L) {
        if (verbose) {
            cli_warn(c(
                "!" = "No observations where {.arg t} <= {.val {t0}}. \\
                {.code x[1]} used as baseline."
            ))
        }
        baseline_idx <- 1L
        t0 <- t[baseline_idx]
    }
    if (t0 > rev(t)[1L]) {
        cli_abort(c(
            "x" = "No observations in {.arg t} before {.arg t0}.",
            "i" = "{.arg t0} must be specified within the range of {.arg t}."
        ))
    }

    ## look for extreme after t0
    x_valid <- c(rep(NA_real_, length(baseline_idx)), x[t > t0])
    extreme_idx <- if (direction == "positive") {
        which.max(x_valid)
    } else {
        which.min(x_valid)
    }

    A <- mean(x[baseline_idx], na.rm = TRUE)
    B <- x[extreme_idx]
    response_fitted <- A + (B - A) * fraction
    response_idx <- which(compare_fn(x_valid, response_fitted))[1L]

    if (is.na(response_idx)) {
        if (verbose) {
            cli_warn(c(
                "!" = "No valid {.val {direction}} extremes after {.arg t0}. \\ Returning {.val {NA}}."
            ))
        }
        response_fitted <- NA_real_
    }

    return(list(
        A = A,
        B = B,
        response_time = t[response_idx] - t0, ## real
        response_value = x[response_idx], ## real
        fitted = response_fitted, ## predicted
        baseline_idx = baseline_idx,
        response_idx = response_idx,
        extreme_idx = extreme_idx
    ))
}


#' Analyse fractional response time across NIRS channels
#'
#' Compute the fractional response time for each `nirs_channel` within a
#' single *"mnirs"* data frame and return a data frame of parameters with
#' per-channel metadata as attributes. Called by [analyse_kinetics()] when
#' `method = "response_time"` or `"HRT"`.
#'
#' @param t0 A numeric value specifying the start of the kinetics response,
#'   in units of `time_channel`. Observations where `t <= t0` define the 
#'   baseline window. Defaults to `0` or retrieves `interval_times` from 
#'   *"mnirs"* metadata.
#' @inheritParams validate_mnirs
#' @inheritParams analyse_kinetics
#' @inheritParams response_time
#'
#' @details
#' ## Per-channel argument overrides
#'
#' Arguments passed to `analyse_response_time()` apply to all `nirs_channels`
#' by default. `channel_args` allows overriding any argument for individual
#' channels, e.g.:
#'
#' ```r
#' analyse_response_time(
#'     data = df,
#'     nirs_channels = c(hhb, smo2),
#'     fraction = 0.5,
#'     direction = "positive",
#'     channel_args = list(
#'         smo2 = list(fraction = 0.632),
#'         hhb  = list(direction = "negative")
#'     )
#' )
#' ```
#'
#' @returns A `data.frame` with one row per `nirs_channel` and columns
#'   `nirs_channels`, `A`, `B`, `response_time`, `response_value`,
#'   `fitted`, `idx`.
#'   Per-channel metadata are attached as attributes:
#'   - `"model"`: `NULL` (no parametric model is fitted).
#'   - `"fitted_data"`: a named list of data frames (per `nirs_channel`)
#'     with columns `window_idx` and `fitted`, containing the baseline,
#'     response, and extreme key points.
#'   - `"diagnostics"`: a `data.frame` with one row per `nirs_channel`
#'     containing model fit diagnostics.
#'   - `"channel_args"`: a `data.frame` with one row per `nirs_channel`
#'     recording the resolved arguments used.
#'
#' @seealso [analyse_kinetics()], [response_time()]
#'
#' @keywords internal
analyse_response_time <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    t0 = 0,
    fraction = 0.5,
    direction = c("auto", "positive", "negative"),
    end_fit_span = 20,
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    ## validation ==============================================
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }
    validate_mnirs_data(data)
    nirs_channels <- validate_nirs_channels(enquo(nirs_channels), data, verbose)
    time_channel <- validate_time_channel(enquo(time_channel), data)
    direction <- match.arg(direction)

    time_vec <- data[[time_channel]]
    default_args <- list(
        t0 = t0, ## ! implement t0 (interval_times?) in other analyse_* fns
        fraction = fraction,
        direction = direction,
        end_fit_span = end_fit_span,
        verbose = verbose,
        bypass_checks = TRUE,
        ...
    )

    ## process per-channel =================================
    results <- lapply(nirs_channels, \(.nirs) {
        ## TODO consider refactoring explicitly for lower overhead
        all_args <- utils::modifyList(
            default_args, channel_args[[.nirs]] %||% list()
        )
        
        ## filter for valid finite idx before first extreme + end_fit_span
        valid <- find_kinetics_idx(
            data[[.nirs]], time_vec, all_args$end_fit_span, all_args$direction
        )
        all_args$direction <- valid$direction
        x_fit <- data[[.nirs]][valid$idx]
        t_fit <- time_vec[valid$idx]

        response <- do.call(
            response_time, c(list(x = x_fit, t = t_fit), all_args)
        )
        
        coefs <- data.frame(
            nirs_channels  = .nirs,
            time_channel   = time_channel,
            A              = response$A,
            B              = response$B,
            response_time  = response$response_time,
            response_value = response$response_value,
            fitted         = response$fitted,
            idx            = response$response_idx
        )
        diag <- compute_diagnostics(
            x              = x_fit[1L:3L], ## placeholder
            t              = t_fit[1L:3L], ## placeholder
            fitted         = c(coefs$A, coefs$fitted, coefs$B),
            n_params       = 0L, ## invalid for response time method
            verbose        = verbose
        )

        ## bind baseline vec with `A`, and response and extreme scalars
        fitted_data <- data.frame(
            window_idx = c(
                response$baseline_idx,
                response$response_idx,
                response$extreme_idx
            ),
            fitted = c(
                rep(response$A, length(response$baseline_idx)),
                response$fitted,
                response$B
            )
        )

        list(
            coefficients = coefs,
            model = NULL,
            fitted_data = fitted_data,
            diagnostics = cbind(data.frame(nirs_channels = .nirs), diag),
            channel_args = build_channel_args(.nirs, all_args)
        )
    })

    ## coefs tibble with per-channel metadata as attributes
    return(build_channel_results(results, nirs_channels))
}
