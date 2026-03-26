#' Monoexponential function with 4 parameters
#'
#' Calculate a four-parameter monoexponential curve.
#'
#' @param t A numeric vector of the predictor variable; time or sample number.
#' @param A A numeric parameter for the starting (baseline) value of the
#'   response variable.
#' @param B A numeric parameter for the ending (asymptote) value of the
#'   response variable.
#' @param tau A numeric parameter for the time constant `tau` (\eqn{\tau})
#'   of the exponential curve, in units of the predictor variable `t`.
#' @param TD A numeric parameter for the time delay before the onset of
#'   exponential response, in units of the predictor variable `t`. If `NULL`
#'   (*default*), a 3-parameter model without time delay is used.
#'
#' @details
#' 3-parameter model equation:
#'   `A + (B - A) * (1 - exp(-t / tau))`
#'
#' 4-parameter model equation:
#'   `ifelse(t <= TD, A, A + (B - A) * (1 - exp(-(t - TD) / tau)))`
#'
#' `tau` is the time constant and equal to the reciprocal of `k`, the rate
#'   constant (`k = 1/tau`).
#'
#' @returns A numeric vector of predicted values the same length as
#'  the predictor variable `t`.
#'
#' @seealso [SS_monoexp3()], [SS_monoexp4()]
#'
#' @examples
#' set.seed(13)
#' t <- 1:60
#'
#' ## create an exponential curve with random noise
#' x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
#'     rnorm(length(t), 0, 3)
#' data <- data.frame(t, x)
#'
#' model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)
#'
#' model
#'
#' y <- predict(model, data)
#'
#' y
#'
#' \donttest{
#'     if (requireNamespace("ggplot2", quietly = TRUE)) {
#'         ggplot2::ggplot(data, ggplot2::aes(t, x)) +
#'             theme_mnirs() +
#'             ggplot2::geom_point() +
#'             ggplot2::geom_line(ggplot2::aes(y = y))
#'     }
#' }
#'
#' @export
monoexponential <- function(t, A, B, tau, TD = NULL) {
    if (is.null(TD)) {
        ## 3-parameter: no time delay
        y <- A + (B - A) * (1 - exp(-t / tau))
        return(y)
    } else {
        ## 4-parameter: with time delay
        y <- A + (B - A) * (1 - exp(-(t - TD) / tau))
        y[t < TD] <- A
        return(y)
    }
}


#' @keywords internal
monoexp3 <- function(t, A, B, tau) {
    A + (B - A) * (1 - exp(-t / tau))
}


#' Initiate self-starting monoexponential model
#'
#' [monoexp_init()]: Returns initial values for the parameters in a `selfStart`
#' model.
#'
#' @param mCall A matched call to the function `model`.
#' @param data A data frame with time `t` and the response variable.
#' @param LHS The left-hand side expression of the model formula.
#' @param ... Additional arguments.
#'
#' @returns [monoexp_init()]: Initial starting estimates for parameters in the
#'   model called by [SS_monoexp3()] and [SS_monoexp4()].
#'
#' @keywords internal
monoexp_init <- function(mCall, data, LHS, ...) {
    ## self-start parameters for nls of monoexponential fit function
    ## uses base R `SSasymp()` initialisation approach
    xy <- stats::sortedXyData(mCall[["t"]], LHS, data)
    y <- xy[["y"]]
    x <- xy[["x"]]
    n <- length(y)

    ## check if TD parameter exists in the call
    has_TD <- "TD" %in% names(mCall)

    ## fit linear model to log-transformed differences from estimated asymptote
    ## initial asymptote guess from last values
    n_tail <- max(1, ceiling(n / 5))
    B_init <- mean(y[seq(n - n_tail + 1, n)])

    ## initial baseline from first values
    n_head <- max(1, ceiling(n / 5))
    A_init <- mean(y[seq_len(n_head)])

    ## estimate rate constant via linearisation (SSasymp method)
    ## log(B - y) ~ log(B - A) - t/tau
    ## use shifted y to avoid log of negative/zero
    y_shifted <- B_init - y
    y_shifted[y_shifted <= 0] <- min(y_shifted[y_shifted > 0]) / 2

    if (sum(y_shifted > 0) >= 3) {
        lm_fit <- stats::lm(log(y_shifted) ~ x)
        rate <- -coef(lm_fit)[2L]
        tau_init <- if (is.finite(rate) && rate > 0) {
            1 / rate
        } else {
            diff(range(x)) / 3
        }
    } else {
        ## fallback: tau from 63.2% rise point
        target <- A_init + 0.632 * (B_init - A_init)
        tau_init <- x[which.min(abs(y - target))]
        tau_init <- max(tau_init, diff(range(x)) / 10)
    }

    tau_init <- max(tau_init, .Machine$double.eps)

    if (has_TD) {
        ## 4-parameter: estimate time delay from derivative changepoint
        dy_dx <- abs(diff(y) / diff(x))
        td_idx <- which.max(dy_dx)
        TD_init <- max(x[td_idx] - tau_init * 0.1, 0)
        return(c(A = A_init, B = B_init, tau = tau_init, TD = TD_init))
    } else {
        ## 3-parameter: no time delay
        return(c(A = A_init, B = B_init, tau = tau_init))
    }
}


#' Self-starting monoexponential models
#'
#' [SS_monoexp3()]: Creates initial coefficient estimates for a `selfStart`
#' model for a 3-parameter [monoexponential()] function (A, B, tau).
#'
#' @usage
#' SS_monoexp3(t, A, B, tau)
#'
#' @inheritParams monoexponential
#'
#' @details
#' For 3-parameter model: `y ~ SS_monoexp3(t, A, B, tau)`
#'
#' For 4-parameter model: `y ~ SS_monoexp4(t, A, B, tau, TD)`
#'
#' The 3-parameter model is recommended for small samples or when no obvious
#'   time delay exists, as it converges more reliably.
#'
#' @returns [SS_monoexp3()] and [SS_monoexp4()]: A numeric vector of predicted
#'   values the same length as the predictor variable `t`.
#'
#' @seealso [monoexponential()], [stats::nls()], [stats::selfStart()],
#'   [SSasymp()]
#'
#' @examples
#' set.seed(13)
#' t <- 1:60
#'
#' ## create an exponential curve with random noise
#' x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
#'     rnorm(length(t), 0, 3)
#' data <- data.frame(t, x)
#'
#' model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)
#'
#' model
#'
#' y <- predict(model, data)
#'
#' y
#'
#' \donttest{
#'     if (requireNamespace("ggplot2", quietly = TRUE)) {
#'         ggplot2::ggplot(data, ggplot2::aes(t, x)) +
#'             theme_mnirs() +
#'             ggplot2::geom_point() +
#'             ggplot2::geom_line(ggplot2::aes(y = y))
#'     }
#' }
#'
#' @name SS_monoexp
#' @rdname SS_monoexp
#' @order 1
#' @export
SS_monoexp3 <- selfStart(
    model = monoexp3,
    initial = monoexp_init,
    parameters = c("A", "B", "tau")
)


#' Self-starting monoexponential models
#'
#' [SS_monoexp4()] supports a 4-parameter [monoexponential()] function
#' (A, B, tau, TD).
#'
#' @param TD A numeric parameter for the time delay before the onset of
#'   exponential response, in units of the predictor variable `t`.
#'
#' @usage
#' SS_monoexp4(t, A, B, tau, TD)
#'
#' @name SS_monoexp
#' @rdname SS_monoexp
#' @order 2
#' @export
SS_monoexp4 <- selfStart(
    model = monoexponential,
    initial = monoexp_init,
    parameters = c("A", "B", "tau", "TD")
)


#' Analyse monoexponential kinetics per channel
#'
#' Fit a monoexponential curve to each `nirs_channel` within a single
#' data frame. Called by [analyse_kinetics()] when `method = "monoexponential"`.
#'
#' @param time_delay Logical; default is `TRUE` to attempt to fit a
#'   4-parameter [SS_monoexp4()] model (A, B, tau, TD) with a time delay.
#'   If the 4-parameter fit fails, or if `time_delay = FALSE`, fits a
#'   reduced 3-parameter [SS_monoexp3()] model (A, B, tau).
#' @inheritParams validate_mnirs
#' @inheritParams analyse_kinetics
#'
#' @returns A `data.frame` with one row per `nirs_channel` and columns
#'   `nirs_channels`, `A`, `B`, `tau`, `k`, `TD`, `MRT`, `HRT`, `tau_fitted`,
#'   `MRT_fitted`, `HRT_fitted`.
#'   Per-channel metadata are attached as attributes:
#'   - `"fitted_data"`: a named list of data frames (per `nirs_channel`)
#'     with columns `window_idx` and `fitted`.
#'   - `"diagnostics"`: a `data.frame` with one row per `nirs_channel`
#'     containing model fit diagnostics.
#'   - `"channel_args"`: a `data.frame` with one row per `nirs_channel`
#'     recording the resolved arguments used.
#'
#' @seealso [analyse_kinetics()], [monoexponential()], [SS_monoexp3()], 
#'   [SS_monoexp4()]
#'
#' @keywords internal
analyse_monoexponential <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    time_delay = TRUE,
    direction = c("auto", "positive", "negative"),
    end_fit_span = 20,
    channel_args = list(),
    verbose = TRUE,
    ...
) {
    ## validation ==================================================
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }
    validate_mnirs_data(data)
    nirs_channels <- validate_nirs_channels(
        enquo(nirs_channels), data, verbose = FALSE
    )
    time_channel <- validate_time_channel(enquo(time_channel), data)
    if (!is.logical(time_delay) || length(time_delay) != 1L) {
        cli_abort(c(
            "x" = "{.arg time_delay} must be a single {.cls logical} value."
        ))
    }

    time_vec <- data[[time_channel]]
    interval_names <- list(...)$interval_names %||% substitute(data)

    default_args <- list(
        time_delay = time_delay,
        verbose = verbose,
        ...
    )

    ## NA scaffold for convergence failure
    na_coefs <- data.frame(
        nirs_channels = NA_character_,
        A = NA_real_,
        B = NA_real_,
        tau = NA_real_,
        k = NA_real_,
        TD = NA_real_,
        MRT = NA_real_,
        HRT = NA_real_,
        tau_fitted = NA_real_,
        MRT_fitted = NA_real_,
        HRT_fitted = NA_real_
    )

    ## process per-channel ============================================
    results <- lapply(nirs_channels, \(.nirs) {
        all_args <- utils::modifyList(
            default_args, channel_args[[.nirs]] %||% list()
        )
        ## derive n_params from time_delay for internal use
        n_params <- if (all_args$time_delay) 4L else 3L

        ## filter for valid finite idx before first extreme + end_fit_span
        valid <- find_kinetics_idx(
            data[[.nirs]],
            time_vec,
            end_fit_span,
            direction
        )
        x_fit <- data[[.nirs]][valid]
        t_fit <- time_vec[valid]

        fit_data <- data.frame(.x = x_fit, .t = t_fit)
        model <- NULL ## pre-allocate for 3-param condition

        ## attempt nls fit on 4-param, fallback to 3
        if (n_params == 4L) {
            model <- tryCatch(
                stats::nls(.x ~ SS_monoexp4(.t, A, B, tau, TD), fit_data),
                error = \(e) {
                    if (verbose) {
                        cli_warn(c(
                            "x" = "4-parameter {.fn SS_monoexp4} fit failed \\
                            for {.field {(.nirs)}} in \\
                            {.field {interval_names}}.",
                            "!" = "{conditionMessage(e)}",
                            "i" = "Attempting 3-parameter fit with \\
                            {.fn SS_monoexp3}."
                        ))
                    }
                    NULL
                }
            )
        }

        if (is.null(model)) {
            ## fallback to 3-param model
            n_params <- 3L
        }

        if (n_params == 3L) {
            model <- tryCatch(
                stats::nls(.x ~ SS_monoexp3(.t, A, B, tau), fit_data),
                error = \(e) {
                    if (verbose) {
                        cli_warn(c(
                            "x" = "3-parameter {.fn SS_monoexp3} fit failed \\
                            for {.field {(.nirs)}} in \\
                            {.field {interval_names}}.",
                            "!" = "{conditionMessage(e)}"
                        ))
                    }
                    NULL
                }
            )
        }

        if (is.null(model)) {
            return(build_na_results(.nirs, na_coefs, all_args, n_params))
        }

        fitted_vals <- stats::predict(model)
        coefs <- stats::coef(model)
        TD_arg <- if (n_params == 4L) coefs[["TD"]] else NULL
        TD_val <- TD_arg %||% NA_real_
        MRT_val <- sum(TD_arg, coefs[["tau"]])
        HRT_val <- sum(TD_arg, coefs[["tau"]] * log(2))

        ## predict response at tau and MRT using the fitted model
        tau_fitted <- monoexponential(
            coefs[["tau"]], coefs[["A"]], coefs[["B"]], coefs[["tau"]], TD_arg
        )
        MRT_fitted <- monoexponential(
            MRT_val, coefs[["A"]], coefs[["B"]], coefs[["tau"]], TD_arg
        )
        HRT_fitted <- monoexponential(
            HRT_val, coefs[["A"]], coefs[["B"]], coefs[["tau"]], TD_arg
        )

        ## ! fix cases where `zero_time = FALSE`: esp. for half_time
        coefs <- data.frame(
            nirs_channels = .nirs,
            A = coefs[["A"]],
            B = coefs[["B"]],
            tau = coefs[["tau"]],
            k = 1 / coefs[["tau"]],
            TD = TD_val,
            MRT = MRT_val,
            HRT = HRT_val,
            tau_fitted = tau_fitted,
            MRT_fitted = MRT_fitted,
            HRT_fitted = HRT_fitted
        )

        diag <- compute_diagnostics(
            x_fit, t_fit, fitted_vals, n_params, verbose
        )

        list(
            coefficients = coefs,
            model = model,
            fitted_data = data.frame(window_idx = valid, fitted = fitted_vals),
            diagnostics = cbind(data.frame(nirs_channels = .nirs), diag),
            channel_args = build_channel_args(.nirs, all_args)
        )
    })
    names(results) <- nirs_channels

    return(build_channel_results(results))
}


#' Update a model object with Fixed coefficients
#'
#' Re-fit a model with fixed coefficients provided as additional arguments.
#' Fixed coefficients are not modified when optimising for best fit.
#'
#' @param model An existing model object from `lm`, `nls`, `glm`, and others.
#' @param data An *optional* data frame to supply manually if original data
#'   frame is unavailable from a different parent environment.
#' @param ... Named model coefficients to fix.
#' @inheritParams validate_mnirs
#'
#' @details
#' If no fixed coefficients are supplied, or if a coefficient does not exist
#'   in the model, the model will be returned unchanged (with a warning).
#'
#' The function cannot update if all model coefficients are supplied as fixed,
#'   and will abort.
#'
#' @returns An updated model object with remaining free coefficients.
#'
#' @keywords internal
fix_coefs <- function(model, data = NULL, verbose = TRUE, ...) {
    current_coefs <- coef(model)
    fixed_coefs <- list(...)
    fixed_names <- names(fixed_coefs)
    current_names <- names(current_coefs)

    ## validate coefs
    invalid <- setdiff(fixed_names, current_names)
    if (verbose && length(invalid) > 0) {
        cli_warn(c(
            "x" = "Unknown model coefficient{?s}: {.val {invalid}}.",
            "i" = "Returning model with known coefficients."
        ))
    }

    ## extract data from the model environment
    if (is.null(data)) {
        data <- tryCatch(
            eval(model$call$data, envir = environment(stats::formula(model))),
            error = \(e) {
                ## fallback: try parent frames
                eval(model$call$data, envir = parent.frame(3))
            }
        )

        if (is.null(data)) {
            cli_abort(c("x" = "Cannot retrieve original model data frame."))
        }
    }

    ## get coef list from model and update in place from fixed coefs
    ## remove fixed coef from the start list
    start_coefs <- current_coefs[!current_names %in% fixed_names]

    if (length(start_coefs) == 0) {
        cli_abort(c(
            "x" = "Cannot update the model if all parameters are fixed. \\
            Nothing to estimate."
        ))
    }

    ## substitute fixed params into model_formula
    new_formula <- do.call(substitute, list(stats::formula(model), fixed_coefs))

    ## update the model
    return(stats::update(
        model,
        formula = new_formula,
        start = start_coefs,
        data = data
    ))
}
