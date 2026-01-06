#' Monoexponential function with 4 parameters
#'
#' Calculate a four-parameter monoexponential curve.
#'
#' @param t A numeric vector of the predictor variable; time or sample number.
#' @param A A numeric parameter for the starting (baseline) value of the
#'   response variable.
#' @param B A numeric parameter for the ending (asymptote) value of the
#'   response variable.
#' @param TD A numeric parameter for the time delay before the onset of
#'   exponential response, in units of the predictor variable `t`.
#' @param tau A numeric parameter for the time constant `tau (𝜏)` of the
#'   exponential curve, in units of the predictor variable `t`.
#'
#' @details
#' Uses the equation:
#'   `ifelse(x <= TD, A, A + (B - A) * (1 - exp((TD - x) / tau)))`
#' `tau` is equal to the reciprocal of `k` (`tau = 1/k`), where `k` is the
#'   rate constant of the same function.
#'
#' @return A numeric vector of predicted values the same length as
#'  the predictor variable `t`.
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' set.seed(13)
#' t <- 1:60
#'
#' ## create an exponential curve with random noise
#' x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) + rnorm(length(t), 0, 3)
#' data <- data.frame(t, x)
#'
#' (model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data))
#'
#' y <- predict(model, data)
#'
#' library(ggplot2)
#' ggplot(data, aes(t, x)) +
#'     theme_mnirs() +
#'     geom_point() +
#'     geom_line(aes(y = y))
#'
#' @export
monoexponential <- function(t, A, B, TD, tau) {
    y <- A + (B - A) * (1 - exp(-(t - TD) / tau))
    y[t < TD] <- A

    return(y)
}


#' Initiate self-starting `nls` monoexponential model
#'
#' [monoexp_init()]: Returns initial values for the parameters in a `selfStart`
#' model.
#'
#' @param mCall A matched call to the function `model`.
#' @param data A data frame with time `t` and the response variable.
#' @param LHS The left-hand side expression of the model formula.
#' @param ... Additional arguments.
#'
#' @return [monoexp_init()]: Initial starting estimates for parameters in the
#'   model called by [SS_monoexp()].
#'
#' @rdname SS_monoexp
#' @order 2
#' @export
monoexp_init <- function(
    mCall,
    data,
    LHS,
    ...
) {
    ## self-start parameters for nls of monoexponential fit function
    ## https://www.statforbiology.com/2020/stat_nls_selfstarting/#and-what-about-nls
    xy <- stats::sortedXyData(mCall[["t"]], LHS, data)
    y <- xy[["y"]]
    x <- xy[["x"]]
    n <- length(y)

    # Detect direction from quartile means
    ## TRUE == UP, FALSE == DOWN
    q <- n %/% 4
    rising <- mean(y[seq_len(q)]) < mean(y[seq(n - q + 1, n)])

    ## A and B as top and bottom quantiles of y
    if (rising) {
        A <- stats::quantile(y, 0.05, names = FALSE)
        B <- stats::quantile(y, 0.95, names = FALSE)
    } else {
        A <- stats::quantile(y, 0.95, names = FALSE)
        B <- stats::quantile(y, 0.05, names = FALSE)
    }
    amplitude <- B - A

    # first x > 0 exceeding 10% amplitude from A
    td_idx <- which(x > 0 & abs(y - A) > abs(amplitude) * 0.1)[1L]
    TD <- if (is.na(td_idx)) 0 else x[td_idx]

    # tau: time from TD to 63.2% of amplitude
    target <- A + 0.632 * amplitude
    onset_idx <- which(x > TD)

    if (length(onset_idx) > 0) {
        near_tau_idx <- onset_idx[which.min(abs(y[onset_idx] - target))]
        tau <- max(x[near_tau_idx] - TD, 1)
    } else {
        tau <- (max(x) - TD) / 3
    }

    return(c(A = A, B = B, TD = TD, tau = tau))
}


#' Self-starting four-parameter monoexponential model
#'
#' [SS_monoexp()]: Creates initial coefficient estimates for a `selfStart`
#' model for the four-parameter [monoexponential()] function. For the
#' parameters `A`, `B`, `TD`, and `tau`.
#'
#' @inheritParams monoexponential
#'
#' @details
#' Uses the equation:
#'   `ifelse(x <= TD, A, A + (B - A) * (1 - exp((TD - x) / tau)))`
#'
#' @return [SS_monoexp()]: A numeric vector of predicted values the same
#'   length as the predictor variable `t`.
#'
#' @seealso [monoexponential()], [stats::nls()], [stats::selfStart()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' set.seed(13)
#' t <- 1:60
#'
#' ## create an exponential curve with random noise
#' x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) + rnorm(length(t), 0, 3)
#' data <- data.frame(t, x)
#'
#' (model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data))
#'
#' y <- predict(model, data)
#'
#' library(ggplot2)
#' ggplot(data, aes(t, x)) +
#'     theme_mnirs() +
#'     geom_point() +
#'     geom_line(aes(y = y))
#'
#' @rdname SS_monoexp
#' @order 1
#' @export
SS_monoexp <- selfStart(
    model = monoexponential,
    initial = monoexp_init,
    parameters = c("A", "B", "TD", "tau")
)


#' Update a model object with Fixed coefficients
#'
#' Re-fit a model with fixed coefficients provided as additional arguments. Fixed
#' coefficients are not modified when optimising for best fit.
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
#' @return An updated model object with remaining free coefficients.
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' set.seed(13)
#' t <- 1:60
#'
#' ## create an exponential curve with random noise
#' x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) + rnorm(length(t), 0, 3)
#' data <- data.frame(t, x)
#'
#' (model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data))
#' y <- predict(model, data)
#'
#' ## update the model with a priori fixed parameter
#' (model_fixed_TD <- fix_coefs(model, TD = 13))
#' y2 <- predict(model_fixed_TD, data)
#'
#' library(ggplot2)
#' ggplot(data, aes(t, x)) +
#'     theme_mnirs() +
#'     scale_colour_mnirs(name = NULL) +
#'     geom_point() +
#'     geom_line(aes(y = y, colour = "free")) +
#'     geom_line(aes(y = y2, colour = "fixed TD"))
#'
#' @keywords internal
#' @export
fix_coefs <- function(model, data = NULL, verbose = TRUE, ...) {
    current_coefs <- stats::coef(model)
    fixed_coefs <- list(...)

    ## validate coefs
    invalid <- setdiff(names(fixed_coefs), names(current_coefs))
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
            cli_abort(c(
                "x" = "Cannot retrieve original model data frame."
            ))
        }
    }

    ## get coef list from model and update in place from fixed coefs
    ## remove fixed coef from the start list
    start_coefs <- current_coefs[!names(current_coefs) %in% names(fixed_coefs)]

    if (length(start_coefs) == 0) {
        cli_abort(c(
            "x" = "Cannot update the model if all parameters are fixed. \\
            Nothing to estimate."
        ))
    }

    ## substitute fixed params into model_formula
    new_formula <- do.call(substitute, list(stats::formula(model), fixed_coefs))

    ## update the model
    stats::update(
        model,
        formula = new_formula,
        start = start_coefs,
        data = data
    )
}
