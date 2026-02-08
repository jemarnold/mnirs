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
#' @return A numeric vector of predicted values the same length as
#'  the predictor variable `t`.
#' 
#' @seealso [SS_monoexp()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' set.seed(13)
#' t <- 1:60
#'
#' ## create an exponential curve with random noise
#' x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) + rnorm(length(t), 0, 3)
#' data <- data.frame(t, x)
#'
#' (model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data))
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
#' @return [monoexp_init()]: Initial starting estimates for parameters in the
#'   model called by [SS_monoexp3()] and [SS_monoexp4()].
#'
#' @keywords internal
monoexp_init <- function(mCall, data, LHS, ...) {
    ## self-start parameters for nls of monoexponential fit function
    ## uses base R `SSasymp()` initialisation approach
    ## https://www.statforbiology.com/2020/stat_nls_selfstarting/#and-what-about-nls
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
        rate <- -stats::coef(lm_fit)[2L]
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
#' @return [SS_monoexp3()] and [SS_monoexp4()]: A numeric vector of predicted values the same
#'   length as the predictor variable `t`.
#'
#' @seealso [monoexponential()], [stats::nls()], [stats::selfStart()],
#'   [SSasymp()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' set.seed(13)
#' t <- 1:60
#'
#' ## create an exponential curve with random noise
#' x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) + rnorm(length(t), 0, 3)
#' data <- data.frame(t, x)
#'
#' (model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data))
#'
#' y <- predict(model, data)
#'
#' library(ggplot2)
#' ggplot(data, aes(t, x)) +
#'     theme_mnirs() +
#'     geom_point() +
#'     geom_line(aes(y = y))
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
#' @keywords internal
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
