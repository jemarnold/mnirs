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
#' 
#' x <- seq(0, 60, by = 2)
#' A <- 10
#' B <- 100
#' TD <- 15
#' tau <- 8
#' y <- monoexponential(x, A, B, TD, tau) + rnorm(length(x), 0, 3)
#' data <- data.frame(x, y)
#' 
#' nls(y ~ SS_monoexp(x, A, B, TD, tau), data = data)
#' 
#' ## TODO ADD PLOT
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
        mCall, data, LHS, ...
) {
    ## self-start parameters for nls of monoexponential fit function
    ## https://www.statforbiology.com/2020/stat_nls_selfstarting/#and-what-about-nls
    xy <- sortedXyData(mCall[["t"]], LHS, data)
    y <- xy[["y"]]
    x <- xy[["x"]]

    # Detect direction from quartile means
    ## TRUE == UP, FALSE == DOWN
    rising <- mean(y[seq_len(n %/% 4)]) < mean(y[seq(n - n %/% 4 + 1, n)])

    A <- if (rising) min(y) else max(y)
    B <- if (rising) max(y) else min(y)
    amplitude <- B - A

    # TD: first x > 0 exceeding 5% amplitude from A
    td_idx <- which(x > 0 & abs(y - A) > abs(amplitude) * 0.05)[1L]
    TD <- if (is.na(td_idx)) 0 else x[td_idx]

    # tau: time from TD to 63.2% of amplitude
    target <- A + 0.632 * amplitude
    tau_idx <- which(abs(y - target) < abs(y - A) & x > TD)[1L]
    tau <- if (is.na(tau_idx)) (max(x) - TD) / 3 else max(x[tau_idx] - TD, 0.1)

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
#' 
#' x <- seq(0, 60, by = 2)
#' A <- 10
#' B <- 100
#' TD <- 15
#' tau <- 8
#' y <- monoexponential(x, A, B, TD, tau) + rnorm(length(x), 0, 3)
#' data <- data.frame(x, y)
#' 
#' nls(y ~ SS_monoexp(x, A, B, TD, tau), data = data)
#' 
#' ## TODO ADD PLOT
#'
#' @rdname SS_monoexp
#' @order 1
#' @export
SS_monoexp <- selfStart(
    model = monoexponential,
    initial = monoexp_init,
    parameters = c("A", "B", "TD", "tau"))
