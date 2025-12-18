#' Calculate linear slope
#'
#' Calculates the linear regression slope of a numeric vector.
#'
#' @inheritParams replace_invalid
#'
#' @details
#' Uses the closed-form ordinary least squares formula when `t` is sequential 
#'   integers. The denominator simplifies to `n(n²-1)/12`
#'
#' @returns A numeric slope of `x/t`.
#'
#' @examples
#' y <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
#' slope(y)
#'
#' y <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, NA)
#' slope(y)
#'
#' @export
slope <- function(x, t = seq_along(x), na.rm = FALSE) {
    validate_x_t(x, t)
    if (na.rm) {
        complete <- !is.na(x) & !is.na(t)
        x <- x[complete]
        t <- t[complete]
    }

    n <- length(x)
    if (n < 2L) {
        return(NA_real_)
    }
    
    sum_t <- sum(t)
    sum_x <- sum(x)
    sum_tx <- sum(t * x)
    sum_t2 <- sum(t^2)
    denom <- n * sum_t2 - sum_t^2 ## should not cause integer overflow
    if (is.na(denom) || denom == 0) {
        return(NA_real_)
    }
    return((n * sum_tx - sum_t * sum_x) / denom)
}

# roll::roll_median()


# rolling_slope <- function(
#     y,
#     x = seq_along(y),
#     span,
#     align = c("center", "left", "right"),
#     na.rm = FALSE
# ) {
#     ## check for sufficient non-NA observations
#     if (sum(!is.na(y)) < 1) {
#         return(NA_real_)
#     }

#     ## invalidate NULL
#     if (is.null(span)) {
#         span <- NA
#     }
#     validate_numeric(span, 1, c(0, Inf), FALSE, msg1 = "one-element positive")

#     align <- match.arg(align)
#     x <- round(x, 8)
#     y <- round(y, 8) ## avoid floating point precision issues
#     n <- length(y)

#     ## find indices for x values between span (in units of x) based on align
#     if (align == "center") {
#         start_x <- pmax(x[1], x - span / 2)
#         end_x <- pmin(x[n], x + span / 2)
#     } else if (align == "left") {
#         ## align left is FORWARD looking
#         ## current observation is at leftmost position of window
#         ## window starts at current x value, extends span units forward
#         start_x <- x
#         end_x <- pmin(x[n], x + span)
#     } else if (align == "right") {
#         ## align right is BACKWARD looking
#         ## current observation is at rightmost position of window
#         ## window ends at current x value, extends span units backward
#         start_x <- pmax(x[1], x - span)
#         end_x <- x
#     }

#     ## vectorised window detection and slope calculation
#     slopes <- sapply(seq_len(n), \(.x) {
#         ## find indices for x within span
#         window_idx <- which(x >= start_x[.x] & x <= end_x[.x])

#         ## handle local missing data
#         if ((!na.rm & any(is.na(y[window_idx]))) || (na.rm & is.na(y[.x]))) {
#             NA_real_
#         } else if (length(window_idx) < 2) {
#             0
#         } else {
#             ## calculate slope within window
#             slope(y[window_idx], x[window_idx])
#         }
#     })

#     return(slopes)
# }