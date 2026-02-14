# analyse_kinetics(
    
#     ...
# ) {
#     ## validation ====================================
#     validate_mnirs_data(data)
#     method <- match.arg(method)
#     if (missing(verbose)) {
#         verbose <- getOption("mnirs.verbose", default = TRUE)
#     }

#     ## create object with class for method dispatch
#     data <- structure(
#         list(data = data),
#         class = c(method, "mnirs.filtered")
#     )

#     UseMethod("filter_mnirs", data)
# }




#' @keywords internal
# analyse_slope <- function(
#     x,
#     t = NULL,
#     data = NULL,
#     x0 = 0,
#     width = NULL,
#     span = NULL,
#     align = c("centre", "left", "right"),
#     direction = c("auto", "positive", "negative"),
#     partial = FALSE,
#     verbose = TRUE
# ) {
#     ## NSE: extract x and t as colnames from data if provided ================
#     if (!is.null(data)) {
#         x_name <- parse_channel_name(enquo(x), data)
#         ## ! I need some name validation
#         x <- data[[x_name]]

#         ## t: parse if provided, else default to seq_along(x)
#         if (!rlang::quo_is_null(enquo(t))) {
#             t_name <- parse_channel_name(enquo(t), data)
#             t <- data[[t_name]]
#         } else {
#             t <- seq_along(x)
#         }

#     } else {
#         if (is.null(t)) t <- seq_along(x)
#     }

#     ## validation ================================================
#     validate_x_t(x, t, invalid = TRUE)
#     validate_numeric(x0, elements = 1L)
#     validate_width_span(width, span, verbose)
#     align <- sub("^center$", "centre", align)
#     align <- match.arg(align)
#     direction <- match.arg(direction)
#     if (missing(verbose)) {
#         verbose <- getOption("mnirs.verbose", default = TRUE)
#     }

#     ## analyse ===================================================
#     result <- peak_slope(
#         x = x,
#         t = t,
#         width = width,
#         span = span,
#         align = align,
#         direction = direction,
#         partial = partial,
#         verbose = verbose
#     )

#     ## structure output ==========================================
#     list(
#         method = "peak_slope",
#         slope = result$slope,
#         intercept = result$intercept,
#         y = result$y,
#         t = result$t,
#         idx = result$idx,
#         fitted = result$fitted,
#         window_idx = result$window_idx,
#         t_relative = result$t - x0,
#         x0 = x0
#     )
# }