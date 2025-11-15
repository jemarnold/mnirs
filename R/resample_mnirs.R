#' Re-sample a Data Frame
#'
#' Up- or down-sample the number of samples in an *"mnirs"* data frame
#' using interpolation.
#'
#' @param resample_rate An *optional* numeric value indicating the desired
#'   output sample rate (in Hz) to re-sample the data frame. The *default*
#'   `resample_rate = sample_rate` will interpolate over missing and repeated
#'   samples within the bounds of the existing data rounded to the nearest
#'   value in Hz.
#' @param resample_time An *optional* numeric value indicating the desired
#'   sample time (in seconds) to re-sample the data frame.
#' @param method A character string indicating how to handle resampling
#'   (see *Details* for more on each method):
#'   \describe{
#'      \item{`"linear"`}{Re-samples and replaces `NA`s via linear
#'      interpolation (the *default*) using [stats::approx()].}
#'      \item{`"locf"`}{(*"Last observation carried forward"*). Re-samples and
#'      replaces `NA`s with the most recent valid non-`NA` value to the left
#'      for trailing samples or to the right for leading samples, using
#'      [stats::approx()].}
#'      \item{`"NA"`}{Re-samples by matching values to their nearest value of
#'      `time_channel`, *without* interpolating across new samples or `NA`s in
#'      the original data frame.}
#'   }
#' @inheritParams validate_mnirs
#'
#' @details
#' This function uses [replace_missing()] (based on [stats::approx()]) to
#'   interpolate across new samples in the re-sampled data range.
#'
#' `time_channel` and `sample_rate` can be retrieved automatically from
#'   `data` of class *"mnirs"* which has been processed with `{mnirs}`,
#'   if not defined explicitly.
#'
#' Otherwise, `sample_rate` will be estimated from the values in `time_channel`.
#'   However, this may return unexpected values, and it is safer to define
#'   `sample_rate` explicitly.
#'
#' The *default* setting `resample_rate = sample_rate` will interpolate over
#'   missing and repeated samples within the bounds of the existing data
#'   rounded to the nearest `sample_rate`.
#'
#' By *default*, `method = "linear"` or `"locf"` will interpolate across `NA`s
#'   in the original data and any new samples between existing values of
#'   `time_channel` (see `?replace_missing`). Whereas `method = "NA"` will
#'   match values of numeric columns from the original samples of `time_channel`
#'   to the new re-sampled samples, without interpolation. Meaning `NA`s in the
#'   original data and any new samples will be returned as `NA`.
#'
#' @returns
#' A [tibble][tibble::tibble-package] of class *"mnirs"* with metadata
#'   available with `attributes()`.
#'
#' @examples
#' ## read example data
#' data <- read_mnirs(
#'     file_path = example_mnirs("moxy_ramp"),
#'     nirs_channels = c(smo2 = "SmO2 Live"),
#'     time_channel = c(time = "hh:mm:ss"),
#'     verbose = FALSE
#' )
#' data
#'
#' data_resampled <- resample_mnirs(
#'     data,
#'     time_channel = NULL,           ## taken from metadata
#'     sample_rate = NULL,
#'     # resample_rate = sample_rate, ## the default will re-sample to sample_rate
#'     method = "linear",             ## default linear interpolation across any new samples
#'     verbose = FALSE                ## will confirm the output sample rate
#' )
#'
#' ## note the altered "time" values 👇
#' data_resampled
#'
#' @export
resample_mnirs <- function(
    data,
    time_channel = NULL,
    sample_rate = NULL,
    resample_rate = sample_rate, ## placeholder indicating default condition
    resample_time = NULL,
    method = c("linear", "locf", "NA"),
    verbose = TRUE
) {
    ## validation ====================================
    validate_mnirs_data(data)
    metadata <- attributes(data)
    time_channel <- validate_time_channel(data, time_channel)
    sample_rate <- validate_sample_rate(
        data, time_channel, sample_rate, verbose
    )
    validate_numeric(
        resample_rate, 1, c(0, Inf), FALSE, msg = "one-element positive"
    )
    validate_numeric(
        resample_time, 1, c(0, Inf), FALSE, msg = "one-element positive"
    )
    ## assign default resample_rate as sample_rate
    if (is.null(c(resample_rate, resample_time))) {
        resample_rate <- sample_rate
    }
    method <- match.arg(method)

    ## convert between `resample_rate` and `resample_time`
    if (!is.null(resample_rate) && is.null(resample_time)) {
        resample_time <- 1 / resample_rate
    } else if (!is.null(resample_time) && is.null(resample_rate)) {
        resample_rate <- 1 / resample_time
    } else if (verbose) {
        resample_time <- 1 / resample_rate
        cli_warn(c(
            "Either {.arg resample_rate} or {.arg resample_time} should be \\
            defined, not both.",
            "i" = "Defaulting to {.arg resample_rate} = {.val {resample_rate}}"
        ))
    }

    ## calculate resampling parameters  ===================================
    time_vec <- round(data[[time_channel]], 6)
    sample_range <- floor(range(time_vec, na.rm = TRUE) * sample_rate) /
        sample_rate
    resampled_times <- seq(sample_range[1], sample_range[2], by = resample_time)
    result <- setNames(tibble::tibble(resampled_times), time_channel)

    ## identify column types
    numeric_cols <- vapply(data, is.numeric, logical(1))
    numeric_cols[time_channel] <- FALSE
    non_numeric_cols <- names(data)[!numeric_cols & names(data) != time_channel]

    ## interpolate numeric columns ==================================
    if (method != "NA" && any(numeric_cols)) {
        result[names(data)[numeric_cols]] <- lapply(data[numeric_cols], \(.x) {
            replace_missing(
                x = .x,
                t = time_vec,
                method = method,
                xout = resampled_times
            )
        })
    } else if (method == "NA" && any(numeric_cols)) {
        tol <- resample_time * 0.5
        result[names(data)[numeric_cols]] <- lapply(data[numeric_cols], \(.x) {
            vapply(resampled_times, \(.t) {
                t_diffs <- round(abs(time_vec - .t), 6)
                if (min(t_diffs, na.rm = TRUE) < tol) {
                    .x[which.min(t_diffs)]
                } else {
                    NA_real_
                }
            }, numeric(1))
        })
    }

    ## vectorized forward fill for non-numeric columns =====================
    if (method == "NA" && length(non_numeric_cols) > 0) {
        ## tolerance-based matching
        tol <- resample_time * 0.5
        idx <- max.col(
            -abs(outer(resampled_times, time_vec, `-`)),
            ties.method = "first"
        )
        match_ok <- abs(resampled_times - time_vec[idx]) < tol
        idx[!match_ok] <- NA
        result[non_numeric_cols] <- lapply(data[non_numeric_cols], \(.x) {
            .x[idx]
        })
    } else if (resample_rate < sample_rate && length(non_numeric_cols) > 0) {
        ## down-sample: first non-NA in each interval
        idx <- findInterval(time_vec, resampled_times, rightmost.closed = FALSE)
        idx[idx == 0] <- 1
        result[non_numeric_cols] <- lapply(data[non_numeric_cols],\(.x) {
            ## split by bin, take first non-NA, unsplit
            unname(unsplit(
                lapply(split(.x, idx), \(.v) .v[!is.na(.v)][1L]),
                unique(idx)
            ))
        })
    } else if (length(non_numeric_cols) > 0) {
        ## forward fill using findInterval
        idx <- findInterval(resampled_times, time_vec, rightmost.closed = FALSE)
        idx[idx == 0] <- 1
        result[non_numeric_cols] <- lapply(data[non_numeric_cols], \(.x) {
            .x[idx]
        })
    }

    ## Metadata =================================
    metadata$time_channel <- time_channel
    metadata$sample_rate <- resample_rate
    metadata$verbose <- verbose

    if (verbose) {
        cli_bullets(c(
            "i" = "Output is resampled at {.val {resample_rate}} Hz."
        ))
    }

    ## column order same as input
    result <- result[names(data)]
    return(create_mnirs_data(result, metadata))
}
