#' Filter a data frame
#'
#' Apply digital filtering/smoothing to numeric vector data within a data frame
#' using either:
#'   1. A cubic smoothing spline.
#'   2. A Butterworth digital filter.
#'   3. A simple moving average.
#'
#' @param sample_rate A numeric value for the sample rate in Hz for
#'   `method = "butterworth"`. Will be taken from metadata or estimated
#'   from `time_channel` if not defined explicitly.
#' @param method A character string indicating how to filter the data (see
#'   *Details*).
#'   \describe{
#'      \item{`"smooth_spline"`}{Fits a cubic smoothing spline.}
#'      \item{`"butterworth"`}{Uses a centred Butterworth digital filter.
#'      `type` should be defined (see *Details*).}
#'      \item{`"moving_average"`}{Uses a centred moving average filter.}
#'   }
#' @param spar A numeric value defining the smoothing parameter for
#'   `method = "smooth_spline"`.
#' @param type A character string indicating the digital filter type for
#'   `method = "butterworth"` (see *Details*).
#'   \describe{
#'      \item{`"low"`}{For a *low-pass* filter (the *default*).}
#'      \item{`"high"`}{For a *high-pass* filter.}
#'      \item{`"stop"`}{For a *stop-band* (band-reject) filter.}
#'      \item{`"pass"`}{For a *pass-band* filter.}
#'   }
#' @param n An integer defining the filter order for `method = "butterworth"`
#'   (*default* `n = 1`).
#' @param W A one- or two-element numeric vector defining the filter cutoff
#'   frequency(ies) for `method = "butterworth"`, as a fraction of the
#'   Nyquist frequency (see *Details*).
#' @param fc A one- or two-element numeric vector defining the filter cutoff
#'   frequency(ies) for `method = "butterworth"`, in Hz (see *Details*).
#' @param width An integer defining the local window in number of samples
#'   around `idx` in which to perform the operation for
#'   `method = "moving_average"`. Between
#'   `[idx - floor(width/2), idx + floor(width/2)]`.
#' @param span A numeric value defining the local window timespan around `idx`
#'   in which to perform the operation for `method = "moving_average"`.
#'   In units of `time_channel` or `t`, between `[t - span/2, t + span/2]`.
#'
#' @param na.rm A logical indicating whether missing values should be ignored
#'   (`TRUE`) before the filter is applied. Otherwise `FALSE` (the *default*)
#'   will throw an error (see *Details*).
#' @param ... Additional arguments.
#' @inheritParams validate_mnirs
#'
#' @details
#' \describe{
#'   \item{`method = "smooth_spline"`}{Applies a non-parametric cubic
#'   smoothing spline from [stats::smooth.spline()]. Smoothing is defined
#'   by the parameter `spar`, which can be left as `NULL` and automatically
#'   determined via penalised log liklihood. This usually works well for
#'   smoothing responses occurring on the order of minutes or longer. `spar`
#'   can be defined explicitly, typically (but not necessarily) in the range
#'   `spar = [0, 1]`.}
#'
#'   \item{`method = "butterworth"`}{Applies a centred (two-pass
#'   symmetrical) Butterworth digital filter from [signal::butter()] and
#'   [signal::filtfilt()].
#'
#'   Filter `type` defines how the desired signal frequencies are either
#'   passed or rejected from the output signal. *Low-pass* and *high-pass*
#'   filters allow only frequencies *lower* or *higher* than the cutoff
#'   frequency `W` to be passed through as the output signal, respectively.
#'   *Stop-band* defines a critical range of frequencies which are rejected
#'   from the output signal. *Pass-band* defines a critical range of
#'   frequencies which are passed through as the output signal.
#'
#'   The filter order (number of passes) is defined by `n`, typically in
#'   the range `n = [1, 10]`. Higher filter order tends to capture more
#'   rapid changes in amplitude, but also causes more distortion around
#'   those change points in the signal. General advice is to use the
#'   lowest filter order which sufficiently captures the desired rapid
#'   responses in the data.
#'
#'   The critical (cutoff) frequency is defined by `W`, a numeric value for
#'   *low-pass* and *high-pass* filters, or a two-element vector
#'   `c(low, high)` defining the lower and upper bands for *stop-band* and
#'   *pass-band* filters. `W` represents the desired fractional cutoff
#'   frequency in the range `W = [0, 1]`, where `1` is the Nyquist
#'   frequency, i.e., half the sample rate of the data in Hz.
#'
#'   Alternatively, the cutoff frequency can be defined by `fc` and
#'   `sample_rate` together. `fc` represents the desired cutoff frequency
#'   in Hz, and `sample_rate` is the sample rate of the recorded data in
#'   Hz. `W = fc / (sample_rate / 2)`.
#'
#'   Only One of either `W` or `fc` should be defined. If both are defined,
#'   `W` will be preferred over `fc`.}
#'
#'   \item{`method = "moving_average"`}{Applies a centred (symmetrical)
#'   moving average filter in a local window, defined by either `width`
#'   as the number of samples around `idx` between `[idx - floor(width/2),`
#'   `idx + floor(width/2)]`. Or by `span` as the timespan in units of
#'   `time_channel` between `[t - span/2, t + span/2]`. A partial moving
#'   average will be calculated at the edges of the data.}
#' }
#'
#' Missing values (`NA`) in `nirs_channels` will cause an error for
#'   `method = "smooth_spline"` or `"butterworth"`, unless `na.rm = TRUE`.
#'   Then `NA`s will be preserved and passed through in the returned data.
#'
#' @returns
#' A [tibble][tibble::tibble-package] of class *"mnirs"* with metadata
#'   available with `attributes()`.
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(ggplot2)
#'
#' ## read example data
#' data <- read_mnirs(
#'     file_path = example_mnirs("moxy_ramp"),
#'     nirs_channels = c(smo2 = "SmO2 Live"),
#'     time_channel = c(time = "hh:mm:ss"),
#'     inform = FALSE
#' ) |>
#'     replace_mnirs(
#'         invalid_values = c(0, 100),
#'         outlier_cutoff = 3,
#'         width = 10,
#'         inform = FALSE
#'     )
#'
#' data_filtered <- filter_mnirs(
#'     data,
#'     # nirs_channel = NULL,  ## taken from metadata
#'     # time_channel = NULL,
#'     # sample_rate = NULL,
#'     method = "butterworth", ## Butterworth digital filter is a common choice
#'     type = "low",           ## specify a low-pass filter
#'     n = 2,                  ## filter order number
#'     W = 0.02,               ## filter fractional critical frequency
#'     inform = FALSE
#' )
#'
#' ## add the non-filtered data back to the plot to compare
#' plot(data_filtered, label_time = TRUE) +
#'     geom_line(
#'         data = data,
#'         aes(y = smo2, colour = "smo2"), alpha = 0.4
#'     )
#'
#' @rdname filter_mnirs
#' @export
filter_mnirs <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    sample_rate = NULL,
    method = c("smooth_spline", "butterworth", "moving_average"),
    spar = NULL,
    type = c("low", "high", "stop", "pass"),
    n = 1,
    W = NULL,
    fc = NULL,
    width = NULL,
    span = NULL,
    na.rm = FALSE,
    inform = TRUE,
    ...
) {
    ## validation ====================================
    validate_mnirs_data(data)
    method <- match.arg(method)
    if (missing(inform)) {
        inform <- getOption("mnirs.inform", default = TRUE)
    }

    ## create object with class for method dispatch
    data <- structure(
        list(data = data),
        class = c(method, "mnirs.filtered")
    )

    UseMethod("filter_mnirs", data)
}


#' @rdname filter_mnirs
#' @usage NULL
#' @export
filter_mnirs.smooth_spline <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    sample_rate = NULL,
    method = c("smooth_spline", "butterworth", "moving_average"),
    spar = NULL,
    type = c("low", "high", "stop", "pass"),
    n = 1,
    W = NULL,
    fc = NULL,
    width = NULL,
    span = NULL,
    na.rm = FALSE,
    inform = TRUE,
    ...
) {
    ## validation ==========================================
    rlang::check_installed("stats", reason = "to use stats::smooth.spline()")
    metadata <- attributes(data)
    ## inform = FALSE because grouping irrelevant
    nirs_channels <- validate_nirs_channels(data, nirs_channels, inform = FALSE)
    time_channel <- validate_time_channel(data, time_channel)
    validate_numeric(
        spar, 1, c(0, Inf), FALSE, msg = "one-element positive"
    )

    ## processing ==========================================
    time_vec <- round(data[[time_channel]], 6)

    if (anyDuplicated(time_vec)) {
        cli_abort(c(
            "{.arg time_channel} has non-sequential or repeating values \\
            causing {.fn stats::smooth.spline} to fail.",
            "i" = "Use {.fn mnirs::resample_mnirs} to fix samples."
        ))
    }

    data[nirs_channels] <- sapply(nirs_channels, \(.x) {
        x <- data[[.x]]
        ## handle NAs
        handle_na <- na.rm && anyNA(x)
        if (handle_na) {
            na_info <- preserve_na(x)
            x <- na_info$x_valid
            time_vec <- time_vec[!na_info$na_idx]
        } else if (anyNA(x)) {
            cli_abort(
                "{.arg x} contains internal `NA`s."
            )
        }

        spline_model <- stats::smooth.spline(x = time_vec, y = x, spar = spar)

        if (is.null(spar) && inform) {
            cli_bullets(c(
                "i" = "{.arg nirs_channel} = {.val {.x}}: \\
                `smooth.spline(spar = {.val {round(spline_model$spar, 3)}})`"
            ))
        }

        if (handle_na) {
            restore_na(spline_model$y, na_info)
        } else {
            spline_model$y
        }
    }, simplify = FALSE, USE.NAMES = TRUE)

    ## Metadata =================================
    metadata$nirs_channels <- unique(c(metadata$nirs_channels, nirs_channels))

    return(create_mnirs_data(data, metadata))
}


#' @rdname filter_mnirs
#' @usage NULL
#' @export
filter_mnirs.butterworth <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    sample_rate = NULL,
    method = c("smooth_spline", "butterworth", "moving_average"),
    spar = NULL,
    type = c("low", "high", "stop", "pass"),
    n = 1,
    W = NULL,
    fc = NULL,
    width = NULL,
    span = NULL,
    na.rm = FALSE,
    inform = TRUE,
    ...
) {
    ## validation ==========================================
    metadata <- attributes(data)
    ## inform = FALSE because grouping irrelevant
    nirs_channels <- validate_nirs_channels(data, nirs_channels, inform = FALSE)
    time_channel <- validate_time_channel(data, time_channel)

    sample_rate <- validate_sample_rate(
        data, time_channel, sample_rate, inform
    )
    type <- match.arg(type)
    edges <- list(...)$edges %||% "rev" ## default filter_butter(edges)

    if (is.null(c(W, fc))) {
        cli_abort("{.arg W} or {.arg fc} must be defined.")
    }

    fc_n <- if (type %in% c("low", "high")) 1 else 2
    ## n & W are validated in filter_butter
    validate_numeric(
        fc, fc_n, c(0, Inf), inclusive = FALSE,
        msg = paste0(fc_n, "-element positive")
    )

    if (!is.null(W) && !is.null(fc)) {
        fc <- NULL
        if (inform) {
            cli_warn(c(
                "Either {.arg W} or {.arg fc} should be defined, not both.",
                "i" = "Defaulting to {.arg W} = {.val {W}}"
            ))
        }
    }

    if (is.null(W) && !is.null(fc) && !is.null(sample_rate)) {
        W <- fc / (sample_rate * 0.5)
    }

    ## processing ==========================================
    data[nirs_channels] <- lapply(data[nirs_channels], \(.x) {
        filter_butter(.x, n, W, type, edges, na.rm)
    })

    ## Metadata =================================
    metadata$nirs_channels <- unique(c(metadata$nirs_channels, nirs_channels))
    metadata$time_channel <- time_channel
    metadata$sample_rate <- sample_rate

    return(create_mnirs_data(data, metadata))
}


#' @rdname filter_mnirs
#' @usage NULL
#' @export
filter_mnirs.moving_average <- function(
    data,
    nirs_channels = NULL,
    time_channel = NULL,
    sample_rate = NULL,
    method = c("smooth_spline", "butterworth", "moving_average"),
    spar = NULL,
    type = c("low", "high", "stop", "pass"),
    n = 1,
    W = NULL,
    fc = NULL,
    width = NULL,
    span = NULL,
    na.rm = FALSE,
    inform = TRUE,
    ...
) {
    ## validation ==========================================
    metadata <- attributes(data)
    ## inform = FALSE because grouping irrelevant
    nirs_channels <- validate_nirs_channels(data, nirs_channels, inform = FALSE)
    time_channel <- validate_time_channel(data, time_channel)

    ## processing ==========================================
    time_vec <- round(data[[time_channel]], 6)

    data[nirs_channels] <- lapply(data[nirs_channels], \(.x) {
        filter_moving_average(.x, time_vec, width, span, inform)
    })

    ## Metadata =================================
    metadata$nirs_channels <- unique(c(metadata$nirs_channels, nirs_channels))
    metadata$time_channel <- time_channel

    return(create_mnirs_data(data, metadata))
}


#' Apply a moving average filter
#'
#' Apply a simple moving average smoothing filter to vector data
#'
#' @param x A numeric vector.
#' @param t An *optional* numeric vector of time or sample number.
#' @inheritParams shift_mnirs
#' @inheritParams filter_mnirs
#'
#' @details
#' Applies a centred (symmetrical) moving average filter in a local window
#'   defined by either `width` as the number of samples around `idx` between
#'   `[idx - floor(width/2),` `idx + floor(width/2)]`. Or by `span` as the
#'   timespan in units of `time_channel` between `[t - span/2, t + span/2]`.
#'
#' If there are no valid values within the calculation window, will return `NA`.
#'   A partial moving average will be calculated at the edges of the data.
#'
#' @returns A numeric vector the same length as `x`.
#'
#' @seealso [zoo::rollmean()]
#'
#' @examples
#' ## basic moving average with sample width
#' x <- c(1, 3, 2, 5, 4, 6, 5, 7)
#' filter_moving_average(x, width = 3)
#'
#' ## with explicit time vector
#' t <- c(0, 1, 2, 3, 4, 5, 6, 7)
#' filter_moving_average(x, t, width = 2)
#'
#' ## using timespan instead of sample width
#' filter_moving_average(x, span = 2)
#'
#' @export
filter_moving_average <- function(
    x,
    t = seq_along(x),
    width = NULL,
    span = NULL,
    inform = TRUE
) {
    ## validation ===========================================
    validate_numeric(x)
    validate_numeric(t)
    if (length(x) != length(t)) {
        cli_abort(
            "{.arg x} and {.arg t} must be {.cls numeric} vectors of the \\
            same length."
        )
    }
    if (missing(inform)) {
        inform <- getOption("mnirs.inform", default = TRUE)
    }

    ## processing ==============================================
    window_idx <- compute_local_windows(
        t, width = width, span = span, inform = inform
    )
    y <- compute_local_fun(x, window_idx, mean)
    ## explicit overwrite NaN to NA
    y[!is.finite(y)] <- NA_real_
    return(y)
}


#' Apply a Butterworth digital filter
#'
#' Apply a Butterworth digital filter to vector data with [signal::butter()]
#' and [signal::filtfilt()] which handles 'edges' better at the start and end
#' of the data.
#'
#' @param x A numeric vector.
#' @param edges A character string indicating how to pad `x` for edge detection.
#'   \describe{
#'      \item{`"rev"`}{(*the default*) Will pad `x` with the preceding 5% data
#'      in reverse sequence.}
#'      \item{`"rep1"`}{Will pad `x` by repeating the last preceding value.}
#'      \item{`"none"`}{Will return the unpadded [signal::filtfilt()] output.}
#'   }
#' @inheritParams filter_mnirs
#'
#' @details
#' Applies a centred (two-pass symmetrical) Butterworth digital filter from
#'   [signal::butter()] and [signal::filtfilt()].
#'
#' Filter `type` defines how the desired signal frequencies are either
#'   passed or rejected from the output signal. *Low-pass* and *high-pass*
#'   filters allow only frequencies *lower* or *higher* than the cutoff
#'   frequency `W` to be passed through as the output signal, respectively.
#'   *Stop-band* defines a critical range of frequencies which are rejected
#'   from the output signal. *Pass-band* defines a critical range of
#'   frequencies which are passed through as the output signal.
#'
#' The filter order (number of passes) is defined by `n`, typically in
#'   the range `n = [1, 10]`. Higher filter order tends to capture more
#'   rapid changes in amplitude, but also causes more distortion around
#'   those change points in the signal. General advice is to use the
#'   lowest filter order which sufficiently captures the desired rapid
#'   responses in the data.
#'
#' The critical (cutoff) frequency is defined by `W`, a numeric value for
#'   *low-pass* and *high-pass* filters, or a two-element vector
#'   `c(low, high)` defining the lower and upper bands for *stop-band* and
#'   *pass-band* filters. `W` represents the desired fractional cutoff
#'   frequency in the range `W = [0, 1]`, where `1` is the Nyquist
#'   frequency, i.e., half the sample rate of the data in Hz.
#'
#' Missing values (`NA`) in `x` will cause an error unless `na.rm = TRUE`.
#'   Then `NA`s will be preserved and passed through in the returned vector.
#'
#' @returns A numeric vector the same length as `x`.
#'
#' @seealso [signal::filtfilt()] [signal::butter()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(ggplot2)
#'
#' set.seed(13)
#' sin <- sin(2 * pi * 1:150 / 50) * 20 + 40
#' noise <- rnorm(150, mean = 0, sd = 6)
#' noisy_sin <- sin + noise
#' filt_without_edge <- filter_butter(x = noisy_sin, n = 2, W = 0.1, edges = "none")
#' filt_with_edge <- filter_butter(x = noisy_sin, n = 2, W = 0.1, edges = "rep1")
#'
#' ggplot(data.frame(), aes(x = seq_along(noise))) +
#'     theme_mnirs() +
#'     scale_colour_mnirs(name = NULL) +
#'     geom_line(aes(y = noisy_sin)) +
#'     geom_line(aes(y = filt_without_edge, colour = "filt_without_edge")) +
#'     geom_line(aes(y = filt_with_edge, colour = "filt_with_edge"))
#'
#' @export
filter_butter <- function(
    x,
    n = 1,
    W,
    type = c("low", "high", "stop", "pass"),
    edges = c("rev", "rep1", "none"),
    na.rm = FALSE
) {
    ## validation ============================================
    rlang::check_installed("signal", "to use Butterworth digital filter")
    validate_numeric(x)
    validate_numeric(
        n, 1, c(1, Inf), integer = TRUE, msg = "one-element positive"
    )
    type <- match.arg(type)
    if (type %in% c("low", "high")) {
        validate_numeric(
            W, 1, c(0, 1), inclusive = FALSE,
            msg = "one-element positive fractional"
        )
    } else if (type %in% c("stop", "pass")) {
        validate_numeric(
            W, 2, c(0, 1), inclusive = FALSE,
            msg = "two-element positive fractional"
        )
    }
    edges <- match.arg(edges)

    ## processing ==============================================
    ## handle NAs
    handle_na <- na.rm && any(is.na(x))
    if (handle_na) {
        na_info <- preserve_na(x)
        x <- na_info$x_valid
    } else if (any(is.na(x))) {
        cli_abort(
            "{.arg x} contains internal `NA`s."
        )
    }

    if (edges == "none") {
        y <- signal::filtfilt(signal::butter(n, W, type), x = x)
    } else {
        x_n <- length(x)
        pad <- max(1, x_n %/% 20) ## 5% padded length

        padded <- switch(
            edges,
            "rev" = c(x[pad:1], x, x[x_n:(x_n - pad + 1)]),
            "rep1" = c(rep(x[1], pad), x, rep(x[x_n], pad))
        )

        y_padded <- signal::filtfilt(signal::butter(n, W, type), x = padded)
        y <- y_padded[(pad + 1):(pad + x_n)]
    }

    ## return NAs to original positions in y
    if (handle_na) {
        return(restore_na(y, na_info))
    } else {
        return(y)
    }
}
