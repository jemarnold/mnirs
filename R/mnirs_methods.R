#' Methods for mnirs objects
#'
#' Generic methods for objects of class `"mnirs"`.
#'
#' @param x Object of class `"mnirs"`.
#' @param ... Additional arguments passed to [print()] methods of `x` or
#'   each list element, e.g. `n` rows for [tibbles][tibble::tibble-package].
#'
#' @returns
#' \item{`print`}{Returns `x` without class attributes.}
#'
#' @examples
#' x <- read_mnirs(
#'     example_mnirs("train.red"),
#'     nirs_channels = c(smo2 = "SmO2"),
#'     time_channel = c(time = "Timestamp (seconds passed)"),
#'     verbose = FALSE
#' ) |>
#'     resample_mnirs(method = "linear", verbose = FALSE) |>
#'     extract_intervals(
#'         start = by_time(2452, 3168),
#'         span = c(-60, 120),
#'         verbose = FALSE
#'     )
#'
#' print(x)
#'
#' @export
print.mnirs <- function(x, ...) {
    class(x) <- setdiff(class(x), "mnirs")
    if (is.data.frame(x)) {
        print(x, ...)
        return(invisible(x))
    }
    ## list of data frames: print each element so `...` (e.g. `n`) reach
    ## element print methods; mimic base `$name` / `[[i]]` headers
    nms <- names(x) %||% rep("", length(x))
    invisible(Map(\(.x, .nm, .i) {
        cat(if (nzchar(.nm)) paste0("$", .nm) else paste0("[[", .i, "]]"), "\n")
        print(.x, ...)
        cat("\n")
    }, x, nms, seq_along(x)))
    return(invisible(x))
}


#' Methods for mnirs_kinetics objects
#'
#' Generic methods for objects returned from [analyse_kinetics()].
#'
#' @param x Object of class `"mnirs_kinetics"` returned from
#'   [analyse_kinetics()].
#' @param ... Additional arguments. For `c`; *"mnirs_kinetics"* objects to
#'   combine, e.g. separate [analyse_kinetics()] calls fitting a different
#'   method to each interval.
#'
#' @returns
#' \item{`print`}{Returns a model summary}
#' \item{`c`}{Returns a single *"mnirs_kinetics"* object with the elements of
#'     each result bound in order, as if returned from one
#'     [analyse_kinetics()] call. `method` holds each unique method; where
#'     methods differ, `coefficients$model` names the method fitted to each
#'     row. Interval names must be unique across results.}
#'
#' @examples
#' intervals <- read_mnirs(
#'     example_mnirs("train.red"),
#'     nirs_channels = c(smo2 = "SmO2"),
#'     time_channel = c(time = "Timestamp (seconds passed)"),
#'     zero_time = TRUE,
#'     verbose = FALSE
#' ) |>
#'     resample_mnirs(method = "linear", verbose = FALSE) |>
#'     extract_intervals(
#'         group_intervals = "distinct", ## return each interval distinctly
#'         start = by_time(368, 1084),
#'         span = c(-20, 90),
#'         zero_time = TRUE,
#'         verbose = FALSE
#'     )
#'
#' result <- analyse_kinetics(
#'     intervals,
#'     method = "peak_slope",
#'     span = 10,
#'     verbose = FALSE
#' )
#'
#' print(result)
#'
#' ## combine separate calls, e.g. a different method per interval
#' c(
#'     analyse_kinetics(
#'         intervals[1],
#'         method = "peak_slope",
#'         span = 10,
#'         verbose = FALSE
#'     ),
#'     analyse_kinetics(intervals[2], method = "monoexponential", verbose = FALSE)
#' )
#'
#' @export
print.mnirs_kinetics <- function(x, ...) {
    coefs <- x$coefficients
    ## remove columns unnecessary for display
    drop_start_time <- isTRUE(all(coefs$start_time == 0))
    drop_cols <- c(
        "HRT",
        "drift_fraction",
        "idx",
        if (drop_start_time) "start_time",
        grep("fitted$", names(coefs), value = TRUE)
    )
    coefs <- coefs[, !names(coefs) %in% drop_cols, drop = FALSE]
    nrows <- nrow(coefs)

    ## prep numeric values for display; ignore all na cols
    numeric_cols <- vapply(coefs, \(.x) {
        is.numeric(.x) && !all(is.na(.x))
    }, logical(1))
    ## prep only non-NA values
    coefs[, numeric_cols] <- lapply(coefs[, numeric_cols, drop = FALSE], \(.x) {
        .x[!is.na(.x)] <- signif_trailing(.x[!is.na(.x)], 4L, "signif")
        .x[is.na(.x)] <- "NA"
        .x
    })
    ## display titles
    method_labels <- c(
        response_time = "Fractional Response Time",
        peak_slope = "Peak Linear Response Rate",
        monoexponential = "Monoexponential One-Phase Kinetics",
        exponential_drift = "Exponential-Linear Drift Two-Phase Kinetics",
        biexponential = "Biexponential Two-Phase Kinetics",
        sigmoidal = "Sigmoidal Inflection Kinetics",
        sigmoidal_drift = "Sigmoidal-Linear Drift Two-Phase Kinetics"
    )

    cat("\n")
    ## combined results list each method fitted
    cat(paste(method_labels[x$method], collapse = " / "))
    cat("\n")
    cat("    Model Coefficients:")
    cat("\n")
    if (nrows <= 10) {
        print(coefs)
    } else {
        start_ids <- 1:5
        end_idx <- (nrows - 4):nrows
        ## get first and last rows
        display <- rbind(coefs[start_ids, ], coefs[end_idx, ])

        ## format data frame with row numbers
        output <- utils::capture.output(print(display, row.names = FALSE))

        ## insert row numbers and spacer
        header <- output[1L]
        lines <- output[-1L]

        ## add row numbers to data lines
        top_lines <- paste0(sprintf("%6d:", start_ids), " ", lines[1:5])
        bottom_lines <- paste0(sprintf("%6d:", end_idx), " ", lines[6:10])

        ## create spacer with column count
        spacer <- sprintf("    --- %d rows omitted", nrows - 10)

        ## print output
        cat(sprintf("        %s", header), "\n", sep = "")
        cat(top_lines, sep = "\n")
        cat(spacer, "\n")
        cat(bottom_lines, sep = "\n")
    }
    cat("\n\n")

    return(invisible(x))
}


#' @rdname print.mnirs_kinetics
#' @export
c.mnirs_kinetics <- function(...) {
    results <- list(...)
    if (!all(vapply(results, inherits, logical(1), "mnirs_kinetics"))) {
        cli_abort(c(
            "x" = "All objects must be of class {.cls mnirs_kinetics}.",
            "i" = "Check the objects returned from {.fn analyse_kinetics}."
        ))
    }
    ## transpose to one list per element across results
    x <- do.call(Map, c(list, results))
    ## duplicate interval names would collide in `data`, `model`, and plot
    ## facets
    data <- do.call(c, x$data)
    dupes <- unique(names(data)[duplicated(names(data))])
    if (length(dupes) > 0L) {
        cli_abort(c(
            "x" = "Interval names must be unique across results.",
            "i" = "Duplicated: {.val {dupes}}."
        ))
    }

    ## mixed methods label each row by its own model, so print & plot
    ## annotate rows by the method that fitted them
    methods <- unique(unlist(x$method))
    if (length(methods) > 1L) {
        x$coefficients <- Map(\(.cf, .m) {
            .cf$model <- .cf$model %||% .m
            .cf
        }, x$coefficients, x$method)
    }
    coefs <- bind_union(x$coefficients)
    lead <- c("interval", "nirs_channels", "start_time", "model")
    coefs <- coefs[union(intersect(lead, names(coefs)), names(coefs))]

    return(structure(
        list(
            method = methods,
            model = do.call(c, x$model),
            coefficients = coefs,
            data = structure(data, class = c("mnirs", "list")),
            interval_times = bind_union(x$interval_times),
            diagnostics = bind_union(x$diagnostics),
            channel_args = bind_union(x$channel_args),
            warnings = bind_union(x$warnings) %||% kinetics_warnings_df(),
            call = sys.call()
        ),
        class = "mnirs_kinetics"
    ))
}
