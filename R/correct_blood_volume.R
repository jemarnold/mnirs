#' @export
correct_blood_volume <- function(
    data,
    oxy_channel = NULL,
    deoxy_channel = NULL,
    total_channel = NULL,
    saturation_channel = NULL,
    verbose = TRUE
) {
    ## validation =====================================================
    ## require exactly two of the four channel names to be supplied
    supplied <- !vapply(
        list(oxy_channel, deoxy_channel, total_channel, saturation_channel),
        is.null,
        logical(1)
    )
    if (sum(supplied) < 2L) {
        cli_abort(c(
            "x" = "At least two of {.arg oxy_channel}, {.arg deoxy_channel}, \\
            {.arg total_channel}, {.arg saturation_channel} are required."
        ))
    }
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }

    ## validate supplied names exist in data
    channels <- c(oxy_channel, deoxy_channel, total_channel, saturation_channel)
    missing_cols <- setdiff(channels, names(data))
    if (length(missing_cols) > 0L) {
        cli_abort(c(
            "x" = "Column{?s} {.val {missing_cols}} not found in {.arg data}.",
            "i" = "Channel names are case-sensitive and must match exactly."
        ))
    }

    ## processing ====================================================
    ## pull whichever channels were supplied
    oxy <- if (!is.null(oxy_channel)) data[[oxy_channel]]
    deoxy <- if (!is.null(deoxy_channel)) data[[deoxy_channel]]
    total <- if (!is.null(total_channel)) data[[total_channel]]
    sat <- if (!is.null(saturation_channel)) data[[saturation_channel]]

    ## rescale percent [0, 100] to fractional [0, 1] for derivations
    if (!is.null(sat) && max(sat, na.rm = TRUE) > 1) {
        sat <- sat / 100
        if (verbose) {
            cli_inform(c(
                "i" = "{.arg saturation_channel} detected as percent \\
                and converted to fractional."
            ))
        }
    }

    ## derive oxy and total from whichever two channels were supplied;
    ## prefer user-supplied values over derived ones (saturation = oxy / total)
    total <- total %||% (oxy + deoxy) %||% (oxy / sat) %||% (deoxy / (1 - sat))
    oxy <- oxy %||% (total - deoxy) %||% (total * sat)
    deoxy <- deoxy %||% (total - oxy)

    ## blood volume correction factor beta, requires all positive values
    beta <- sat %||% oxy / total

    ## corrected signals telescope to a cumulative sum of adjusted diffs
    diff_total <- diff(total)
    oxy_c <- cumsum(c(0, diff(oxy) - beta[-1] * diff_total))
    deoxy_c <- cumsum(c(0, diff(deoxy) - (1 - beta[-1]) * diff_total))

    ## thb corrected for changes in blood volume becomes equal to zero
    if (!is.null(oxy_channel)) {
        data[[oxy_channel]] <- oxy_c
    }
    if (!is.null(deoxy_channel)) {
        data[[deoxy_channel]] <- deoxy_c
    }
    if (!is.null(total_channel)) {
        data[[total_channel]] <- double(length(oxy_c)) ## avoid float error
    }

    return(data)
}
