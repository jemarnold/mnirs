#' Re-scale data range
#'
#' Expand or reduce the range (min and max values) of data channels to a new
#' amplitude/dynamic range, e.g. re-scale the range of NIRS data to `c(0, 100)`.
#'
#' @param nirs_channels A `list()` of character vectors indicating the column
#'   names for data channels to be re-scaled (see *Details*).
#'   \describe{
#'      \item{`list("A", "B", "C")`}{Will re-scale each channel independently,
#'      losing the relative scaling between channels.}
#'      \item{`list(c("A", "B", "C"))`}{Will re-scale all channels together,
#'      preserving the relative scaling between channels.}
#'      \item{`list(c("A", "B"), c("C", "D"))`}{Will re-scale channels `A` and
#'      `B` in one group, and channels `C` and `D` in another group,
#'      preserving relative scaling within, but not between groups.}
#'   }
#'   Must match column names in data exactly. Will be taken from metadata if
#'   not defined explicitly.
#' @param range A numeric vector in the form `c(min, max)`, indicating the
#'   range of output values to which data channels will be re-scaled.
#' @inheritParams validate_mnirs
#'
#' @details
#' `nirs_channels = list()` can be used to group data channels to preserve
#'   absolute or relative scaling.
#'
#' - Channels grouped together in a list item will be re-scaled to a common
#'   value, and the relative scaling within that group will be preserved.
#'
#' - Channels grouped in separate list items will be re-scaled independently,
#'   and relative scaling between groups will be lost.
#'
#' - Channels (columns) in `data` not explicitly defined in `nirs_channels`
#'   will be passed through untouched to the output data frame.
#'
#' `nirs_channels` can be retrieved automatically from
#'   `data` of class *"mnirs"* which has been processed with `{mnirs}`,
#'   if not defined explicitly. This will default to returning all
#'   `nirs_channels` grouped together, and should be defined explicitly
#'   for other grouping arrangements.
#'
#' @returns
#' A [tibble][tibble::tibble-package] of class *"mnirs"* with metadata
#'   available with `attributes()`.
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(ggplot2)
#'
#' options(mnirs.verbose = FALSE)
#' 
#' ## read example data
#' data_rescaled <- read_mnirs(
#'     file_path = example_mnirs("moxy_ramp"),
#'     nirs_channels = c(smo2 = "SmO2 Live"),
#'     time_channel = c(time = "hh:mm:ss")
#' ) |>
#'     resample_mnirs() |>
#'     replace_mnirs(
#'         invalid_values = c(0, 100),
#'         outlier_cutoff = 3,
#'         width = 10
#'     ) |>
#'     filter_mnirs(na.rm = TRUE) |>
#'     rescale_mnirs(
#'         range = c(0, 100)   ## rescale to a 0-100% functional exercise range
#'     )
#'
#' plot(data_rescaled, label_time = TRUE) +
#'     geom_hline(yintercept = c(0, 100), linetype = "dotted")
#'
#' @export
rescale_mnirs <- function(
    data,
    nirs_channels = list(NULL),
    range,
    verbose = TRUE
) {
    ## validate =================================
    validate_mnirs_data(data, ncol = 1)
    metadata <- attributes(data)
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }
    nirs_channels <- validate_nirs_channels(data, nirs_channels, verbose)
    validate_numeric(range, 2, msg = "two-element `c(min, max)`")

    ## rescale range ================================
    ## this is actually a fast modify-in-place for loop
    for (group in nirs_channels) {
        group_data <- as.matrix(data[, group, drop = FALSE])
        group_data_range <- range(group_data, na.rm = TRUE)
        range_diff <- diff(group_data_range)

        if (range_diff != 0) {
            # fmt: skip
            data[, group] <- (group_data - group_data_range[1]) /
                range_diff * diff(range) + range[1]
        }
    }

    ## Metadata =================================
    metadata$nirs_channels <- unique(
        c(metadata$nirs_channels, unlist(nirs_channels))
    )

    return(create_mnirs_data(data, metadata))
}
