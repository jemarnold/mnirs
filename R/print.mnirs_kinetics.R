#' Methods for mnirs_kinetics objects
#'
#' Generic methods for objects returned from [analyse_kinetics()].
#'
#' @param x Object of class `mnirs_kinetics` returned from [analyse_kinetics()].
#' @param ... Additional arguments.
#'
#' @returns Return:
#'      \item{`print`}{Returns a model summary}
#'
#' @export
print.mnirs_kinetics <- function(x, ...) {
    results <- as.data.frame(x$results)
    nrows <- nrow(results)

    numeric_cols <- vapply(results, is.numeric, logical(1))
    results[, numeric_cols] <- lapply(
        results[, numeric_cols, drop = FALSE],
        signif_trailing,
        digits = 4L,
        format = "max_signif"
    )


    # data_list <- x$data

    if (x$method == "peak_slope") {

        cat("\n")
        cat("Peak Linear Regression Slope")
        cat("\n\n")
        cat("    Model Coefficients:")
        cat("\n")
        if (nrows <= 10) {
            print(results)
        } else {
            top_idx <- 1:5
            bottom_idx <- (nrows - 4):nrows
            ## get first and last rows
            display <- rbind(results[top_idx, ], results[bottom_idx, ])

            ## format data frame with row numbers
            output <- utils::capture.output(print(display, row.names = FALSE))

            ## insert row numbers and spacer
            header <- output[1L]
            data_lines <- output[-1L]

            ## add row numbers to data lines
            top_lines <- paste0(
                sprintf("%6d:", top_idx),
                " ",
                data_lines[1:5]
            )
            bottom_lines <- paste0(
                sprintf("%6d:", bottom_idx),
                " ",
                data_lines[6:10]
            )

            ## create spacer with column count
            spacer <- sprintf("    --- %d rows omitted", nrows - 10)
            
            ## print output
            cat(sprintf("        %s", header), "\n", sep = "")
            cat(top_lines, sep = "\n")
            cat(spacer, "\n")
            cat(bottom_lines, sep = "\n")


            ## ! add instructions how to retrieve returned objec
        }
        cat("\n\n")

    }
}

