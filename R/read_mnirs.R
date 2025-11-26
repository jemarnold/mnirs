#' Read *{mnirs}* data from file
#'
#' Read files exported from most commercially available mNIRS devices and
#' return a data frame of class *"mnirs"* with recorded time series data
#' and metadata.
#'
#' @param file_path The file path including extension (either *`".xlsx"`*,
#'   *`".xls"`*, or *`".csv"`*) to import.
#' @param nirs_channels A character vector indicating the mNIRS column names
#'   to import from the file. Must match column names in the data file exactly.
#'   A named character vector can be used to rename columns in the form:
#'   `c(new_name = "original_name")` (see *Details*).
#' @param time_channel An *optional* character string indicating the time or
#'   sample column name to import from the file. Must match column names in the
#'   data file exactly. A named character vector can be used to rename columns
#'   in the form: `c(new_name = "original_name")`. Time will be converted to
#'   *numeric* format (see *Details*).
#' @param event_channel An *optional* character string indicating the event or
#'   lap column name to import from the file. Must match column names in the
#'   data file exactly. A named character vector can be used to rename columns
#'   in the form: `c(new_name = "original_name")` (see *Details*).
#' @param sample_rate An *optional* numeric value for the exported sample rate
#'   in Hz. If not defined explicitly, will be estimated from the data (see
#'   *Details*).
#' @param add_timestamp `<under development>` A logical to add a *"timestamp"*
#'   column with date-time for each sample (class *POSIXct*), if present in the
#'   data file. If no absolute timestamp is detected, will instead return
#'   relative time as *hh:mm:ss*.
#' @param zero_time A logical to re-calculate `time_channel` to start
#'   from zero or `FALSE` keep the original values (the *default*).
#' @param keep_all A logical to include all columns detected from the file
#'   or `FALSE` to only include the explicitly specified data columns
#'   (the *default*).
#' @param inform A logical to display (the *default*) or `FALSE` to silence
#'   warnings and information messages used for troubleshooting.
#'
#' @details
#' Channel names are matched to a single row, representing the header row for
#'   data columns anywhere in the data file, not necessarily the top row of
#'   the file.
#'
#' Channels can be renamed in the format `c(new_name = "original_name")`,
#'   where `*"original_name"*` should exactly match the column names found in
#'   the file.
#'
#' If there are duplicate column names in the file, the channel names will
#'   attempt to match them in the order in which they appear. You may want to
#'   confirm that the correct columns have been assigned to each channel as
#'   intended.
#'
#' `nirs_channels` must be defined explicitly and match column names exactly.
#'   If `time_channel` is left blank, the function will attempt to identify a
#'   time column automatically based on column names or values containing time
#'   (`POSIXct`) data or time-formatted character strings (e.g. *"hh:mm:ss"*).
#'
#' `time_channel` will typically contain time values in seconds. However,
#'   some NIRS devices (for example, *Artinis* devices recorded with *Oxysoft*)
#'   export the sample index (i.e. integer row numbers). If *Oxysoft* export
#'   sample rate is detected in the file metadata, a `"time"` column will be
#'   added converting the sample indices to time values in seconds.
#'
#' When the `time_channel` is provided in date-time format, it will be
#'   converted to numeric values and re-calculated from zero. A timestamp
#'   column can be included with absolute date-time (e.g.
#'   *"yyyy-mm-dd hh:mm:ss"*) if unix timestamps are present in the data file.
#'   Otherwise, relative time (*"hh:mm:ss"*) will be returned.
#'
# #' If `time_channel` is not specified, then a `"time"` column will be added
# #'   from sample indices (row numbers) and converted to time values in seconds
# #'   as long as `sample_rate` is specified. If `sample_rate` is not specified,
# #'   then this `"time"` column will remain as sample indices.
# #'
#' If `time_channel` contains irregular sampling (i.e., non-sequential,
#'   repeated, or unordered values) a warning will be displayed (if
#'   `inform = TRUE`) suggesting that the user confirm the file data manually.
#'
#' `sample_rate` is required for certain `{mnirs}` functions to work properly
#'   and can be carried forward in the data frame metadata. If it is not
#'   defined explicitly, it will be estimated from the differences between
#'   values in the `time_channel`. As above, in certain cases where the
#'   `time_channel` represents sample indices rather than time values,
#'   `sample_rate` will be inaccurately estimated to be 1 Hz. In such cases,
#'   `sample_rate` should be defined explicitly.
#'
#' Columns and rows which contain entirely missing data (`NA`) are omitted.
#'
#' `inform = TRUE` will display warnings and information messages which can be
#'   useful for troubleshooting. Errors causing abort messages will always be
#'   displayed. Messages can be silenced globally with
#'   `options(mnirs.inform = FALSE)`.
#'
#' @returns
#' A [tibble][tibble::tibble-package] of class *"mnirs"* with metadata
#'   available with `attributes()`.
#'
#' @examples
#' ## call an example mNIRS data file
#' file_path <- example_mnirs("moxy_ramp")
#'
#' data_table <- read_mnirs(
#'     file_path,
#'     nirs_channels = c(smo2_right = "SmO2 Live", ## identify and rename channels
#'                       smo2_left = "SmO2 Live(2)"),
#'     time_channel = c(time = "hh:mm:ss"), ## date-time format will be converted to numeric
#'     inform = FALSE                       ## hide warnings & messages
#' )
#'
#' data_table
#'
#' @export
read_mnirs <- function(
    file_path,
    nirs_channels,
    time_channel = NULL,
    event_channel = NULL,
    sample_rate = NULL,
    add_timestamp = FALSE,
    zero_time = FALSE,
    keep_all = FALSE,
    inform = TRUE
) {
    ## global options overrides implicit but not explicit `inform`
    if (missing(inform)) {
        inform <- getOption("mnirs.inform", default = TRUE)
    }

    ## import data_raw from either excel or csv
    data_raw <- read_file(file_path)

    ## detect mNIRS device. Returns NULL if not found
    ## TODO expand detection algorithms for other devices
    nirs_device <- detect_mnirs_device(data_raw)

    ## extract the data_table, and name by header row
    table_list <- read_data_table(
        data_raw,
        nirs_channels,
        time_channel,
        event_channel
    )
    data_table <- table_list$data_table
    file_header <- table_list$file_header

    ## attempt to detect `time_channel` automatically
    time_channel <- detect_time_channel(
        data_table,
        time_channel,
        nirs_device,
        inform
    )

    ## rename from channel names, make duplicates unique, keep columns
    ## return list(data_renamed, nirs_renamed, time_renamed, event_renamed)
    renamed_list <- select_rename_data(
        data_table,
        nirs_channels,
        time_channel,
        event_channel,
        keep_all,
        inform
    )
    nirs_renamed <- renamed_list$nirs_channel
    time_renamed <- renamed_list$time_channel
    event_renamed <- renamed_list$event_channel

    ## prepare data
    data_prepared <- renamed_list$data |>
        ## remove empty (NA) columns and rows
        remove_empty_rows_cols() |>
        ## convert column types
        utils::type.convert(na.strings = c("NA", ""), as.is = TRUE) |>
        ## convert POSIXct to numeric and/or recalc time from zero
        parse_time_channel(time_renamed, add_timestamp, zero_time)

    ## standardise invalid to NA and
    ## round numeric cols to avoid float precision errors
    data_prepared[] <- lapply(data_prepared, \(.x) {
        clean_invalid(.x)
    })

    ## validate and estimate sample rate
    ## will write new "time" column if Oxysoft export rate detected
    ## return list(data_sampled, time_renamed, sample_rate)
    sample_list <- parse_sample_rate(
        data_prepared,
        file_header,
        time_renamed,
        sample_rate,
        nirs_device,
        inform
    )
    data_sampled <- sample_list$data
    time_renamed <- sample_list$time_channel
    sample_rate <- sample_list$sample_rate

    ## print warnings for irregular samples
    detect_irregular_samples(
        data_sampled[[time_renamed]],
        time_renamed,
        inform
    )

    ## assign metadata to attributes(data)
    metadata <- list(
        nirs_device = nirs_device,
        nirs_channels = nirs_renamed,
        time_channel = time_renamed,
        event_channel = event_renamed,
        sample_rate = sample_rate,
        inform = inform
    )

    return(create_mnirs_data(data_sampled, metadata))
}


#' Create an *{mnirs}* data frame with metadata
#'
#' Manually add class *"mnirs"* and metadata to an existing data frame.
#'
#' @param data A data frame with existing metadata (`attributes(data)`).
#' @param ... Additional arguments with metadata to add to the data frame.
#'   - nirs_device
#'   - nirs_channels
#'   - time_channel
#'   - event_channel
#'   - sample_rate
#'
#' @details
#' Typically will only be called internally, but can be used to inject *{mnirs}*
#'   metadata into any data frame.
#'
#' @returns A [tibble][tibble::tibble-package] of class *"mnirs"* with
#'   metadata available with `attributes()`.
#'
#' @examples
#' df <- data.frame(
#'     A = 1:3,
#'     B = seq(10, 30, 10),
#'     C = seq(11, 33, 11)
#' )
#'
#' attributes(df)
#'
#' ## inject metadata
#' nirs_data <- create_mnirs_data(
#'     df,
#'     nirs_channels = c("B", "C"),
#'     time_channel = "A",
#'     sample_rate = 1
#' )
#'
#' attributes(nirs_data)
#'
#' @export
create_mnirs_data <- function(data, ...) {
    ## from https://github.com/fmmattioni/whippr/blob/master/R/tbl.R

    name <- substitute(data)
    if (!is.data.frame(data)) {
        cli_abort("{.arg data} = {.val {name}} should be a data frame.")
    }

    ## overwrite existing attributes and add from incoming metadata
    ## incoming metadata from `...` can be either listed or un-listed
    args <- list(...)
    incoming_metadata <- if (length(args) == 1L && is.list(args[[1L]])) {
        args[[1L]]
    } else {
        args
    }
    metadata <- utils::modifyList(attributes(data), incoming_metadata)

    nirs_data <- tibble::new_tibble(
        data,
        class = "mnirs",
        nirs_device = metadata$nirs_device,
        nirs_channels = metadata$nirs_channels,
        time_channel = metadata$time_channel,
        event_channel = metadata$event_channel,
        sample_rate = metadata$sample_rate,
    )

    tibble::validate_tibble(nirs_data)

    return(nirs_data)
}


#' Get path to *{mnirs}* example files
#'
#' @param file Name of file as character string. If `NULL`, returns a vector
#' of all available file names.
#'
#' @returns
#' A file path character string for selected example files stored in this
#'   package.
#'
#' @examples
#' ## lists all files
#' example_mnirs()
#'
#' ## partial matching will error if matches multiple
#' try(example_mnirs("moxy"))
#'
#' example_mnirs("moxy_ramp")
#'
#' @export
example_mnirs <- function(file = NULL) {
    dir_files <- list.files(
        system.file("extdata", package = "mnirs"),
        pattern = "^[^~]" ## exclude open files
    )

    if (is.null(file)) {
        return(dir_files)
    }

    matches <- grep(file, dir_files, fixed = TRUE, value = TRUE)
    if (length(matches) > 1) {
        cli_abort(c(
            "x" = "Multiple files match {.val {file}}:",
            "i" = "Matching files: {.val {matches}}"
        ))
    }

    file <- match.arg(file, choices = dir_files)
    system.file("extdata", file, package = "mnirs", mustWork = TRUE)
}
