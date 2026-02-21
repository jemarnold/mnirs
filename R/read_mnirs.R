#' Read *{mnirs}* data from file
#'
#' Import time-series data exported from common muscle NIRS (mNIRS) devices and
#' return a tibble of class `"mnirs"` with the selected signal channels and
#' metadata.
#'
#' @param file_path Path of the data file to import. Supported file extensions 
#'   are `".xlsx"`, `".xls"`, and `".csv"`.
#' 
#' @param nirs_channels A character vector of one or more column names 
#'   containing mNIRS signals to import. Names must match the file header 
#'   exactly.
#'
#'   - If `NULL` (default), `read_mnirs()` attempts to detect the device from
#'     the file contents and use a known `nirs_channel` name.
#'   - A *named* character vector can be used to rename columns on import, in
#'     the form `c(new_name = "original_name")`.
#' 
#' @param time_channel A character scalar giving the name of the time 
#'   (or sample) column to import. The name must match the file header exactly.
#'
#'   - If `NULL` (default), `read_mnirs()` attempts to identify a time-like
#'     column automatically (by known device defaults and/or time-formatted
#'     values).
#'   - A *named* character vector can be used to rename the column on import,
#'     in the form `c(time = "original_name")`.
#' 
#' @param event_channel An *optional* character scalar giving the name of an 
#'   event/marker column to import. Names must match the file header exactly. 
#'   A named character vector can be used to rename the column on import in 
#'   the form `c(event = "original_name")`.
#' 
#' @param sample_rate An *optional* numeric sample rate (Hz). If left blank
#'   (`NULL`), the sample rate is estimated from `time_channel` (see *Details*).
#' 
#' @param add_timestamp A logical. Default is `FALSE`. If `TRUE` and if the 
#'   source data contain an absolute date-time (POSIXct) time value, will add 
#'   a `"timestamp"` column in addition to the specified `time_channel` as a 
#'   numeric time column.
#' 
#' @param zero_time Logical. Default is `FALSE`. If `TRUE`, will re-zero 
#'   `time_channel` to start from `0`.
#' 
#' @param keep_all Logical. Default is `TRUE`. Will keep all columns found in
#'   the file data table. If `FALSE`, will keep only the explicitly specified
#'   `nirs_channels`, `time_channel`, and `event_channel`.
#' 
#' @param verbose Logical. Default is `TRUE`. Will display or silence (if 
#'   `FALSE`) warnings and information messages helpful for troubleshooting. A 
#'   global default can be set via `options(mnirs.verbose = FALSE)`.
#'
#' @details
#'## Header detection
#' `read_mnirs()` searches the file for a header row containing the requested
#' channel names. The header row does not need to be the first row in the file.
#' 
#' - If duplicate column names exist, columns are matched in the order they
#'   appear and renamed with unique strings.
#' - Columns without a header name in the source file will be renamed to 
#'   `col_*`, where `*` is the numeric column number in which they appear in
#'   the file (e.g. `col_6`). This applies to *Artinis Oxysoft* event label
#'   columns, which do not have a column header and must be identified manually.
#' 
#' ## Renaming channels
#' A named character vector can be specified to rename `nirs_channels`, 
#' `time_channel`, and `event_channel`, in the form 
#' `c(new_name = "original_name")`. The `"original_name"` must match the 
#' contents of the file data table header row exactly.
#' 
#' ## Time parsing
#' `time_channel` will be converted to numeric for analysis.
#'
#' - If `time_channel` is a date-time (POSIXct) format, it will be converted 
#'   to numeric and re-based to start from 0, regardless of `zero_time`.
#' - Some devices export a sample index rather than time values. In those 
#'   cases, if an export `sample_rate` is detected in the file metadata (e.g. 
#'   *Artinis Oxysoft* exports), `read_mnirs()` will create or overwrite a 
#'   `"time"` column in seconds derived from the sample index and the detected
#'   `sample_rate`.
#' 
#' ## Sample rate
#' If `sample_rate` is not specified, it is estimated from differences in 
#' `time_channel`. If `time_channel` is actually a sample index, as described 
#' above, this may erroneously be estimated at 1 Hz. `sample_rate` should be
#' specified explicitly in this case.
#' 
#' ## Data cleaning
#' Entirely empty rows and columns are removed. Invalid values (e.g. 
#' `c(NaN, Inf)`) are standardized to `NA`. A warning will be displayed when
#' irregular sampling is detected (e.g. non-monotonic, repeated, or unequal 
#' time values), if `verbose = TRUE`.
#'
#' @returns
#' A [tibble][tibble::tibble-package] of class `"mnirs"`. Metadata are stored
#'   as attributes and can be accessed with `attributes(data)`.
#'
#' @examples
#' ## call an example mNIRS data file
#' file_path <- example_mnirs("moxy_ramp")
#'
#' read_mnirs(
#'     file_path,
#'     nirs_channels = c(                   ## identify and rename channels
#'         smo2_right = "SmO2 Live",
#'         smo2_left = "SmO2 Live(2)"
#'     ),
#'     time_channel = c(time = "hh:mm:ss"), ## date-time format will be converted to numeric
#'     sample_rate = NULL,                  ## sample_rate will be estimated from time_channel
#'     verbose = FALSE                      ## silence warnings & messages
#' )
#'
#' @export
read_mnirs <- function(
    file_path,
    nirs_channels = NULL,
    time_channel = NULL,
    event_channel = NULL,
    sample_rate = NULL,
    add_timestamp = FALSE,
    zero_time = FALSE,
    keep_all = TRUE,
    verbose = TRUE
) {
    ## global options overrides implicit but not explicit `verbose`
    if (missing(verbose)) {
        verbose <- getOption("mnirs.verbose", default = TRUE)
    }

    ## import data_raw from either excel or csv
    df <- read_file(file_path)

    ## detect mNIRS device from raw data. Returns NULL if not found
    nirs_device <- detect_mnirs_device(df, frac_row = 0.333)

    ## resolve channels: use user input if provided, otherwise detect from
    ## known device channel names. Errors if neither available.
    channels <- detect_device_channels(
        nirs_device,
        nirs_channels,
        time_channel,
        verbose
    )
    nirs_channels <- channels$nirs_channels
    time_channel <- channels$time_channel

    ## extract the data_table, and name by header row
    table_list <- read_data_table(
        df,
        nirs_channels,
        time_channel,
        event_channel
    )
    df <- table_list$data_table
    file_header <- table_list$file_header

    ## extract start time from file header
    start_timestamp <- extract_start_timestamp(file_header)

    ## attempt to detect `time_channel` automatically
    time_channel <- detect_time_channel(df, time_channel, nirs_device, verbose)

    ## rename from channel names, make duplicates unique, keep columns
    ## return list(data_renamed, nirs_renamed, time_renamed, event_renamed)
    renamed_list <- select_rename_data(
        df,
        nirs_channels,
        time_channel,
        event_channel,
        keep_all,
        verbose
    )
    df <- renamed_list$data
    nirs_renamed <- renamed_list$nirs_channel
    time_renamed <- renamed_list$time_channel
    event_renamed <- renamed_list$event_channel

    ## remove empty (NA) columns and rows
    df <- remove_empty_rows_cols(df)
    ## convert char decimal "," to "." and convert column types
    df <- convert_type(df, time_channel)
    ## convert POSIXct to numeric and/or recalc time from zero
    ## return list(data, start_timestamp) — start_timestamp from time_channel POSIXct
    time_list <- parse_time_channel(
        df,
        time_renamed,
        start_timestamp,
        add_timestamp,
        zero_time
    )
    df <- time_list$data
    ## extract start_timestamp from df if not already found in header
    if (is.null(start_timestamp)) {
        start_timestamp <- time_list$start_timestamp
    }

    ## standardise invalid to NA
    df[] <- lapply(df, \(.x) clean_invalid(.x))

    ## validate and estimate sample rate
    ## will write new "time" column if Oxysoft export rate detected
    ## return list(data_sampled, time_renamed, sample_rate)
    sample_list <- parse_sample_rate(
        df,
        file_header,
        time_renamed,
        sample_rate,
        nirs_device,
        verbose
    )
    df <- sample_list$data
    time_renamed <- sample_list$time_channel
    sample_rate <- sample_list$sample_rate

    ## print warnings for irregular samples
    detect_irregular_samples(df[[time_renamed]], time_renamed, verbose)

    ## assign metadata to attributes(data)
    metadata <- list(
        nirs_device = nirs_device,
        nirs_channels = nirs_renamed,
        time_channel = time_renamed,
        event_channel = event_renamed,
        sample_rate = sample_rate,
        start_timestamp = start_timestamp,
        verbose = verbose
    )

    return(create_mnirs_data(df, metadata))
}


#' Create an *{mnirs}* data frame with metadata
#'
#' Manually add class `"mnirs"` and metadata to an existing data frame.
#'
#' @param data A data frame with existing metadata (accessed with 
#'   `attributes(data)`).
#' 
#' @param ... Additional arguments with metadata to add to the data frame.
#'   Can be either seperate named arguments or a list of named values.
#'   - nirs_device
#'   - nirs_channels
#'   - time_channel
#'   - event_channel
#'   - sample_rate
#'   - start_timestamp
#'   - event_times 
#'   - interval_span 
#'
#' @details
#' Typically will only be called internally, but can be used to inject 
#'   *{mnirs}* metadata into any data frame.
#'
#' @returns 
#' A [tibble][tibble::tibble-package] of class `"mnirs"`. Metadata are stored
#'   as attributes and can be accessed with `attributes(data)`.
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
    validate_mnirs_data(data, 1L)

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
        start_timestamp = metadata$start_timestamp,
        event_times = metadata$event_times,
        interval_span = metadata$interval_span,
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
    if (length(matches) > 1L) {
        cli_abort(c(
            "x" = "Multiple files match {.val {file}}:",
            "i" = "Matching files: {.val {matches}}"
        ))
    }

    file <- match.arg(file, choices = dir_files)
    system.file("extdata", file, package = "mnirs", mustWork = TRUE)
}
