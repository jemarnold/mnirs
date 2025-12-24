#' Read raw data frame from file path
#' @keywords internal
read_file <- function(file_path) {
    ## validation: check file exists
    if (!file.exists(file_path)) {
        cli_abort(c(
            "x" = "File not found. Check that file exists.",
            "i" = "{.arg file_path} = {.path {file_path}}"
        ))
    }

    ## import data_raw from either excel or csv
    if (grepl("\\.xls(x)?$", file_path, ignore.case = TRUE)) {
        ## report error when file is open and cannot be accessed by readxl
        data_raw <- tryCatch(
            readxl::read_excel(
                path = file_path,
                col_names = FALSE,
                col_types = "text",
                .name_repair = "minimal"
            ),
            error = \(e) {
                if (grepl("cannot be opened", e$message)) {
                    cli_abort(c(
                        "{e}",
                        "x" = "File cannot be opened.",
                        "i" = "Check the file is not in use by another \\
                        application.",
                        "i" = "{.arg file_path} = {.path {file_path}}"
                    ))
                } else {
                    cli_abort(e$message)
                }
            }
        )
    } else if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        data_raw <- readr::read_csv(
            file_path,
            col_names = FALSE,
            col_types = readr::cols(.default = "c"),
            name_repair = "minimal",
            show_col_types = FALSE
        )
    } else {
        ## validation: check file types
        cli_abort(c(
            "x" = "Unrecognised file type. Only {.var .xls(x)} or \\
            {.var .csv} supported.",
            "i" = "{.arg file_path} = {.path {file_path}}"
        ))
    }

    return(data_raw)
}


#' Detect mnirs device from file metadata
#' @keywords internal
detect_mnirs_device <- function(data) {
    devices <- c(
        Artinis = "OxySoft",
        Train.Red = "Train.Red",
        Moxy = "LUT Part Number"
    )

    matches <- vapply(devices, \(.pattern) {
        any(grepl(
            .pattern,
            data[seq_len(min(100L, nrow(data))), ],
            ignore.case = TRUE
        ))
    }, logical(1))

    if (any(matches)) {
        return(names(devices)[which.max(matches)])
    } else {
        return(NULL)
    }
}


#' Read data table from raw data
#' @keywords internal
read_data_table <- function(
    data,
    nirs_channels,
    time_channel = NULL,
    event_channel = NULL
) {
    channel_vec <- c(nirs_channels, time_channel, event_channel)
    ## detect header row where channels exists
    header_row <- which(apply(
        data[seq_len(min(1000, nrow(data))), ],
        1,
        \(.row) all(nirs_channels %in% .row)
    ))

    ## validation: all channels must be detected to extract the data frame
    ## return error if channels string is detected at multiple rows
    if (length(header_row) == 0) {
        cli_abort(c(
            "x" = "Channel names not detected.",
            "i" = "Column names are case sensitive and must match exactly."
        ))
    } else if (length(header_row) > 1) {
        cli_abort(c(
            "x" = "Channel names detected at multiple rows.",
            "i" = "Ensure column names in data file are unique."
        ))
    }

    ## extract the data_table, and name by header row
    rows <- (header_row + 1):nrow(data)
    data_table <- stats::setNames(data[rows, ], data[header_row, ])
    file_header <- data[seq_len(header_row - 1), ]

    return(list(
        data_table = data_table,
        file_header = file_header
    ))
}


#' Detect time_channel from header row
#' @keywords internal
detect_time_channel <- function(
    data,
    time_channel = NULL,
    nirs_device = NULL,
    verbose = TRUE
) {
    if (!is.null(time_channel)) {
        return(time_channel)
    }

    ## return default sample column for Artinis Oxysoft
    if (!is.null(nirs_device) && nirs_device == "Artinis") {
        if (verbose) {
            cli_inform(c(
                "!" = "Oxysoft {.val sample} column detected."
            ))
        }
        return(c(sample = "1"))
    }

    ## alert and return detected `time_channel` column name
    alert_time_channel <- function(time_channel) {
        if (verbose) {
            cli_inform(c(
                "!" = "Detected {.arg time_channel} = {.val {time_channel}}."
            ))
        }
        return(time_channel)
    }

    ## match column names to possible time column names
    names <- names(data)
    time_regex <- "time|duration|hms|h+:m+:s+"
    time_idx <- grep(time_regex, names, ignore.case = TRUE)[1L]
    if (!is.na(time_idx)) {
        return(alert_time_channel(names[time_idx]))
    }

    ## find name of POSIXct column
    posix_idx <- Position(\(.col) inherits(.col, "POSIXct"), data)
    if (!is.na(posix_idx)) {
        return(alert_time_channel(names[posix_idx]))
    }

    ## find name of character column with time format strings
    string_idx <- Position(\(.col) {
        is.character(.col) && {
            first_val <- .col[which(!is.na(.col))[1L]]
            !is.na(first_val) && grepl("^\\d{1,2}:\\d{2}(:\\d{2})?", first_val)
        }
    }, data)
    if (!is.na(string_idx)) {
        return(alert_time_channel(names[string_idx]))
    }

    cli_abort(c(
        "x" = "{.arg time_channel} not detected.",
        "i" = "Define {.arg time_channel} explicitly."
    ))
}


#' Detect empty or NA strings
#' @keywords internal
is_empty <- function(x) {
    is.na(x) | x == ""
}


#' Rename duplicate strings in a vector with `make.unique()`
#' @keywords internal
rename_duplicates <- function(x) {
    if (is.null(x)) {
        return(x)
    }
    ## rename blank strings
    empty <- is_empty(x)
    x[empty] <- paste0("col_", which(empty))

    return(make.unique(x, sep = "_"))
}


#' Force names on character strings
#' @keywords internal
name_channels <- function(x) {
    ## if channels not named, names from object
    names <- names(x) %||% character(length(x))
    empty_names <- is_empty(names)
    names[empty_names] <- as.character(x)[empty_names]
    return(setNames(x, names))
}


#' Select data table columns and rename from channels, handling duplicates
#' @keywords internal
select_rename_data <- function(
    data,
    nirs_channels,
    time_channel,
    event_channel = NULL,
    keep_all = FALSE,
    verbose = TRUE
) {
    ## if channels not named, names from object
    channel_list <- list(
        time_channel = time_channel,
        event_channel = event_channel,
        nirs_channels = nirs_channels
    ) |>
        lapply(\(.x) {
            if (is.null(.x)) {
                return(.x)
            }
            name_channels(.x)
        })

    channel_inputs <- lapply(channel_list, names)
    renamed_channels <- lapply(channel_inputs, rename_duplicates)

    ## carry forward renamed channels
    nirs_renamed <- renamed_channels$nirs_channels
    time_renamed <- renamed_channels$time_channel
    event_renamed <- renamed_channels$event_channel

    ## de-duplicate channels & columns
    data_names <- rename_duplicates(names(data))
    channel_vec <- unlist(
        lapply(channel_list, rename_duplicates),
        use.names = FALSE
    )
    channel_inputs <- unlist(channel_inputs, use.names = FALSE)
    renamed_channels <- unlist(renamed_channels, use.names = FALSE)

    ## TODO check for duplicate channel_inputs not in data names33

    ## check channels exist in data
    if (length(setdiff(channel_vec, data_names)) > 0) {
        cli_abort(c(
            "x" = "Channel names not detected.",
            "i" = "Column names are case sensitive and must match exactly."
        ))
    }

    ## keep all columns or only specified channels
    if (keep_all) {
        selected_cols <- data_names
    } else {
        selected_cols <- channel_vec
    }

    ## rename columns from specified channel names
    result <- stats::setNames(data, data_names)
    result <- result[, selected_cols, drop = FALSE]
    channel_names_idx <- match(channel_vec, names(result))
    ## prioritise user input channel names if duplicates with result names
    prioritise_custom <- rename_duplicates(c(renamed_channels, names(result)))
    names(result) <- prioritise_custom[!prioritise_custom %in% renamed_channels]
    names(result)[channel_names_idx] <- renamed_channels

    renamed <- !names(result)[channel_names_idx] %in% channel_inputs

    if (verbose && any(renamed)) {
        ## warn about renamed names
        old_names <- channel_inputs[renamed]
        new_names <- names(result)[channel_names_idx][renamed]
        cli_warn(c(
            "!" = "Duplicate channel names detected.",
            "i" = "Renamed: {.val {paste(old_names, new_names, sep = ' = ')}}",
            "i" = "Unique channel names can be defined explicitly."
        ))
    }

    return(list(
        data = result,
        nirs_channel = nirs_renamed,
        time_channel = time_renamed,
        event_channel = event_renamed
    ))
}


#' Standardise invalid to NA values and round numeric to avoid float
#' precision error
#' @keywords internal
clean_invalid <- function(x) {
    if (is.character(x)) {
        x[x %in% c("", "NA")] <- NA_character_
    } else if (is.numeric(x)) {
        x[!is.finite(x)] <- NA_real_
    }
    return(x)
}

#' Remove Empty Rows and Columns
#' @keywords internal
remove_empty_rows_cols <- function(data) {
    data <- data[rowSums(!is_empty(data)) > 0, , drop = FALSE]
    return(data[, colSums(!is_empty(data)) > 0, drop = FALSE])
}


#' Parse time_channel character or dttm to numeric
#' @keywords internal
parse_time_channel <- function(
    data,
    time_channel,
    add_timestamp = FALSE,
    zero_time = FALSE
) {
    time_vec <- data[[time_channel]]
    ## fractional unix time to POSIXct
    if (is.numeric(time_vec) && all(time_vec <= 1, na.rm = TRUE)) {
        time_vec <- as.POSIXct(time_vec * 86400)
    } else if (is.character(time_vec)) {
        ## character to POSIXct
        formats <- c(
            "%Y-%m-%dT%H:%M:%OS",
            "%Y-%m-%dT%H:%M:%OS%z",
            "%Y-%m-%d %H:%M:%OS",
            "%Y/%m/%d %H:%M:%OS",
            "%d-%m-%Y %H:%M:%OS",
            "%d/%m/%Y %H:%M:%OS",
            "%H:%M:%OS"
        )
        time_vec <- as.POSIXct(time_vec, tryFormats = formats, optional = TRUE)
    }

    if (zero_time && is.numeric(time_vec)) {
        ## recalculate to start from zero
        time_vec <- time_vec - time_vec[1]
    }

    if (inherits(time_vec, "POSIXct")) {
        ## preserve timestamp
        timestamp_vec <- time_vec
        ## POSIXct to numeric seconds
        time_vec <- as.numeric(difftime(time_vec, time_vec[1], units = "secs"))
    }

    if (exists("timestamp_vec") && add_timestamp) {
        ## add_timestamp preserves dttm column or adds
        time_idx <- match(time_channel, names(data))
        data_names <- append(names(data), "timestamp", time_idx)
        data[["timestamp"]] <- timestamp_vec
        data <- data[data_names]
    }

    data[[time_channel]] <- time_vec

    return(data)
}


#' Extract Oxysoft sample rate
#' @keywords internal
extract_oxysoft_rate <- function(file_header, sample_rate = NULL) {
    pos <- which(file_header == "Export sample rate", arr.ind = TRUE)
    sample_rate <- as.numeric(file_header[pos[1], pos[2] + 1])

    return(sample_rate)
}


#' Validate and Estimate Sample Rate
#' @keywords internal
parse_sample_rate <- function(
    data,
    file_header,
    time_channel,
    sample_rate = NULL,
    nirs_device = NULL,
    verbose = TRUE
) {
    ## if Oxysoft, sample_rate will be detected = 1
    ## extract and overwrite with exported sample_rate
    ## create new "time" column at col_idx behind `time_channel`
    if (!is.null(nirs_device) && nirs_device == "Artinis") {
        sample_rate <- extract_oxysoft_rate(file_header, sample_rate)

        time_vec <- data[[time_channel]] / sample_rate
        time_idx <- match(time_channel, names(data))
        data_names <- append(names(data), "time", time_idx)
        data_names <- rename_duplicates(data_names)
        time_channel <- setdiff(data_names, names(data))
        data[[time_channel]] <- time_vec
        data <- data[data_names]

        if (verbose) {
            cli_inform(c(
                "!" = "Oxysoft {.arg sample_rate} = {.val {sample_rate}} Hz.",
                "i" = "{.arg time_channel} = {.val {time_channel}} added to \\
                the data frame, in {.cls seconds}."
            ))
        }
    }

    ## validate priority user input sample_rate
    ## metadata check will be skipped
    ## will estimate from time_channel (time_channel)
    ## will error on unable to estimate sample_rate
    sample_rate <- validate_sample_rate(
        data,
        time_channel,
        sample_rate,
        verbose
    )

    return(list(
        data = data,
        time_channel = time_channel,
        sample_rate = sample_rate
    ))
}


#' Report warnings for unbalanced time_channel samples
#' @keywords internal
detect_irregular_samples <- function(
    x,
    time_channel,
    verbose = TRUE
) {
    if (!verbose) {
        return(invisible())
    }

    ## find duplicated, unordered, or big gaps in samples
    diffs <- diff(x)
    duplicates <- which(duplicated(x))
    unordered <- which(diffs < 0)
    big_gap <- which(diffs >= 3600)
    irregular_idx <- c(duplicates, unordered, big_gap)
    

    ## silence if no irregular samples
    if (length(irregular_idx) == 0) {
        return(invisible())
    }

    irregular_vec <- round(unique(x[irregular_idx]), 6)

    if (length(irregular_vec) > 5) {
        ## if more than 5 irregular samples, print the first three
        irregular_n <- min(3, length(irregular_vec))
        irregular_display <- irregular_vec[seq_len(irregular_n)]
        info_msg <- c(
            "Investigate at {.arg {time_channel}} = \\
            {.val {irregular_display}} and {length(irregular_vec) - 3} more."
        )
    } else {
        ## if 5 or fewer irregular samples, print all of them
        info_msg <- c(
            "Investigate at {.arg {time_channel}} = {.val {irregular_vec}}."
        )
    }

    cli_warn(c(
        "!" = "Duplicate or irregular {.arg time_channel} samples detected.",
        "i" = info_msg,
        "i" = "Re-sample with {.fn mnirs::resample_mnirs}."
    ))

    return(invisible())
}
