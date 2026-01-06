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
    if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        dt <- data.table::fread(
            file_path,
            sep = "", ## read single col to preserve metadata rows
            header = FALSE,
            colClasses = "character",
            na.strings = c("", "NA")
        )
        ## manual sep rows by "," and construct data.frame
        data_raw <- data.table::tstrsplit(dt[[1L]], ",", fixed = TRUE)
        data.table::setDT(data_raw)

    } else if (grepl("\\.xls(x)?$", file_path, ignore.case = TRUE)) {
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

        ## convert to data.table for consistent downstream handling
        data.table::setDT(data_raw)

    } else {
        ## validation: check file types
        cli_abort(c(
            "i" = "{.arg file_path} = {.path {file_path}}",
            "x" = "Unsupported file type.",
            "i" = "Only {.var .csv} or {.var .xls(x)} supported."
        ))
    }

    return(data_raw)
}


#' Read data table from raw data
#' @keywords internal
read_data_table <- function(
    data,
    nirs_channels,
    time_channel = NULL,
    event_channel = NULL,
    rows = 200L
) {
    search_df <- data[seq_len(min(rows, nrow(data))), ]

    ## detect header row where channels exists
    header_row <- which(apply(search_df, 1L, \(.row) {
        all(nirs_channels %in% .row)
    }))

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
    col_names <- as.character(data[header_row, ])
    data_table <- data[(header_row + 1L):nrow(data), ]
    data.table::setnames(data_table, col_names)
    file_header <- data[seq_len(header_row - 1L), ]

    return(list(
        data_table = data_table,
        file_header = file_header
    ))
}


#' Detect mnirs device from file metadata
#' @keywords internal
detect_mnirs_device <- function(data) {
    devices <- list(
        Artinis = "OxySoft",
        Train.Red = "Train.Red",
        Moxy = "LUT Part Number"
    )

    matches <- vapply(devices, \(.pattern) {
        any(grepl(.pattern, unlist(data), ignore.case = TRUE))
    }, logical(1))

    if (any(matches)) {
        return(names(devices)[which.max(matches)])
    }

    return(NULL)
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
    col_names <- names(data)
    time_regex <- "time|duration|hms|h+:m+:s+"
    time_idx <- grep(time_regex, col_names, ignore.case = TRUE)[1L]
    if (!is.na(time_idx)) {
        return(alert_time_channel(col_names[time_idx]))
    }

    ## find name of POSIXct column
    posix_idx <- Position(\(.col) inherits(.col, "POSIXct"), data)
    if (!is.na(posix_idx)) {
        return(alert_time_channel(col_names[posix_idx]))
    }

    ## find name of character column with time format strings
    string_idx <- Position(\(.col) {
        is.character(.col) && {
            first_val <- .col[which(!is.na(.col))[1L]]
            !is.na(first_val) && grepl("^\\d{1,2}:\\d{2}(:\\d{2})?", first_val)
        }
    }, data)

    if (!is.na(string_idx)) {
        return(alert_time_channel(col_names[string_idx]))
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
    return(stats::setNames(x, names))
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
        lapply(\(.x) if (is.null(.x)) .x else name_channels(.x))

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

    ## TODO check for duplicate channel_inputs not in data names?

    ## check channels exist in data
    if (length(setdiff(channel_vec, data_names)) > 0L) {
        cli_abort(c(
            "x" = "Channel names not detected.",
            "i" = "Column names are case sensitive and must match exactly."
        ))
    }

    ## keep all columns or only specified channels
    selected_cols <- if (keep_all) data_names else channel_vec

    ## rename columns from specified channel names
    data.table::setnames(data, data_names)
    result <- data[, selected_cols, drop = FALSE]
    channel_names_idx <- match(channel_vec, names(result))
    
    ## prioritise user input channel names if duplicates with result names
    prioritise_custom <- rename_duplicates(c(renamed_channels, names(result)))
    final_names <- prioritise_custom[!prioritise_custom %in% renamed_channels]
    final_names[channel_names_idx] <- renamed_channels
    data.table::setnames(result, final_names)

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


#' Remove empty rows and columns
#' @keywords internal
remove_empty_rows_cols <- function(dt) {
    col_names <- names(dt)

    ## row-wise: count non-empty values per row
    row_non_empty <- Reduce(
        `+`,
        lapply(col_names, \(.col) !is_empty(dt[[.col]]))
    )
    dt <- dt[row_non_empty > 0L, ]

    ## column-wise: identify columns with at least one non-empty value
    col_has_data <- vapply(col_names, \(.col) {
        any(!is_empty(dt[[.col]]))
    }, logical(1))

    cols_to_keep <- col_names[col_has_data]

    return(dt[, cols_to_keep, drop = FALSE])
}


#' Convert character columns to appropriate types
#' @keywords internal
convert_types <- function(dt) {
    col_names <- names(dt)

    ## identify character columns
    is_char <- vapply(col_names, \(.col) is.character(dt[[.col]]), logical(1))
    char_cols <- col_names[is_char]

    if (length(char_cols) == 0L) {
        return(dt)
    }

    ## identify which can be converted to numeric
    can_be_numeric <- vapply(char_cols, \(.col) {
        x <- dt[[.col]]
        non_na <- suppressWarnings(as.numeric(x[!is.na(x) & x != ""]))
        length(non_na) == 0L || !any(is.na(non_na))
    }, logical(1))

    num_cols <- char_cols[can_be_numeric]

    ## convert by reference
    lapply(num_cols, \(.col) {
        data.table::set(dt, j = .col, value = as.numeric(dt[[.col]]))
        NULL
    })

    return(dt)
}


#' Standardise invalid to NA values by reference
#' @keywords internal
clean_invalid <- function(dt) {
    ## process each column by reference
    lapply(names(dt), \(.col) {
        x <- dt[[.col]]
        if (is.character(x)) {
            idx <- which(x %in% c("", "NA"))
            data.table::set(dt, i = idx, j = .col, value = NA_character_)
        } else if (is.numeric(x)) {
            idx <- which(!is.finite(x))
            data.table::set(dt, i = idx, j = .col, value = NA_real_)
        }
    })

    return(dt)
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
        time_vec <- time_vec - time_vec[1L]
    }

    timestamp_vec <- NULL
    if (inherits(time_vec, "POSIXct")) {
        ## preserve timestamp
        timestamp_vec <- time_vec
        ## POSIXct to numeric seconds
        time_vec <- as.numeric(difftime(time_vec, time_vec[1L], units = "secs"))
    }

    # if (exists("timestamp_vec") && add_timestamp) {
    #     col_names <- names(data)
    #     ## add_timestamp preserves dttm column or adds
    #     time_idx <- match(time_channel, col_names)
    #     data_names <- append(col_names, "timestamp", time_idx)
    #     data[["timestamp"]] <- timestamp_vec
    #     data <- data[data_names]
    # }

    # data[[time_channel]] <- time_vec

    data.table::set(data, j = time_channel, value = time_vec)
    col_names <- names(data)

    if (!is.null(timestamp_vec) && add_timestamp) {
        time_idx <- match(time_channel, col_names)
        data[["timestamp"]] <- timestamp_vec
        col_order <- c(
            col_names[seq_len(time_idx)],
            "timestamp",
            setdiff(col_names, c(col_names[seq_len(time_idx)], "timestamp"))
        )
        data.table::setcolorder(data, col_order)
    }

    return(data)
}


#' Extract Oxysoft sample rate
#' @keywords internal
extract_oxysoft_rate <- function(file_header, sample_rate = NULL) {
    pos <- which(file_header == "Export sample rate", arr.ind = TRUE)
    sample_rate <- as.numeric(file_header[pos[1L], pos[2L] + 1L])

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

        col_names <- names(data)
        time_vec <- data[[time_channel]] / sample_rate
        time_idx <- match(time_channel, col_names)
        data_names <- append(col_names, "time", time_idx)
        data_names <- rename_duplicates(data_names)
        time_channel <- setdiff(data_names, col_names)
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
        data, time_channel, sample_rate, verbose
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
    irregular_idx <- c(
        which(duplicated(x)),
        which(diffs < 0),
        which(diffs >= 3600)
    )

    ## silence if no irregular samples
    if (length(irregular_idx) == 0) {
        return(invisible())
    }

    irregular_vec <- round(unique(x[irregular_idx]), 6)

    info_msg <- if (length(irregular_vec) > 5L) {
        ## if more than 5 irregular samples, print the first three
        irregular_display <- irregular_vec[1:3]

        "Investigate at {.arg {time_channel}} = {.val {irregular_display}} \\
        and {length(irregular_vec) - 3L} more."
    } else {
        ## if 5 or fewer irregular samples, print all of them
        "Investigate at {.arg {time_channel}} = {.val {irregular_vec}}."
    }

    cli_warn(c(
        "!" = "Duplicate or irregular {.arg time_channel} samples detected.",
        "i" = info_msg,
        "i" = "Re-sample with {.fn mnirs::resample_mnirs}."
    ))

    return(invisible())
}
