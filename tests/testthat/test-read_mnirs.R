## example_mnirs() ====================================================
test_that("example_mnirs() returns all files when file = NULL", {
    files <- example_mnirs()
    expect_type(files, "character")
    expect_true(length(files) > 0)
})

test_that("example_mnirs() returns valid path for exact match", {
    # Assumes at least one file exists in extdata
    all_files <- example_mnirs()
    skip_if(length(all_files) == 0, "No example files available")

    path <- example_mnirs(all_files[1])
    expect_type(path, "character")
    expect_true(file.exists(path))
    expect_match(path, all_files[1], fixed = TRUE)
})

test_that("example_mnirs() returns valid path for partial match", {
    skip_if_not(any(grepl("moxy_ramp", example_mnirs(), fixed = TRUE)),
                "moxy_ramp.xlsx not available")

    path <- example_mnirs("moxy_ramp")
    expect_true(file.exists(path))
    expect_match(path, "moxy_ramp", fixed = TRUE)
})

test_that("example_mnirs() errors on multiple partial matches", {
    skip_if_not(sum(grepl("moxy", example_mnirs(), fixed = TRUE)) > 1,
                "Multiple moxy files not available")

    expect_error(example_mnirs("moxy"), "Multiple files match")
})

test_that("example_mnirs() errors on non-existent file", {
    expect_error(example_mnirs("nonexistent_file_xyz"), "'arg' should be one of")
})

test_that("example_mnirs() does not show files with `~`", {
    # Create test directory structure
    test_dir <- file.path(tempdir(), "epl_test")
    dir.create(test_dir, recursive = TRUE)

    # Create test files
    file.create(file.path(test_dir, "data.csv"))
    file.create(file.path(test_dir, "~temp.csv"))
    file.create(file.path(test_dir, "results.xlsx"))

    result <- list.files(test_dir)
    expect_length(result, 3)
    expect_true(any(grepl("^~", result)))

    result <- list.files(test_dir, pattern = "^[^~]")

    expect_length(result, 2)
    expect_false(any(grepl("^~", result)))
    expect_true(all(c("data.csv", "results.xlsx") %in% result))
})


## read_file() ==================================================

test_that("read_file() reads Excel files correctly", {
    file_path <- example_mnirs("moxy_ramp")
    skip_if(!grepl("\\.xls(x)?$", file_path, ignore.case = TRUE))

    result <- read_file(file_path)

    expect_s3_class(result, "data.frame")
    expect_true(ncol(result) > 0)
    expect_true(nrow(result) > 0)
    expect_true(all(sapply(result, is.character)))
})

test_that("read_file() reads CSV files correctly", {
    file_path <- example_mnirs("train.red")
    skip_if(!grepl("\\.csv$", file_path, ignore.case = TRUE))

    result <- read_file(file_path)

    expect_s3_class(result, "data.frame")
    expect_true(ncol(result) > 0)
    expect_true(nrow(result) > 0)
    expect_true(all(sapply(result, is.character)))
})

test_that("read_file() errors", {
    expect_error(
        read_file("nonexistent_file.xlsx"),
        "File not found"
    )

    temp_file <- tempfile(fileext = ".txt")
    writeLines("test", temp_file)
    on.exit(unlink(temp_file))

    expect_error(
        read_file(temp_file),
        "Unrecognised file type"
    )
})

test_that("read_file() handles locked Excel files", {
    skip("Manual test: requires open excel file")
    skip_on_cran()
    # This test requires actually opening the system file
    ## doesn't seem to lock the file? non-interactive environment?
    file_path <- example_mnirs("moxy_ramp")
    skip_if(!grepl("\\.xls(x)?$", file_path, ignore.case = TRUE))

    read_file(file_path)
})


## detect_mnirs_device() ===============================================
test_that("detect_mnirs_device works on example files", {
    expect_equal(
        read_file(example_mnirs("moxy_ramp")) |>
            detect_mnirs_device(),
        "Moxy"
    )

    expect_equal(
        read_file(example_mnirs("train.red")) |>
            detect_mnirs_device(),
        "Train.Red"
    )

    expect_equal(
        read_file(example_mnirs("artinis_intervals")) |>
            detect_mnirs_device(),
        "Artinis"
    )
})

test_that("detect_mnirs_device() returns NULL when no match", {
    data <- data.frame(
        V1 = c("Unknown", "device", "data"),
        V2 = c("header", "col1", "val1")
    )

    expect_null(detect_mnirs_device(data))
})

test_that("detect_mnirs_device() only checks first 100 rows", {
    data <- data.frame(
        V1 = c(rep("random", 101), "OxySoft"),
        V2 = rep("data", 102)
    )

    expect_null(detect_mnirs_device(data))
})


## read_data_table() ===================================================
test_that("read_data_table() extracts data with valid channels", {
    data <- data.frame(
        V1 = c("meta1", "meta2", "O2Hb", "10", "20"),
        V2 = c("meta1", "meta2", "HHb", "5", "15"),
        V3 = c("meta1", "meta2", "Time", "0.1", "0.2"),
        stringsAsFactors = FALSE
    )

    result <- read_data_table(
        data,
        nirs_channels = c("O2Hb", "HHb"),
        time_channel = "Time"
    )

    expect_type(result, "list")
    expect_named(result, c("data_table", "file_header"))
    expect_s3_class(result$data_table, "data.frame")
    expect_s3_class(result$file_header, "data.frame")

    expect_equal(nrow(result$data_table), 2)
    expect_equal(ncol(result$data_table), 3)
    expect_equal(names(result$data_table), c("O2Hb", "HHb", "Time"))
    expect_true(all(result$data_table == data[4:5,]))

    expect_equal(nrow(result$file_header), 2)
    expect_equal(ncol(result$file_header), 3)
    expect_true(all(result$file_header == data[1:2,]))
})

test_that("read_data_table() works with event channel", {
    data <- data.frame(
        V1 = c("header", "O2Hb", "10"),
        V2 = c("header", "Time", "0.1"),
        V3 = c("header", "Event", "Start"),
        stringsAsFactors = FALSE
    )

    result <- read_data_table(
        data,
        nirs_channels = "O2Hb",
        time_channel = "Time",
        event_channel = "Event"
    )

    expect_equal(names(result$data_table), c("O2Hb", "Time", "Event"))
    expect_equal(result$data_table$Event, "Start")
})

test_that("read_data_table() searches first 1000 rows only", {
    data <- data.frame(
        V1 = c(rep("filler", 1001), "O2Hb", "10"),
        V2 = c(rep("filler", 1001), "Time", "0.1"),
        stringsAsFactors = FALSE
    )

    expect_error(
        read_data_table(data, "O2Hb", "Time"),
        "Channel names not detected"
    )

    data <- data.frame(
        V1 = c(rep("filler", 999), "O2Hb", rep("10", 10)),
        V2 = c(rep("filler", 999), "Time", rep("0.1", 10)),
        stringsAsFactors = FALSE
    )

    expect_silent(result <- read_data_table(data, "O2Hb", "Time"))
    expect_equal(nrow(result$data_table), 10)
    expect_equal(ncol(result$data_table), 2)

    expect_equal(nrow(result$file_header), 999)
    expect_equal(ncol(result$file_header), 2)
})

test_that("read_data_table() errors when channels not found", {
    data <- data.frame(
        V1 = c("header", "WrongChannel", "10"),
        V2 = c("header", "Time", "0.1"),
        stringsAsFactors = FALSE
    )

    expect_error(
        read_data_table(data, "O2Hb", "Time"),
        "Channel names not detected"
    )
})

test_that("read_data_table() errors with duplicate headers", {
    data <- data.frame(
        V1 = c("O2Hb", "O2Hb", "10", "0.1"),
        V2 = c("HHb", "HHb", "5", "Start"),
        V3 = c("Time", "Time", "0", "1"),
        stringsAsFactors = FALSE
    )

    expect_error(
        read_data_table(data, c("O2Hb", "HHb"), "Time"),
        "Channel names detected at multiple rows"
    )
})

test_that("read_data_table() is case sensitive", {
    data <- data.frame(
        V1 = c("meta", "o2hb", "10"),
        V2 = c("meta", "time", "0.1"),
        stringsAsFactors = FALSE
    )

    expect_error(
        read_data_table(data, "O2Hb", "Time"),
        "case sensitive"
    )
})


## detect_time_channel() ==============================================
test_that("detect_time_channel returns provided time_channel", {
    df <- data.frame(x = 1:5, y = 6:10)
    expect_equal(
        detect_time_channel(df, time_channel = "custom", verbose = FALSE),
        "custom"
    )
})

test_that("detect_time_channel returns sample for Artinis", {
    df <- data.frame(`1` = 1:5, check.names = FALSE)
    expect_equal(
        detect_time_channel(df, nirs_device = "Artinis", verbose = FALSE),
        c(sample = "1")
    )
})

test_that("detect_time_channel finds time column by name", {
    df <- data.frame(time = 1:5, value = 6:10)
    expect_equal(
        detect_time_channel(df, verbose = FALSE),
        "time"
    )

    df <- data.frame(Time = 1:5, value = 6:10)
    expect_equal(
        detect_time_channel(df, verbose = FALSE),
        "Time"
    )

    df <- tibble::tibble("hh:mm:ss" = 1:5, value = 6:10)
    expect_equal(
        detect_time_channel(df, verbose = FALSE),
        "hh:mm:ss"
    )

    df <- data.frame("hms" = 1:5, value = 6:10)
    expect_equal(
        detect_time_channel(df, verbose = FALSE),
        "hms"
    )

    df <- data.frame(duration = 1:5, value = 6:10)
    expect_equal(
        detect_time_channel(df, verbose = FALSE),
        "duration"
    )
})

test_that("detect_time_channel finds POSIXct column", {
    df <- data.frame(
        value = 1:5,
        timestamp = as.POSIXct("2024-01-01 12:00:00") + 1:5
    )
    expect_equal(
        detect_time_channel(df, verbose = FALSE),
        "timestamp"
    )
})

test_that("detect_time_channel finds character time format", {
    df <- data.frame(
        value = 1:5,
        time_str = c("12:30:45", "12:30:46", "12:30:47",
                     "12:30:48", "12:30:49")
    )
    expect_equal(
        detect_time_channel(df, verbose = FALSE),
        "time_str"
    )

    # Test with H:MM format
    df <- data.frame(
        value = 1:5,
        time_str = c("1:30", "1:31", "1:32", "1:33", "1:34")
    )
    expect_equal(
        detect_time_channel(df, verbose = FALSE),
        "time_str"
    )
})

test_that("detect_time_channel handles NA values in character column", {
    df <- data.frame(
        value = 1:5,
        time_str = c(NA, "12:30:45", "12:30:46", "12:30:47",
                     "12:30:48")
    )
    expect_equal(
        detect_time_channel(df, verbose = FALSE),
        "time_str"
    )
})

test_that("detect_time_channel errors when no time column found", {
    df <- data.frame(x = 1:5, y = 6:10)
    expect_error(
        detect_time_channel(df, verbose = FALSE),
        "time_channel.*not detected"
    )
})

test_that("detect_time_channel prioritises time_channel argument", {
    df <- data.frame(
        time = 1:5,
        custom = 6:10,
        timestamp = as.POSIXct("2024-01-01 12:00:00") + 1:5
    )
    expect_equal(
        detect_time_channel(df, time_channel = "custom", verbose = FALSE),
        "custom"
    )
})

test_that("detect_time_channel verbose messages work", {
    df <- data.frame(time = 1:5, value = 6:10)
    expect_message(
        detect_time_channel(df, verbose = TRUE),
        "Detected.*time_channel"
    ) |>
        expect_message("Overwrite")

    df_artinis <- data.frame(`1` = 1:5, check.names = FALSE)
    expect_message(
        detect_time_channel(df_artinis, nirs_device = "Artinis",
                            verbose = TRUE),
        "Oxysoft detected"
    ) |>
        expect_message("Overwrite")
})


## rename_duplicates() ================================================
test_that("rename_duplicates() handles duplicate strings", {
    x <- c("O2Hb", "HHb", "O2Hb", "Time")
    result <- rename_duplicates(x)

    expect_equal(result, c("O2Hb", "HHb", "O2Hb_1", "Time"))
    expect_true(all(!duplicated(result)))
})

test_that("rename_duplicates() handles empty strings", {
    x <- c("O2Hb", "", "HHb", "")
    result <- rename_duplicates(x)

    expect_equal(result, c("O2Hb", "col_2", "HHb", "col_4"))
})

test_that("rename_duplicates() handles naming empty as duplicated", {
    skip("rename_duplicates should not encounter this situation")
    x <- c("O2Hb", "", "HHb", "col_2")
    result <- rename_duplicates(x)

    expect_equal(result, c("O2Hb", "col_2", "HHb", "col_2_1"))
})

test_that("rename_duplicates() handles NA values", {
    x <- c("O2Hb", NA, "HHb", NA)
    result <- rename_duplicates(x)

    expect_equal(result, c("O2Hb", "col_2", "HHb", "col_4"))
})

test_that("rename_duplicates() handles NULL input", {
    expect_null(rename_duplicates(NULL))
})

test_that("rename_duplicates() handles all unique values", {
    x <- c("O2Hb", "HHb", "Time")
    result <- rename_duplicates(x)

    expect_equal(result, x)
})

test_that("rename_duplicates() handles multiple duplicates", {
    x <- c("O2Hb", "O2Hb", "O2Hb")
    result <- rename_duplicates(x)

    expect_equal(result, c("O2Hb", "O2Hb_1", "O2Hb_2"))
})

## name_channels() ====================================================
test_that("name_channels() names unnamed vector", {
    x <- c("O2Hb", "HHb", "Time")
    result <- name_channels(x)

    expect_equal(names(result), c("O2Hb", "HHb", "Time"))
    expect_equal(as.character(result), x)
})

test_that("name_channels() preserves existing names", {
    x <- c(oxy = "O2Hb", deoxy = "HHb", time = "Time")
    result <- name_channels(x)

    expect_equal(names(result), c("oxy", "deoxy", "time"))
    expect_equal(as.character(result), c("O2Hb", "HHb", "Time"))
})

test_that("name_channels() fills in missing names", {
    x <- c(oxy = "O2Hb", "HHb", time = "Time")
    result <- name_channels(x)

    expect_equal(names(result), c("oxy", "HHb", "time"))
})

test_that("name_channels() handles NA names", {
    x <- c("O2Hb", "HHb")
    names(x) <- c(NA, "deoxy")
    result <- name_channels(x)

    expect_equal(names(result), c("O2Hb", "deoxy"))
})

test_that("name_channels() handles all empty names", {
    x <- c("O2Hb", "HHb", "Time")
    names(x) <- c("", "", "")
    result <- name_channels(x)

    expect_equal(names(result), c("O2Hb", "HHb", "Time"))
})


## select_rename_data() ===========================================
test_that("select_rename_data() selects and renames channels \\
          in correct order", {
    data <- data.frame(
        O2Hb = c("10", "20"),
        HHb = c("5", "15"),
        Time = c("0.1", "0.2"),
        stringsAsFactors = FALSE
    )

    result <- select_rename_data(
        data,
        nirs_channels = c(oxy = "O2Hb", deoxy = "HHb"),
        time_channel = c(time = "Time"),
        verbose = FALSE
    )

    expect_equal(names(result$data), c("time", "oxy", "deoxy"))
    expect_equal(result$nirs_channel, c("oxy", "deoxy"))
    expect_equal(result$time_channel, c("time"))
})

test_that("select_rename_data() works with unnamed channels", {
    data <- data.frame(
        O2Hb = c("10"),
        Time = c("0.1"),
        stringsAsFactors = FALSE
    )

    result <- select_rename_data(
        data,
        nirs_channels = "O2Hb",
        time_channel = "Time",
        verbose = FALSE
    )

    expect_equal(names(result$data), c("Time", "O2Hb"))
    expect_equal(result$nirs_channel, "O2Hb")
    expect_equal(result$time_channel, "Time")
})

test_that("select_rename_data() includes event channel", {
    data <- data.frame(
        O2Hb = c("10"),
        Time = c("0.1"),
        Event = c("Start"),
        stringsAsFactors = FALSE
    )

    result <- select_rename_data(
        data,
        nirs_channels = "O2Hb",
        time_channel = "Time",
        event_channel = "Event",
        verbose = FALSE
    )

    expect_equal(names(result$data), c("Time", "Event", "O2Hb"))
    expect_equal(result$event_channel, "Event")
})

test_that("select_rename_data() handles duplicate channel names", {
    skip('currently returns "Channel names not detected error"')

    data <- data.frame(
        O2Hb = c("10"),
        Time = c("0.1"),
        stringsAsFactors = FALSE
    )

    expect_warning(
        result <- select_rename_data(
            data,
            nirs_channels = c("O2Hb", "O2Hb"),
            time_channel = "Time",
            verbose = TRUE
        ),
        "Duplicated channel names"
    )

    expect_equal(result$nirs_channel, c("O2Hb", "O2Hb_1"))
})

test_that("select_rename_data() handles duplicate data columns", {
    data <- data.frame(
        O2Hb = c("10"),
        O2Hb = c("20"),
        Time = c("0.1"),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )

    result <- select_rename_data(
        data,
        nirs_channels = c(oxy1 = "O2Hb", oxy2 = "O2Hb"),
        time_channel = "Time",
        verbose = FALSE
    )

    expect_equal(names(result$data), c("Time", "oxy1", "oxy2"))
})

test_that("select_rename_data() keeps all columns with keep_all", {
    data <- data.frame(
        O2Hb = c("10"),
        HHb = c("5"),
        Time = c("0.1"),
        Extra = c("x"),
        stringsAsFactors = FALSE
    )

    result <- select_rename_data(
        data,
        nirs_channels = c(o2hb = "O2Hb", hhb = "HHb"),
        time_channel = c(time = "Time"),
        keep_all = TRUE,
        verbose = FALSE
    )

    expect_equal(ncol(result$data), 4)
    expect_true(all(
        c("o2hb", "hhb", "time", "Extra") %in% names(result$data)
    ))
})

test_that("select_rename_data() drops extra columns by default", {
    data <- data.frame(
        O2Hb = c("10"),
        HHb = c("5"),
        Time = c("0.1"),
        Extra = c("x"),
        stringsAsFactors = FALSE
    )

    result <- select_rename_data(
        data,
        nirs_channels = "O2Hb",
        time_channel = "Time",
        keep_all = FALSE,
        verbose = FALSE
    )

    expect_equal(ncol(result$data), 2)
    expect_false("Extra" %in% names(result$data))
})

test_that("select_rename_data() errors when channel not found", {
    data <- data.frame(
        O2Hb = c("10"),
        Time = c("0.1"),
        stringsAsFactors = FALSE
    )

    expect_error(
        select_rename_data(
            data,
            nirs_channels = "HHb",
            time_channel = "Time"
        ),
        "Channel names not detected"
    )
})

test_that("select_rename_data() suppresses warnings with verbose", {
    data <- data.frame(
        O2Hb = c("10"),
        O2Hb = c("20"),
        Time = c("0.1"),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )

    expect_silent(
        select_rename_data(
            data,
            nirs_channels = c(o2hb = "O2Hb", o2hb = "O2Hb"),
            time_channel = "Time",
            verbose = FALSE
        )
    )

    expect_warning(
        select_rename_data(
            data,
            nirs_channels = c(o2hb = "O2Hb", o2hb = "O2Hb"),
            time_channel = "Time",
            verbose = TRUE
        ),
        "o2hb = o2hb_1"
    )
})

test_that("select_rename_data() prioritises custom names over data", {
    data <- data.frame(
        O2Hb = c("10"),
        Time = c("0.1"),
        custom = c("x"),
        stringsAsFactors = FALSE
    )

    result <- select_rename_data(
        data,
        nirs_channels = c(custom = "O2Hb"),
        time_channel = "Time",
        keep_all = TRUE,
        verbose = FALSE
    )

    expect_true(all(c("custom", "custom_1") %in% names(result$data)))
    expect_equal(result$data$custom, "10")
})


## remove_empty_rows_cols() ===========================================
test_that("remove_empty_rows_cols() removes empty rows & cols", {
    data <- data.frame(
        A = c("1", "", "3"),
        B = c("x", "", "z"),
        stringsAsFactors = FALSE
    )

    result <- remove_empty_rows_cols(data)
    expect_equal(nrow(result), 2)
    expect_equal(result$A, c("1", "3"))

    data <- data.frame(
        A = c("1", "2"),
        B = c("", ""),
        C = c("x", "y"),
        stringsAsFactors = FALSE
    )

    result <- remove_empty_rows_cols(data)

    expect_equal(ncol(result), 2)
    expect_equal(names(result), c("A", "C"))

    data <- data.frame(
        A = c("1", "", "3"),
        B = c("", "", ""),
        C = c("x", "", "z"),
        stringsAsFactors = FALSE
    )

    result <- remove_empty_rows_cols(data)
    expect_equal(nrow(result), 2)
    expect_equal(result$A, c("1", "3"))
    expect_equal(ncol(result), 2)
    expect_equal(names(result), c("A", "C"))
})

test_that("remove_empty_rows_cols() handles NA values", {
    data <- data.frame(
        A = c("1", NA, "3"),
        B = c(NA, NA, NA),
        stringsAsFactors = FALSE
    )

    result <- remove_empty_rows_cols(data)

    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 1)
})

test_that("remove_empty_rows_cols() preserves non-empty data", {
    data <- data.frame(
        A = c("1", "2", "3"),
        B = c("x", "y", "z"),
        stringsAsFactors = FALSE
    )

    result <- remove_empty_rows_cols(data)

    expect_equal(result, data)
})

test_that("remove_empty_rows_cols() handles all empty data", {
    data <- data.frame(
        A = c("", ""),
        B = c("", ""),
        stringsAsFactors = FALSE
    )

    result <- remove_empty_rows_cols(data)

    expect_equal(nrow(result), 0)
    expect_equal(ncol(result), 0)
})

## parse_time_channel() ================================================
test_that("parse_time_channel() parses numeric time from zero", {
    data <- data.frame(
        time = c(10, 20, 30),
        value = c(1, 2, 3)
    )

    result <- parse_time_channel(data, "time", zero_time = TRUE)

    expect_equal(result$time, c(0, 10, 20))
})

# test_that("parse_time_channel() parses fractional unix time", {
#     data <- data.frame(
#         time = c(0.5, 0.6, 0.7),
#         value = c(1, 2, 3)
#     )
#
#     result <- parse_time_channel(data, "time")
#
#     expect_s3_class(result$time, "POSIXct")
# })

test_that("parse_time_channel() parses ISO 8601 timestamps", {
    data <- data.frame(
        time = c("2025-01-01T10:00:00", "2025-01-01T10:00:01"),
        value = c(1, 2),
        stringsAsFactors = FALSE
    )

    result <- parse_time_channel(data, "time")

    expect_type(result$time, "double")
    expect_equal(result$time, c(0, 1))
})

test_that("parse_time_channel() parses various date formats", {
    formats <- list(
        c("2025-01-01 10:00:00", "2025-01-01 10:00:01"),
        c("2025/01/01 10:00:00", "2025/01/01 10:00:01"),
        c("01-01-2025 10:00:00", "01-01-2025 10:00:01"),
        c("01/01/2025 10:00:00", "01/01/2025 10:00:01")
    )

    for (fmt in formats) {
        data <- data.frame(time = fmt, value = c(1, 2),
                           stringsAsFactors = FALSE)
        result <- parse_time_channel(data, "time")
        expect_type(result$time, "double")
    }
})

test_that("parse_time_channel() parses time only format", {
    data <- data.frame(
        time = c("10:00:00", "10:00:01"),
        value = c(1, 2),
        stringsAsFactors = FALSE
    )

    result <- parse_time_channel(data, "time")

    expect_type(result$time, "double")
})

test_that("parse_time_channel() preserves timestamp with add_timestamp=TRUE", {
    data <- data.frame(
        time = c("2025-01-01T10:00:00", "2025-01-01T10:00:01"),
        value = c(1, 2),
        stringsAsFactors = FALSE
    )

    result <- parse_time_channel(data, "time", add_timestamp = TRUE)

    expect_true("timestamp" %in% names(result))
    expect_s3_class(result$timestamp, "POSIXct")
    expect_type(result$time, "double")
    expect_equal(which(names(result) == "timestamp"), 2)
})

test_that("parse_time_channel() converts POSIXct with add_timestamp=FALSE", {
    data <- data.frame(
        time = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 10:00:01")),
        value = c(1, 2)
    )

    result <- parse_time_channel(data, "time", add_timestamp = FALSE)

    expect_type(result$time, "double")
    expect_false("timestamp" %in% names(result))
    expect_equal(result$time, c(0, 1))
})

test_that("parse_time_channel() recalculates from zero with POSIXct", {
    data <- data.frame(
        time = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 10:00:01")),
        value = c(1, 2)
    )

    result <- parse_time_channel(data, "time", zero_time = FALSE)

    expect_type(result$time, "double")
    expect_equal(result$time, c(0, 1))
})

test_that("parse_time_channel() preserves numeric time", {
    data <- data.frame(
        time = c(10.5, 20.5, 30.5),
        value = c(1, 2, 3)
    )

    result <- parse_time_channel(data, "time", zero_time = FALSE)

    expect_equal(result$time, data$time)
})

test_that("parse_time_channel() handles milliseconds in timestamps", {
    data <- data.frame(
        time = c("2025-01-01T10:00:00.123", "2025-01-01T10:00:01.456"),
        value = c(1, 2),
        stringsAsFactors = FALSE
    )

    result <- parse_time_channel(data, "time")

    expect_type(result$time, "double")
    expect_true(result$time[2] > 1)
})

## parse_sample_rate() ================================================
test_that("parse_sample_rate returns correct structure", {
    data <- data.frame(time = seq(0, 10, by = 0.1),
                       value = rnorm(101, 10, 1))
    file_header <- matrix(NA, nrow = 5, ncol = 5)

    result <- parse_sample_rate(
        data = data,
        file_header = file_header,
        time_channel = "time",
        sample_rate = 10,
        nirs_device = NULL,
        verbose = FALSE
    )

    expect_type(result, "list")
    expect_named(result, c("data", "time_channel", "sample_rate"))
    expect_s3_class(result$data, "data.frame")
    expect_type(result$time_channel, "character")
    expect_type(result$sample_rate, "double")
    expect_equal(result$data$value, data$value)
    expect_equal(nrow(result$data), nrow(data))
    expect_equal(result$sample_rate, 10)
})

test_that("parse_sample_rate handles Artinis device", {
    file_header <- read_file(example_mnirs("artinis_intervals"))
    data <- read_mnirs(
        example_mnirs("artinis_intervals"),
        nirs_channels = c(HHb = 2, O2Hb = 3),
        time_channel = c(sample = 1),
        event_channel = NULL,
        verbose = FALSE
    ) |>
        dplyr::select(-time)

    result <- parse_sample_rate(
        data = data,
        file_header = file_header,
        time_channel = "sample",
        sample_rate = NULL,
        nirs_device = "Artinis",
        verbose = FALSE
    )

    expect_equal(result$sample_rate, 10)
    expect_true("time" %in% names(result$data))
    expect_equal(result$time_channel, "time")
    expect_equal(ncol(result$data), 4)
    expect_equal(result$data$time, data$sample/10)

})

# test_that("parse_sample_rate errors when rate indeterminable", {
#     data <- data.frame(x = rep(1, 10))
#     file_header <- matrix(NA, nrow = 5, ncol = 5)
#
#     expect_error(
#         parse_sample_rate(
#             data = data,
#             file_header = file_header,
#             time_channel = "x",
#             sample_rate = NULL,
#             verbose = FALSE
#         ),
#         "sample_rate"
#     )
# })

test_that("parse_sample_rate verbose output for Artinis", {
    file_header <- read_file(example_mnirs("artinis_intervals"))
    data <- read_mnirs(
        example_mnirs("artinis_intervals"),
        nirs_channels = c(HHb = 2, O2Hb = 3),
        time_channel = c(sample = 1),
        event_channel = NULL,
        verbose = FALSE
    ) |>
        dplyr::select(-time)

    expect_message(
        result <- parse_sample_rate(
            data = data,
            file_header = file_header,
            time_channel = "sample",
            sample_rate = NULL,
            nirs_device = "Artinis",
            verbose = TRUE
        ),
        "Oxysoft detected") |>
        expect_message("time.*channel added") |>
        expect_message("Overwrite")
})

## detect_irregular_samples() =========================================
test_that("detect_irregular_samples returns invisibly with no irregularities", {
    x <- seq(0, 100, by = 1)
    expect_invisible(detect_irregular_samples(x, "time", verbose = TRUE))
    expect_invisible(detect_irregular_samples(x, "time", verbose = FALSE))
})

test_that("detect_irregular_samples detects duplicated samples", {
    x <- c(0, 1, 2, 2, 3, 4)
    expect_warning(
        detect_irregular_samples(x, "time"),
        "duplicated or irregular samples"
    )
    expect_warning(
        detect_irregular_samples(x, "time"),
        "time.*=.*2"
    )
})

test_that("detect_irregular_samples detects unordered samples", {
    x <- c(0, 1, 3, 2, 4, 5)
    expect_warning(
        detect_irregular_samples(x, "time"),
        "duplicated or irregular samples"
    )
    expect_warning(
        detect_irregular_samples(x, "time"),
        "time.*=.*3"
    )
})

test_that("detect_irregular_samples detects large gaps (>= 3600)", {
    x <- c(0, 1, 2, 3602, 3603)
    expect_warning(
        detect_irregular_samples(x, "time"),
        "duplicated or irregular samples"
    )
    expect_warning(
        detect_irregular_samples(x, "time"),
        "time.*=.*2"
    )
})

test_that("detect_irregular_samples shows first 3 when > 5 irregularities", {
    x <- c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)
    expect_warning(
        detect_irregular_samples(x, "time"),
        "and 3 more"
    )
})

test_that("detect_irregular_samples shows all when <= 5 irregularities", {
    x <- c(0, 1, 1, 2, 2, 3)
    w <- expect_warning(detect_irregular_samples(x, "time"))
    expect_false(grepl("more", w$message))
})

test_that("detect_irregular_samples respects verbose = FALSE", {
    x <- c(0, 1, 1, 2, 2)
    expect_invisible(detect_irregular_samples(x, "time", verbose = FALSE))
    expect_no_warning(detect_irregular_samples(x, "time", verbose = FALSE))
})

test_that("detect_irregular_samples handles multiple irregularity types", {
    x <- c(0, 1, 1, 3, 2, 3606)
    expect_warning(
        detect_irregular_samples(x, "time"),
        "duplicated or irregular samples"
    )
})

test_that("detect_irregular_samples uses correct time_channel name", {
    x <- c(0, 1, 1, 2)
    expect_warning(
        detect_irregular_samples(x, "my_time_col"),
        "my_time_col"
    )
})

## read_mnirs() =======================================================
## moxy ===============================================================
test_that("read_mnirs moxy .xlsx works with timestamp", {
    expect_warning(
        df <- read_mnirs(
            file_path = example_mnirs("moxy_ramp.xlsx"),
            nirs_channels = c(smo2_left = "SmO2 Live",
                              smo2_right = "SmO2 Live(2)"),
            time_channel = c(time = "hh:mm:ss"),
            add_timestamp = TRUE,
            keep_all = FALSE,
            verbose = TRUE),
        "duplicated or irregular") |>
        expect_message("Estimated.*sample_rate.*2") |>
        expect_message("Overwrite")

    expect_s3_class(df, "mnirs")
    expect_s3_class(df, "data.frame")
    expect_true(all(
        c("time", "timestamp", "smo2_left", "smo2_right") %in% names(df)
    ))
    expect_equal(df$time[1], 0)
    expect_equal(class(df$time), "numeric")
    expect_true(any(class(df$timestamp) %in% "POSIXct"))

    ## check that time diffs should be 0 < Δ < 1 with proper POSIXct import
    expect_gt(sum(diff(head(df$time, 100)) < 1 & diff(head(df$time, 100)) > 0), 0)
    expect_lt(sum(diff(head(df$time, 100)) %in% c(0, 1)), 99)

    expect_true(all(
        c("nirs_device", "nirs_channels", "time_channel",
          "sample_rate", "verbose") %in%
            names(attributes(df))))

    expect_equal(attr(df, "nirs_device"), "Moxy")
    expect_equal(attr(df, "sample_rate"), 2)
    expect_true(attr(df, "verbose"))
})

test_that("read_mnirs moxy .csv works converting time to numeric", {
    expect_message(
        df <- read_mnirs(
        file_path = example_mnirs("moxy_intervals"),
        nirs_channels = c(smo2_left = "SmO2 Live",
                          thb = "THb"),
        time_channel = c(time = "hh:mm:ss"),
        add_timestamp = FALSE,
        verbose = TRUE),
        "Estimated.*sample_rate.*0.5") |>
        expect_message("Overwrite")

    expect_equal(class(df$time), "numeric")
    expect_false(class(df$time) %in% "POSIXct")
    expect_false(c("timestamp") %in% names(df))

    ## check that time diffs should be Δ >= 2 with proper POSIXct import
    expect_gt(sum(diff(head(df$time, 100)) >= 2), 0)

    expect_true(all(
        c("nirs_device", "nirs_channels", "time_channel",
          "sample_rate", "verbose") %in%
            names(attributes(df))))

    expect_equal(attr(df, "nirs_device"), "Moxy")
    expect_equal(attr(df, "sample_rate"), 0.5)
    expect_true(attr(df, "verbose"))
})


test_that("read_mnirs moxy invalid channel names", {
    file_path <- example_mnirs("moxy_ramp.xlsx")

    ## invalid channel names
    expect_error(
        read_mnirs(
            file_path = file_path,
            nirs_channels = c(""),
            time_channel = c(time = "hh:mm:ss"),
            verbose = FALSE),
        "not detected")

    expect_error(
        read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2_left = "smo2_doesnt_exist"),
            time_channel = c(time = "hh:mm:ss"),
            verbose = FALSE),
        "not detected")

    expect_message(
        df <- read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2_left = "SmO2 Live",
                              smo2_right = "SmO2 Live(2)"),
            time_channel = NULL,
            verbose = TRUE),
        "Detected.*time_channel") |>
        expect_message("Overwrite") |>
        expect_message("Estimated.*sample_rate.*2") |>
        expect_message("Overwrite") |>
        expect_warning("duplicated or irregular")

    expect_equal(attr(df, "time_channel"), "hh:mm:ss")

    ## duplicate input names are renamed
    expect_warning(
        df <- read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2 = "SmO2 Live",
                              smo2 = "SmO2 Live(2)"),
            time_channel = c(time = "hh:mm:ss"),
            verbose = TRUE),
        "Duplicated.*renamed") |>
        expect_warning("duplicated or irregular") |>
        expect_message("Estimated.*sample_rate.*2") |>
        expect_message("Overwrite")

    expect_true(all(c("smo2", "smo2_1") %in% names(df)))
})



## train.red ========================================================
test_that("read_mnirs train.red works", {
    file_path <- example_mnirs("train.red_intervals.csv")

    expect_no_message(
        read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2_left = "SmO2 unfiltered",
                              smo2_right = "SmO2 unfiltered"),
            time_channel = c(time = "Timestamp (seconds passed)"),
            verbose = FALSE)
    )

    expect_warning(
        df <- read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2_left = "SmO2 unfiltered",
                              smo2_right = "SmO2 unfiltered"),
            time_channel = c(time = "Timestamp (seconds passed)"),
            verbose = TRUE),
        "duplicated or irregular") |>
        expect_message("Estimated.*sample_rate.*10") |>
        expect_message("Overwrite")

    expect_s3_class(df, "mnirs")
    expect_s3_class(df, "data.frame")
    expect_true(all(
        c("time", "smo2_left", "smo2_right") %in% names(df)
    ))
    expect_equal(class(df$time), "numeric")
    expect_gte(df$time[1], 0)

    ## check that time diffs should be 0 < Δ < 1 with proper POSIXct import
    expect_gt(sum(diff(head(df$time, 100)) < 1 & diff(head(df$time, 100)) > 0), 0)
    expect_lt(sum(diff(head(df$time, 100)) %in% c(0, 1)), 99)

    expect_true(all(
        c("nirs_device", "nirs_channels", "time_channel",
          "sample_rate", "verbose") %in%
            names(attributes(df))))

    expect_equal(attr(df, "nirs_device"), "Train.Red")
    expect_equal(attr(df, "sample_rate"), 10)
    expect_true(attr(df, "verbose"))
})

test_that("read_mnirs train.red works with zero_time", {
    file_path <- example_mnirs("train.red_intervals.csv")

    expect_equal(
        read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2_left = "SmO2 unfiltered",
                              smo2_right = "SmO2 unfiltered"),
            time_channel = c(time = "Timestamp (seconds passed)"),
            zero_time = TRUE,
            verbose = FALSE)$time[1],
        0)
})

test_that("read_mnirs train.red invalid channel names", {
    file_path <- example_mnirs("train.red_intervals.csv")

    ## invalid channel names
    expect_error(
        read_mnirs(
            file_path = file_path,
            nirs_channels = c(""),
            time_channel = c(time = "Timestamp (seconds passed)"),
            verbose = FALSE),
        "not detected")

    expect_error(
        read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2_left = "smo2_doesnt_exist"),
            time_channel = c(time = "Timestamp (seconds passed)"),
            verbose = FALSE),
        "not detected")

    expect_message(
        df <- read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2_left = "SmO2 unfiltered",
                              smo2_right = "SmO2 unfiltered"),
            time_channel = NULL,
            verbose = TRUE),
        "Detected.*time_channel") |>
        expect_message("Overwrite") |>
        expect_message("Estimated.*sample_rate.*10") |>
        expect_message("Overwrite") |>
        expect_warning("duplicated or irregular")

    expect_equal(attr(df, "time_channel"), "Timestamp (seconds passed)")

    ## duplicate input names are renamed
    expect_warning(
        df <- read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2 = "SmO2 unfiltered",
                              smo2 = "SmO2 unfiltered"),
            time_channel = c(time = "Timestamp (seconds passed)"),
            verbose = TRUE),
        "Duplicated.*renamed") |>
        expect_warning("duplicated or irregular") |>
        expect_message("Estimated.*sample_rate.*10") |>
        expect_message("Overwrite")

    expect_true(all(c("smo2", "smo2_1") %in% names(df)))
})


## oxysoft =======================================================
test_that("read_mnirs oxysoft works", {
    file_path <- example_mnirs("artinis_intervals")

    expect_message(
        df <- read_mnirs(
            file_path = file_path,
            nirs_channels = c(HHb = 2,
                              O2Hb = 3),
            time_channel = c(sample = 1),
            verbose = TRUE),
        "Oxysoft detected.*10") |>
        expect_message("time.*channel added") |>
        expect_message("Overwrite")

    expect_s3_class(df, "mnirs")
    expect_s3_class(df, "data.frame")
    expect_true(all(
        c("time", "HHb", "O2Hb") %in% names(df)
    ))
    expect_equal(class(df$time), "numeric")
    expect_gte(df$time[1], 0)
    expect_equal(df$sample[1:10]/10, df$time[1:10])

    expect_true(all.equal(diff(head(df$time, 100)), rep(0.1, 99)))

    expect_true(all(
        c("nirs_device", "nirs_channels", "time_channel",
          "sample_rate", "verbose") %in%
            names(attributes(df))))

    expect_equal(attr(df, "nirs_device"), "Artinis")
    expect_equal(attr(df, "sample_rate"), 10)
    expect_equal(attr(df, "time_channel"), "time")
    expect_true(attr(df, "verbose"))
})

test_that("read_mnirs Oxysoft invalid channel names", {
    file_path <- example_mnirs("artinis_intervals")

    ## invalid channel names
    expect_error(
        read_mnirs(
            file_path = file_path,
            nirs_channels = c(""),
            time_channel = c(sample = 1),
            verbose = FALSE),
        "not detected")

    expect_error(
        read_mnirs(
            file_path = file_path,
            nirs_channels = c(smo2_left = "smo2_doesnt_exist"),
            time_channel = c(sample = 1),
            verbose = FALSE),
        "not detected")

    expect_message(
        df <- read_mnirs(
            file_path = file_path,
            nirs_channels = c(HHb = 2,
                              O2Hb = 3),
            time_channel = NULL,
            verbose = TRUE),
        "detected.*time_channel") |>
        expect_message("Overwrite") |>
        expect_message("detected.*sample_rate.*10") |>
        expect_message("time.*channel added") |>
        expect_message("Overwrite")

    ## detected as "sample" then updated to "time" automatically
    expect_equal(attr(df, "time_channel"), "time")

    ## duplicate input names are renamed
    expect_message(
        df <- read_mnirs(
            file_path = file_path,
            nirs_channels = c(HHb = 2,
                              HHb = 3),
            time_channel = c(sample = 1),
            verbose = TRUE),
        "Oxysoft detected.*10") |>
        expect_message("time.*channel added") |>
        expect_message("Overwrite") |>
        expect_warning("Duplicated.*renamed")

    expect_true(all(c("HHb", "HHb_1") %in% names(df)))
})
