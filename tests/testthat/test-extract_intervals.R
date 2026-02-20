# Mock mnirs data with attributes
create_mock_mnirs <- function(n = 100, sample_rate = 10) {
    df <- tibble::tibble(
        time = seq(0, (n - 1) / sample_rate, length.out = n),
        smo2_left = sin(time * 0.5) * 10 + 50,
        smo2_right = cos(time * 0.5) * 10 + 50,
        event = c(rep("", 10), "start", rep("", n - 21), "end", rep("", 9))
    )
    structure(
        df,
        class = c("mnirs", class(df)),
        nirs_channels = c("smo2_left", "smo2_right"),
        time_channel = "time",
        event_channel = "event",
        sample_rate = sample_rate,
        nirs_device = "test_device"
    )
}

# Mock interval data (as returned by extract_interval_list)
create_mock_interval <- function(
    time_start = 0,
    n = 50,
    sample_rate = 10,
    event_time = 0,
    span = c(-1, 4)
) {
    time_vec <- seq(
        time_start,
        time_start + (n - 1) / sample_rate,
        length.out = n
    )
    df <- tibble::tibble(
        time = time_vec,
        smo2_left = sin(time_vec) * 5 + 50,
        smo2_right = cos(time_vec) * 5 + 50
    )
    structure(
        df,
        class = c("mnirs", class(df)),
        nirs_channels = c("smo2_left", "smo2_right"),
        time_channel = "time",
        event_times = event_time,
        interval_span = span,
        nirs_device = "test_device",
        event_channel = NULL
    )
}

## validate event_times & event_samples error messages ===================
test_that("validate event_times & event_samples errors correctly", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    expect_error(
        extract_intervals(
            data,
            event_times = 999,
            span = c(-1, 1),
            verbose = FALSE
        ),
        "event_times.*valid.*numeric.*range.*time_channel"
    )

    expect_error(
        extract_intervals(
            data,
            event_samples = 999,
            span = c(-1, 1),
            verbose = FALSE
        ),
        "event_samples.*valid.*integer.*nrows.*data"
    )

    expect_error(
        extract_intervals(
            data,
            event_samples = 0,
            span = c(-1, 1),
            verbose = FALSE
        ),
        "event_samples.*valid.*integer.*nrows.*data"
    )
})


## detect_events() ==========================================================
test_that("detect_events finds indices from event_times", {
    time_vec <- seq(0.1, 10, by = 0.1)

    ## event_times
    result <- detect_events(
        time_vec = time_vec,
        event_vec = NULL,
        event_times = c(2, 5, 8),
        event_labels = NULL,
        event_samples = NULL,
        verbose = FALSE
    )

    expect_equal(result, c(20, 50, 80))

    ## event_samples
    result <- detect_events(
        time_vec = time_vec,
        event_vec = NULL,
        event_times = NULL,
        event_labels = NULL,
        event_samples = c(10, 30, 70),
        verbose = FALSE
    )

    expect_equal(result, c(10, 30, 70))

    ## event_labels
    event_vec <- c("start", rep("", 4), "mid", rep("", 4), "end")

    result <- detect_events(
        time_vec = NULL,
        event_vec = event_vec,
        event_times = NULL,
        event_labels = c("start", "mid"),
        event_samples = NULL,
        verbose = FALSE
    )

    expect_equal(result, c(1, 6))
})

test_that("detect_events combines multiple event sources and deduplicates", {
    time_vec <- seq(0, 10, by = 0.1)
    event_vec <- c(rep("", 50), "marker", rep("", 50))

    result <- detect_events(
        time_vec = time_vec,
        event_vec = event_vec,
        event_times = c(2, 5), ## idx = c(21, 51)
        event_labels = "marker", ## idx = 51
        event_samples = c(20, 80), ## idx = c(20, 80)
        verbose = FALSE
    )

    # Should be sorted and unique
    expect_equal(result, c(20, 21, 51, 80))
})

test_that("detect_events errors when no events detected", {
    time_vec <- seq(0, 10, by = 0.1)

    expect_error(
        detect_events(
            time_vec = time_vec,
            event_vec = NULL,
            event_times = NULL,
            event_labels = NULL,
            event_samples = NULL,
            verbose = FALSE
        ),
        "No events detected"
    )

    expect_error(
        detect_events(
            time_vec = NULL,
            event_vec = NULL,
            event_times = NULL,
            event_labels = NULL,
            event_samples = NULL,
            verbose = FALSE
        ),
        "No events detected"
    )

    expect_error(
        detect_events(
            time_vec = NULL,
            event_vec = NULL,
            event_times = NULL,
            event_labels = "invalid",
            event_samples = NULL,
            verbose = FALSE
        ),
        "No events detected"
    )

    event_vec <- c(rep("", 50), "marker", rep("", 50))
    expect_error(
        detect_events(
            time_vec = NULL,
            event_vec = event_vec,
            event_times = NULL,
            event_labels = "invalid",
            event_samples = NULL,
            verbose = FALSE
        ),
        "No events detected"
    )
})

test_that("detect_events warns when event_labels not found", {
    time_vec <- seq(0, 10, by = 1)
    event_vec <- c("start", rep("", 9), "end")

    expect_warning(
        detect_events(
            time_vec = time_vec,
            event_vec = event_vec,
            event_times = 5,
            event_labels = "nonexistent",
            event_samples = NULL,
            verbose = TRUE
        ),
        "No events detected"
    )
})

## recycle_to_length() ==============================================
test_that("recycle_to_length returns unchanged when lengths match", {
    input <- list(c(-1, 1), c(-2, 2), c(-3, 3))
    result <- recycle_to_length(input, n = 3, verbose = FALSE)

    expect_equal(result, input)
})

test_that("recycle_to_length recycles last element when n_param < n", {
    result <- recycle_to_length(
        list(c(-1, 1), c(-2, 2)),
        n = 4,
        verbose = FALSE
    )

    expect_length(result, 4)
    expect_equal(result[[1]], c(-1, 1))
    expect_equal(result[[2]], c(-2, 2))
    expect_equal(result[[3]], c(-2, 2))
    expect_equal(result[[4]], c(-2, 2))
})

test_that("recycle_to_length truncates when n_param > n", {
    result <- recycle_to_length(
        list(c(-1, 1), c(-2, 2), c(-3, 3), c(-4, 4)),
        n = 2,
        verbose = FALSE
    )

    expect_length(result, 2)
    expect_equal(result[[1]], c(-1, 1))
    expect_equal(result[[2]], c(-2, 2))
})

test_that("recycle_to_length handles single element recycling", {
    result <- recycle_to_length(
        list(c(-1, 1)),
        n = 3,
        verbose = FALSE
    )

    expect_length(result, 3)
    expect_equal(result[[1]], c(-1, 1))
    expect_equal(result[[2]], c(-1, 1))
    expect_equal(result[[3]], c(-1, 1))
})

test_that("recycle_to_length messages when recycling with verbose", {
    expect_message(
        recycle_to_length(
            list(c(-1, 1), c(-2, 2), c(-3, 3)),
            n = 1,
            verbose = TRUE
        ),
        regexp = "exceeds.*ignored"
    )

    expect_message(
        recycle_to_length(
            list(c(-1, 1), c(-2, 2)),
            n = 4,
            verbose = TRUE
        ),
        regexp = "recycled.*unspecified"
    )

    ## no message when single param recycled (common case)
    expect_no_message(
        recycle_to_length(
            list(c(-1, 1)),
            n = 4,
            verbose = TRUE
        )
    )
})

## recycle_param() ==============================================
test_that("recycle_param converts non-list to list", {
    result <- recycle_param(
        c(1, 2),
        n_events = 1,
        group_events = "distinct",
        verbose = FALSE
    )

    expect_true(is.list(result))
    expect_equal(result, list(c(1, 2)))
})

test_that("recycle_param returns unchanged when lengths match", {
    input <- list(c(-1, 1), c(-2, 2), c(-3, 3))
    result <- recycle_param(
        input,
        n_events = 3,
        group_events = "distinct",
        verbose = FALSE
    )

    expect_equal(result, input)
})

test_that("recycle_param recycles last element when param_length < n_events", {
    result <- recycle_param(
        list(c(-1, 1), c(-2, 2)),
        n_events = 4,
        group_events = "distinct",
        verbose = FALSE
    )

    expect_length(result, 4)
    expect_equal(result[[3]], c(-2, 2))
    expect_equal(result[[4]], c(-2, 2))
})

test_that("recycle_param truncates when param_length > n_events", {
    result <- recycle_param(
        list(c(-1, 1), c(-2, 2), c(-3, 3), c(-4, 4)),
        n_events = 2,
        group_events = "distinct",
        verbose = FALSE
    )

    expect_length(result, 2)
    expect_equal(result[[1]], c(-1, 1))
    expect_equal(result[[2]], c(-2, 2))
})

test_that("recycle_param flattens nested lists", {
    result <- recycle_param(
        list(list(c(-1, 1)), list(c(-2, 2))),
        n_events = 2,
        group_events = "distinct",
        verbose = FALSE
    )

    expect_equal(result[[1]], c(-1, 1))
    expect_equal(result[[2]], c(-2, 2))
})

test_that("recycle_param warns when recycling & truncating", {
    expect_message(
        recycle_param(
            list(c(-1, 1), c(-2, 2)),
            n_events = 4,
            group_events = "distinct",
            verbose = TRUE
        ),
        regexp = "recycled.*unspecified"
    )

    expect_message(
        recycle_param(
            list(c(-1, 1), c(-2, 2), c(-3, 3)),
            n_events = 1,
            group_events = "distinct",
            verbose = TRUE
        ),
        regexp = "exceeds.*ignored"
    )
})

test_that("recycle_param expands per group and reorders to event order", {
    ## 4 events, 2 groups:  group 1 = events 1,3; group 2 = events 2,4
    ## 2 span values should map:  span1 -> events 1,3; span2 -> events 2,4
    result <- recycle_param(
        list(c(-0.3, 0.3), c(-0.5, 0.5)),
        n_events = 5,
        group_events = list(c(1, 3, 5), c(2, 4)),
        verbose = FALSE
    )

    expect_length(result, 5)
    expect_equal(result[[1]], c(-0.3, 0.3))
    expect_equal(result[[2]], c(-0.5, 0.5))
    expect_equal(result[[3]], c(-0.3, 0.3))
    expect_equal(result[[4]], c(-0.5, 0.5))
    expect_equal(result[[5]], c(-0.3, 0.3))
})

test_that("recycle_param recycles params to match group count", {
    ## 4 events, 3 groups but only 1 param: recycle to all groups
    result <- recycle_param(
        list(c(-1, 1)),
        n_events = 4,
        group_events = list(c(1, 2), c(3), c(4)),
        verbose = FALSE
    )

    expect_length(result, 4)
    expect_equal(result[[1]], c(-1, 1))
    expect_equal(result[[2]], c(-1, 1))
    expect_equal(result[[3]], c(-1, 1))
    expect_equal(result[[4]], c(-1, 1))

    ## 2 params for 3 groups: last param recycled to group 3
    result <- recycle_param(
        list(c(-0.3, 0.3), c(-0.5, 0.5)),
        n_events = 4,
        group_events = list(c(4, 3), c(2), c(1)),
        verbose = FALSE
    )

    expect_length(result, 4)
    expect_equal(result[[4]], c(-0.3, 0.3)) # group 1
    expect_equal(result[[3]], c(-0.3, 0.3)) # group 1
    expect_equal(result[[2]], c(-0.5, 0.5)) # group 2
    expect_equal(result[[1]], c(-0.5, 0.5)) # group 3 (recycled from group 2)
})

test_that("recycle_param handles ungrouped events with custom grouping", {
    ## 5 events but only 4 grouped: event 5 ungrouped
    result <- recycle_param(
        param = list(c(-0.3, 0.3), c(-0.5, 0.5)),
        n_events = 5,
        group_events = list(c(1, 3), c(2, 4)),
        verbose = FALSE
    )

    expect_length(result, 5)
    expect_equal(result[[1]], c(-0.3, 0.3))
    expect_equal(result[[2]], c(-0.5, 0.5))
    expect_equal(result[[3]], c(-0.3, 0.3))
    expect_equal(result[[4]], c(-0.5, 0.5))
    expect_equal(result[[5]], c(-0.5, 0.5)) # ungrouped:  last param recycled
})

## TODO should this produce info message?
test_that("recycle_param truncates when more grouped events than actual events", {
    ## group_events specifies events 1-6 but only 4 actual events
    result <- recycle_param(
        param = list(c(-0.3, 0.3), c(-0.5, 0.5)),
        n_events = 4,
        group_events = list(c(1, 3, 5), c(2, 4, 6)),
        verbose = FALSE
    )

    expect_length(result, 4)
    expect_equal(result[[1]], c(-0.3, 0.3))
    expect_equal(result[[2]], c(-0.5, 0.5))
    expect_equal(result[[3]], c(-0.3, 0.3))
    expect_equal(result[[4]], c(-0.5, 0.5))
})

test_that("recycle_param handles non-contiguous group indices", {
    ## groups reference events 1, 4, 2, 5 (non-sequential)
    result <- recycle_param(
        param = list(c(-0.3, 0.3), c(-0.5, 0.5)),
        n_events = 5,
        group_events = list(c(1, 4), c(2, 5)),
        verbose = FALSE
    )

    expect_length(result, 5)
    expect_equal(result[[1]], c(-0.3, 0.3)) # group 1
    expect_equal(result[[2]], c(-0.5, 0.5)) # group 2
    expect_equal(result[[3]], c(-0.5, 0.5)) # ungrouped (event 3 not in groups)
    expect_equal(result[[4]], c(-0.3, 0.3)) # group 1
    expect_equal(result[[5]], c(-0.5, 0.5)) # group 2
})

test_that("recycle_param truncates excess params for custom grouping", {
    ## 3 params but only 2 groups
    expect_message(
        result <- recycle_param(
            list(c(-0.3, 0.3), c(-0.5, 0.5), c(-1, 1)),
            n_events = 4,
            group_events = list(c(1, 3), c(2, 4)),
            verbose = TRUE
        ),
        regexp = "exceeds"
    )

    expect_length(result, 4)
    expect_equal(result[[1]], c(-0.3, 0.3))
    expect_equal(result[[2]], c(-0.5, 0.5))
    expect_equal(result[[3]], c(-0.3, 0.3))
    expect_equal(result[[4]], c(-0.5, 0.5))
})

test_that("recycle_param messages when recycling groups with verbose", {
    expect_message(
        recycle_param(
            list(c(-1, 1), c(-2, 2), c(-3, 3)),
            n_events = 1,
            group_events = list(1),
            verbose = TRUE
        ),
        regexp = "exceeds.*ignored"
    )

    expect_message(
        recycle_param(
            list(c(-0.3, 0.3), c(-0.5, 0.5)),
            n_events = 6,
            group_events = list(c(1, 2), c(3, 4), c(5, 6)),
            verbose = TRUE
        ),
        regexp = "recycled.*unspecified"
    )

    ## no message when single param recycled (common case)
    expect_no_message(
        recycle_param(
            list(c(-0.3, 0.3)),
            n_events = 6,
            group_events = list(c(1, 2), c(3, 4), c(5, 6)),
            verbose = TRUE
        )
    )
})


## specify_intervals() ================================================
test_that("specify_intervals creates correct interval specification", {
    time_vec <- seq(0, 10, by = 0.1)
    event_indices <- c(20, 50, 80)
    span <- list(c(-1, 1), c(-1, 1), c(-1, 1))

    result <- specify_intervals(
        time_vec = time_vec,
        event_indices = event_indices,
        span = span,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
    expect_equal(result$event_indices, event_indices)
    expect_equal(result$event_times, time_vec[event_indices])
})

test_that("specify_intervals calculates correct start/end indices", {
    time_vec <- seq(0, 10, by = 0.1)
    event_indices <- 51 # time = 5

    result <- specify_intervals(
        time_vec = time_vec,
        event_indices = event_indices,
        span = list(c(-1, 2)),
        verbose = FALSE
    )

    # span = c(-1, 2) around time = 5 means [4, 7]
    expect_equal(result$start_times, 4)
    expect_equal(result$end_times, 7)
    expect_equal(result$start_idx, 41) # index for time = 4
    expect_equal(result$end_idx, 71) # index for time = 7
})

test_that("specify_intervals clips partial out-of-bounds intervals", {
    time_vec <- seq(0, 10, by = 0.1)

    result <- specify_intervals(
        time_vec = time_vec,
        event_indices = 6, # time = 0.5
        span = list(c(-2, 2)), # time = c(-1.5, 2.5)
        verbose = FALSE
    )

    expect_equal(result$start_idx, 1) # clipped to 1

    ## returns warning with verbose = TRUE
    expect_warning(
        result <- specify_intervals(
            time_vec = time_vec,
            event_indices = 6, # time = 0.5
            span = list(c(-2, 10)), # time = c(-1.5, 10.5)
            verbose = TRUE
        ),
        "partially outside"
    )

    expect_equal(result$start_idx, 1) # clipped to 1
    expect_equal(result$end_idx, length(time_vec)) # clipped to 1
})

test_that("specify_intervals errors for entirely out-of-bounds intervals", {
    time_vec <- seq(0, 10, by = 0.1)

    expect_error(
        specify_intervals(
            time_vec = time_vec,
            event_indices = 50,
            span = list(c(100, 200)), # entirely outside
            verbose = FALSE
        ),
        regexp = "Interval.*1.*entirely outside"
    )

    expect_error(
        specify_intervals(
            time_vec = time_vec,
            event_indices = c(50, 100),
            span = list(c(100, 200), c(100, 200)), # entirely outside
            verbose = FALSE
        ),
        regexp = "Intervals.*1.*2.*entirely outside"
    )
})


## extract_interval_list() ==============================================
test_that("extract_interval_list returns correct number of intervals", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    interval_spec <- data.frame(
        start_idx = c(10, 50),
        end_idx = c(30, 70),
        event_times = c(1, 5),
        span_before = c(-1, -1),
        span_after = c(2, 2)
    )

    result <- extract_interval_list(
        data = data,
        interval_spec = interval_spec,
        nirs_channels = list(
            c("smo2_left", "smo2_right"),
            c("smo2_left", "smo2_right")
        )
    )

    expect_length(result, 2)
    expect_named(result, c("interval_1", "interval_2"))
})

test_that("extract_interval_list extracts correct row ranges", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    interval_spec <- data.frame(
        start_idx = 20,
        end_idx = 40,
        event_times = 3,
        span_before = -1,
        span_after = 1
    )

    result <- extract_interval_list(
        data = data,
        interval_spec = interval_spec,
        nirs_channels = list(c("smo2_left", "smo2_right"))
    )

    expect_equal(nrow(result[[1L]]), 21) # rows 20 to 40 inclusive
})

test_that("extract_interval_list preserves metadata attributes", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    interval_spec <- data.frame(
        start_idx = 10,
        end_idx = 30,
        event_times = 1.5,
        span_before = -0.5,
        span_after = 1.5
    )

    result <- extract_interval_list(
        data = data,
        interval_spec = interval_spec,
        nirs_channels = list(c("smo2_left"))
    )

    expect_equal(attr(result[[1L]], "event_times"), 1.5)
    expect_equal(attr(result[[1L]], "interval_span"), c(-0.5, 1.5))
    expect_equal(attr(result[[1L]], "nirs_channels"), "smo2_left")
})


## zero_offset_data() ===============================================
test_that("zero_offset_data shifts time channel by event time", {
    df <- tibble::tibble(time = c(5, 6, 7, 8, 9), value = 1:5)
    result <- zero_offset_data(df, time_channel = "time", t0 = 7)
    expect_equal(result$time, c(-2, -1, 0, 1, 2))
    expect_equal(result$value, 1:5) # other columns unchanged

    ## negative event times
    df <- tibble::tibble(time = c(-5, -4, -3, -2, -1), value = 1:5)
    result <- zero_offset_data(df, time_channel = "time", t0 = -3)
    expect_equal(result$time, c(-2, -1, 0, 1, 2))
})


## ensemble_intervals() ====================================================
test_that("ensemble_intervals averages across intervals correctly", {
    # Create two intervals with known values
    interval1 <- create_mock_interval(time_start = 10, n = 11, event_time = 10)
    interval1$smo2_left <- rep(40, 11)
    interval1$smo2_right <- rep(60, 11)

    interval2 <- create_mock_interval(time_start = 20, n = 11, event_time = 20)
    interval2$smo2_left <- rep(60, 11)
    interval2$smo2_right <- rep(40, 11)

    interval_list <- list(interval_1 = interval1, interval_2 = interval2)
    metadata <- list(time_channel = "time", sample_rate = 10)

    result <- ensemble_intervals(
        interval_list = interval_list,
        nirs_channels = c("smo2_left", "smo2_right"),
        metadata = metadata,
        verbose = FALSE
    )

    # Ensemble mean of 40 and 60 should be 50
    expect_true(all(abs(result$smo2_left - 50) < 1e-10))
    expect_true(all(abs(result$smo2_right - 50) < 1e-10))

    # Times should be zero-offset (start at 0)
    expect_equal(min(result$time), 0)
})

test_that("ensemble_intervals preserves metadata", {
    interval1 <- create_mock_interval(time_start = 10, n = 11, event_time = 10)
    interval2 <- create_mock_interval(time_start = 20, n = 11, event_time = 20)
    interval_list <- list(interval_1 = interval1, interval_2 = interval2)
    metadata <- list(time_channel = "time", sample_rate = 10)

    result <- ensemble_intervals(
        interval_list = interval_list,
        nirs_channels = c("smo2_left", "smo2_right"),
        metadata = metadata,
        verbose = FALSE
    )

    expect_equal(attr(result, "time_channel"), "time")
    expect_equal(attr(result, "sample_rate"), 10)
    expect_true(is.list(attr(result, "event_times")))
    expect_setequal(unlist(attr(result, "event_times")), c(10, 20))
    expect_true(is.list(attr(result, "interval_span")))
    expect_setequal(lengths(attr(result, "interval_span")), 2)
})

test_that("ensemble_intervals warns on irregular samples with verbose", {
    interval1 <- create_mock_interval(time_start = 0, n = 5, event_time = 0)
    interval2 <- create_mock_interval(
        time_start = 0.05,
        n = 5,
        event_time = 0.05
    )
    interval2[2, ] <- NA
    interval_list <- list(interval_1 = interval1, interval_2 = interval2)
    metadata <- list(time_channel = "time", sample_rate = 10)

    expect_warning(
        ensemble_intervals(
            interval_list = interval_list,
            nirs_channels = c("smo2_left", "smo2_right"),
            metadata = metadata,
            verbose = TRUE
        ),
        regexp = "irregular.*samples"
    )
})

test_that("ensemble_intervals returns the right number of dims", {
    interval1 <- create_mock_interval(time_start = 0, n = 5, event_time = 0)
    interval2 <- create_mock_interval(
        time_start = 0.05,
        n = 5,
        event_time = 0.05
    )
    interval_list <- list(interval_1 = interval1, interval_2 = interval2)
    metadata <- list(time_channel = "time", sample_rate = 10)
    nirs_channels = c("smo2_left")
    
    result <- ensemble_intervals(
        interval_list = interval_list,
        nirs_channels = nirs_channels,
        metadata = metadata,
        verbose = FALSE
    )
    
    expect_equal(ncol(result), length(nirs_channels) + 1)

    nirs_channels = c("smo2_left", "smo2_right")
    
    result <- ensemble_intervals(
        interval_list = interval_list,
        nirs_channels = nirs_channels,
        metadata = metadata,
        verbose = FALSE
    )
    
    expect_equal(ncol(result), length(nirs_channels) + 1)
})


## group_intervals() ==================================================
test_that("group_intervals returns distinct intervals unchanged", {
    interval1 <- create_mock_interval(time_start = 0, n = 11, event_time = 0)
    interval2 <- create_mock_interval(time_start = 10, n = 11, event_time = 10)
    interval_list <- list(interval_1 = interval1, interval_2 = interval2)
    metadata <- list(time_channel = "time", sample_rate = 10)

    result <- group_intervals(
        interval_list = interval_list,
        nirs_channels = list(
            c("smo2_left", "smo2_right"),
            c("smo2_left", "smo2_right")
        ),
        metadata = metadata,
        group_events = "distinct",
        zero_time = TRUE,
        verbose = FALSE
    )

    expect_length(result, 2)
    expect_named(result, c("interval_1", "interval_2"))
    ## zero offset explicitly for "distinct"
    expect_equal(min(result[[1]]$time), 0)
})

test_that("group_intervals ensembles all intervals with 'ensemble'", {
    interval1 <- create_mock_interval(time_start = 0, n = 11, event_time = 0)
    interval2 <- create_mock_interval(time_start = 10, n = 11, event_time = 10)
    interval_list <- list(interval_1 = interval1, interval_2 = interval2)
    metadata <- list(time_channel = "time", sample_rate = 10)

    result <- group_intervals(
        interval_list = interval_list,
        nirs_channels = list(
            c("smo2_left", "smo2_right"),
            c("smo2_left", "smo2_right")
        ),
        metadata = metadata,
        group_events = "ensemble",
        zero_time = FALSE,
        verbose = FALSE
    )

    expect_length(result, 1)
    expect_named(result, "ensemble")
    ## zero offset regardless for "ensemble"
    expect_equal(min(result[[1]]$time), 0)
})

test_that("group_intervals handles custom grouping", {
    interval1 <- create_mock_interval(time_start = 0, n = 11, event_time = 0)
    interval2 <- create_mock_interval(time_start = 10, n = 11, event_time = 10)
    interval3 <- create_mock_interval(time_start = 20, n = 11, event_time = 20)
    interval4 <- create_mock_interval(time_start = 30, n = 11, event_time = 30)
    interval_list <- list(
        interval_1 = interval1,
        interval_2 = interval2,
        interval_3 = interval3,
        interval_4 = interval4
    )
    metadata <- list(time_channel = "time", sample_rate = 10)

    result <- group_intervals(
        interval_list = interval_list,
        nirs_channels = rep(list(c("smo2_left", "smo2_right")), 4),
        metadata = metadata,
        group_events = list(c(1, 2), c(3, 4)),
        zero_time = TRUE,
        verbose = FALSE
    )

    expect_length(result, 2)
    expect_named(result, c("interval_1_2", "interval_3_4"))

    # Group only intervals 1, 2 & 3, leaving 4 ungrouped
    expect_message(
        result <- group_intervals(
            interval_list = interval_list,
            nirs_channels = rep(list(c("smo2_left", "smo2_right")), 3),
            metadata = metadata,
            group_events = list(c(1, 2), 4),
            zero_time = TRUE,
            verbose = TRUE
        ),
        "Ungrouped.*discrete"
    )

    expect_length(result, 3)
    expect_named(result, c("interval_1_2", "interval_3", "interval_4"))

    ## group the same interval multiple times throws warning
    expect_warning(
        result <- group_intervals(
            interval_list = interval_list,
            nirs_channels = rep(list(c("smo2_left", "smo2_right")), 3),
            metadata = metadata,
            group_events = list(c(1, 2, 3), c(2, 4)),
            zero_time = TRUE,
            verbose = TRUE
        ),
        "Duplicates detected"
    )
})

test_that("group_intervals returns single interval as distinct regardless", {
    interval1 <- create_mock_interval(time_start = 0, n = 11, event_time = 0)
    interval_list <- list(interval_1 = interval1)
    metadata <- list(time_channel = "time", sample_rate = 10)

    result <- group_intervals(
        interval_list = interval_list,
        nirs_channels = list(c("smo2_left", "smo2_right")),
        metadata = metadata,
        group_events = "ensemble", # request ensemble but only 1 interval
        zero_time = FALSE,
        verbose = FALSE
    )

    expect_length(result, 1)
})


## extract_intervals() ===================================================
test_that("extract_intervals returns list of tibbles", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        event_times = c(2, 5),
        span = c(-1, 1),
        group_events = "distinct",
        verbose = FALSE
    )

    expect_type(result, "list")
    expect_true(all(vapply(result, tibble::is_tibble, logical(1))))
})

test_that("extract_intervals works with event_samples", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        event_samples = c(20, 50),
        span = c(-1, 1),
        group_events = "distinct",
        verbose = FALSE
    )

    expect_length(result, 2)
})

test_that("extract_intervals works with event_labels", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    data$event[50] <- "marker"

    result <- extract_intervals(
        data = data,
        event_channel = "event",
        event_labels = "marker",
        span = c(-1, 1),
        group_events = "distinct",
        verbose = FALSE
    )

    expect_length(result, 1)
})

test_that("extract_intervals combines multiple event specification methods", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    data$event[30] <- "marker"

    result <- extract_intervals(
        data = data,
        event_channel = "event",
        event_times = 5,
        event_labels = "marker",
        event_samples = 80,
        span = c(-0.5, 0.5),
        group_events = "distinct",
        verbose = FALSE
    )

    expect_length(result, 3)
})

test_that("extract_intervals applies zero_time correctly", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        event_times = 5,
        span = c(-1, 1),
        group_events = "distinct",
        zero_time = TRUE,
        verbose = FALSE
    )

    # Time should start at -1 (span before) after zero offset
    expect_equal(min(result[[1]]$time), -1, tolerance = 0.1)
})

test_that("extract_intervals handles grouping", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        event_times = c(2, 5, 8),
        span = c(-0.5, 0.5), ## single span recycled to all events
        group_events = "ensemble",
        verbose = FALSE
    )

    expect_length(result, 1)
    expect_named(result, "ensemble")
    ## check interval span
    expect_setequal(range(result$ensemble$time), c(-0.5, 0.5))

    result <- extract_intervals(
        data = data,
        event_times = c(2, 4, 6, 8),
        span = list(c(-0.3, 0.3), c(-0.5, 0.5)),
        group_events = list(c(1, 3), c(2, 4)),
        verbose = FALSE
    )

    expect_length(result, 2)
    expect_named(result, c("interval_1_3", "interval_2_4"))
    expect_setequal(range(result$interval_1_3$time), c(-0.3, 0.3))
    expect_setequal(range(result$interval_2_4$time), c(-0.5, 0.5))
})

test_that("extract_intervals handles different spans per event", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        event_times = c(2, 5),
        span = list(c(-0.5, 0.5), c(-1, 1)),
        group_events = "distinct",
        verbose = FALSE
    )

    expect_length(result, 2)
    # Second interval should be larger due to wider span
    expect_true(nrow(result[[2]]) > nrow(result[[1]]))
})

test_that("extract_intervals errors & messages", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    ## no events specified
    expect_error(
        extract_intervals(
            data = data,
            span = c(-1, 1),
            verbose = FALSE
        ),
        regexp = "No events detected"
    )

    ## edge case: event at data boundary
    expect_warning(
        result <- extract_intervals(
            data = data,
            event_times = 0.5,
            span = c(-1, 1),
            group_events = "distinct",
            verbose = TRUE
        ),
        regexp = "partially outside"
    ) |>
        expect_message("nirs_channels.*grouped together")

    expect_length(result, 1)
    ## start value bounded by time = zero
    expect_setequal(range(result$interval_1$time), c(0, 1.5))
})

test_that("extract_intervals respects nirs_channels metadata", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    all_channels <- attr(data, "nirs_channels")

    result <- extract_intervals(
        data = data,
        nirs_channels = "smo2_left",
        event_times = c(1, 5),
        span = c(-1, 1),
        group_events = "distinct",
        verbose = FALSE
    )

    ## TODO 2026-02-15 changed to overwrite nirs_channels
    # expect_equal(attr(result[[1]], "nirs_channels"), all_channels)
    # expect_equal(attr(result[[2]], "nirs_channels"), all_channels)
    expect_equal(attr(result[[1]], "nirs_channels"), "smo2_left")
    expect_equal(attr(result[[2]], "nirs_channels"), "smo2_left")

    result <- extract_intervals(
        data = data,
        nirs_channels = "smo2_left",
        time_channel = "time",
        event_times = c(1, 5),
        span = c(-1, 1),
        group_events = "ensemble",
        verbose = TRUE
    )

    # expect_equal(attr(result[[1]], "nirs_channels"), all_channels)
    expect_equal(attr(result[[1]], "nirs_channels"), "smo2_left")
})


## integration tests ===================================

test_that("extract_intervals works on Moxy data", {
    data <- read_mnirs(
        example_mnirs("moxy_ramp"),
        nirs_channels = c(smo2_left = "SmO2 Live", smo2_right = "SmO2 Live(2)"),
        keep_all = FALSE,
        verbose = FALSE
    )

    result <- extract_intervals(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        event_times = 870,
        span = list(c(-30, 180)),
        zero_time = FALSE,
        verbose = FALSE
    )

    ## structure
    expect_length(result, 1)
    expect_length(result[[1L]], 3)
    expect_named(result[[1L]], c("hh:mm:ss", "smo2_left", "smo2_right"))
    ## range of time_channel
    expect_gte(min(result[[1L]][[1]]), 870 - 30)
    expect_equal(min(result[[1L]][[1]]), 870 - 30, tolerance = 1)
    expect_lte(max(result[[1L]][[1]]), 870 + 180)
    expect_equal(max(result[[1L]][[1]]), 870 + 180, tolerance = 1)
    ## equivalent to intake df
    expect_equal(
        result[[1L]],
        data[within(data$`hh:mm:ss`, c(870 - 30, 870 + 180)), ],
        ignore_attr = TRUE
    )
})

test_that("extract_intervals works on train.red data", {
    data <- read_mnirs(
        example_mnirs("train.red"),
        nirs_channels = c(
            smo2_left = "SmO2 unfiltered",
            smo2_right = "SmO2 unfiltered"
        ),
        time_channel = c(time = "Timestamp (seconds passed)"),
        keep_all = FALSE,
        verbose = FALSE
    ) |>
        resample_mnirs(verbose = FALSE)

    result <- extract_intervals(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        event_times = c(2455, 3166),
        span = list(c(-30, 180)),
        group_events = "ensemble",
        zero_time = FALSE,
        verbose = FALSE
    )

    ## visual check
    # plot(result[[1L]])

    ## structure
    expect_length(result, 1)
    expect_length(result[[1L]], 3)
    expect_named(result[[1L]], c("time", "smo2_left", "smo2_right"))
    ## range of time_channel
    expect_gte(min(result[[1L]][[1]]), -30)
    expect_equal(min(result[[1L]][[1]]), -30, tolerance = 0.1)
    expect_lte(max(result[[1L]][[1]]), 180)
    expect_equal(max(result[[1L]][[1]]), 180, tolerance = 0.1)

    result <- extract_intervals(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        event_times = c(2455, 3166),
        span = list(c(-30, 180)),
        group_events = "distinct",
        zero_time = FALSE,
        verbose = FALSE
    )

    ## structure
    expect_length(result, 2)
    expect_length(result[[1L]], 3)
    expect_length(result[[2L]], 3)
    expect_named(result[[1L]], c("time", "smo2_left", "smo2_right"))
    expect_named(result[[2L]], c("time", "smo2_left", "smo2_right"))
    ## range of time_channel
    expect_lte(min(result[[1L]][[1]]), 2455 - 30)
    expect_equal(min(result[[1L]][[1]]), 2455 - 30, tolerance = 0.1)
    expect_lte(max(result[[1L]][[1]]), 2455 + 180)
    expect_equal(max(result[[1L]][[1]]), 2455 + 180, tolerance = 0.1)
    expect_lte(min(result[[2L]][[1]]), 3166 - 30)
    expect_equal(min(result[[2L]][[1]]), 3166 - 30, tolerance = 0.1)
    expect_lte(max(result[[2L]][[1]]), 3166 + 180)
    expect_equal(max(result[[2L]][[1]]), 3166 + 180, tolerance = 0.1)
})
