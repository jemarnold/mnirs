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
        interval_times = event_time,
        interval_span = span,
        nirs_device = "test_device",
        event_channel = NULL
    )
}

## by_time(), by_sample(), by_label() constructors =======================
test_that("by_time creates mnirs_interval with correct structure", {
    result <- by_time(2, 5, 8)
    expect_s3_class(result, "mnirs_interval")
    expect_equal(result$type, "time")
    expect_equal(result$by_time, c(2, 5, 8))
})

test_that("by_sample creates mnirs_interval with correct structure", {
    result <- by_sample(10, 30, 70)
    expect_s3_class(result, "mnirs_interval")
    expect_equal(result$type, "sample")
    expect_equal(result$by_sample, c(10L, 30L, 70L))
})

test_that("by_sample validates input", {
    expect_error(by_sample(0), "valid.*integer")
    expect_error(by_sample(-1), "valid.*integer")
    expect_error(by_sample(1.5), "valid.*integer")
})

test_that("by_label creates mnirs_interval with correct structure", {
    result <- by_label("start", "end")
    expect_s3_class(result, "mnirs_interval")
    expect_equal(result$type, "label")
    expect_equal(result$by_label, c("start", "end"))
})

test_that("by_label validates input", {
    expect_error(by_label(123), "valid.*character")
})

test_that("by_lap creates mnirs_interval with correct structure", {
    result <- by_lap(1, 3, 5)
    expect_s3_class(result, "mnirs_interval")
    expect_equal(result$type, "lap")
    expect_equal(result$by_lap, c(1L, 3L, 5L))
})

test_that("by_lap validates input", {
    expect_error(by_lap(0), "valid.*integer")
    expect_error(by_lap(-1), "valid.*integer")
    expect_error(by_lap(1.5), "valid.*integer")
})


## as_mnirs_interval() =====================================================
test_that("as_mnirs_interval passes through NULL", {
    expect_null(as_mnirs_interval(NULL))
})

test_that("as_mnirs_interval passes through mnirs_interval", {
    interval <- by_time(5)
    result <- as_mnirs_interval(interval)
    expect_identical(result, interval)
})

test_that("as_mnirs_interval coerces numeric to by_time", {
    result <- as_mnirs_interval(c(2, 5, 8))
    expect_s3_class(result, "mnirs_interval")
    expect_equal(result$type, "time")
    expect_equal(result$by_time, c(2, 5, 8))
})

test_that("as_mnirs_interval coerces character to by_label", {
    result <- as_mnirs_interval(c("start", "end"))
    expect_s3_class(result, "mnirs_interval")
    expect_equal(result$type, "label")
    expect_equal(result$by_label, c("start", "end"))
})

test_that("as_mnirs_interval coerces integer to by_lap", {
    result <- as_mnirs_interval(c(1L, 3L))
    expect_s3_class(result, "mnirs_interval")
    expect_equal(result$type, "lap")
    expect_equal(result$by_lap, c(1L, 3L))
})

test_that("as_mnirs_interval errors on unsupported type", {
    expect_error(as_mnirs_interval(TRUE, "start"), "start.*must be")
    expect_error(as_mnirs_interval(list(1), "end"), "end.*must be")
})


## recycle_span() =========================================================
test_that("recycle_span works", {
    ## recycle_span expands positive scalar to c(0, x)
    expect_equal(recycle_span(60), c(0, 60))
    ## recycle_span expands negative scalar to c(x, 0)
    expect_equal(recycle_span(-60), c(-60, 0))
    ## recycle_span treats zero as positive
    expect_equal(recycle_span(0), c(0, 0))
    ## recycle_span passes through two-element vector
    expect_equal(recycle_span(c(-5, 10)), c(-5, 10))
})

test_that("recycle_span validates span", {
    ## length > 2 errors
    expect_error(recycle_span(c(1, 2, 3)), "span.*must be")
    ## length 0 errors
    expect_error(recycle_span(numeric(0)), "span.*must be")
    ## non-numeric errors
    expect_error(recycle_span("a"), "span.*must be")
})


## resolve_interval_indices() =======================================================
test_that("resolve_interval_indices resolves time to correct indices", {
    time_vec <- seq(0.1, 10, by = 0.1)

    result <- resolve_interval_indices(by_time(2, 5, 8), time_vec)
    expect_equal(result, c(20L, 50L, 80L))
})

test_that("resolve_interval_indices resolves sample indices directly", {
    time_vec <- seq(0.1, 10, by = 0.1)

    result <- resolve_interval_indices(by_sample(10, 30, 70), time_vec)
    expect_equal(result, c(10L, 30L, 70L))
})

test_that("resolve_interval_indices resolves labels to matching indices", {
    time_vec <- seq(0.1, 10, by = 0.1)
    event_vec <- c("start", rep("", 4), "mid", rep("", 4), "end")

    result <- resolve_interval_indices(
        by_label("start", "mid"),
        time_vec,
        event_vec
    )
    expect_equal(result, c(1L, 6L))
})

test_that("resolve_interval_indices errors when no labels match", {
    event_vec <- c(rep("", 50), "marker", rep("", 50))

    expect_error(
        resolve_interval_indices(
            by_label("invalid"),
            time_vec = NULL,
            event_vec
        ),
        "No events detected"
    )
})

test_that("resolve_interval_indices resolves laps with position = first", {
    event_vec <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L)

    result <- resolve_interval_indices(
        by_lap(1, 3),
        time_vec = NULL,
        event_vec,
        position = "first"
    )
    expect_equal(result, c(1L, 7L))
})

test_that("resolve_interval_indices resolves laps with position = last", {
    event_vec <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L)

    result <- resolve_interval_indices(
        by_lap(1, 3),
        time_vec = NULL,
        event_vec,
        position = "last"
    )
    expect_equal(result, c(3L, 9L))
})

test_that("resolve_interval_indices errors when lap not found", {
    event_vec <- c(1L, 1L, 2L, 2L)

    expect_error(
        resolve_interval_indices(
            by_lap(5),
            time_vec = NULL,
            event_vec,
            position = "first"
        ),
        "No samples found for lap"
    )
})

## resolve_interval() ===============================================
test_that("resolve_interval returns start-only indices", {
    time_vec <- seq(0, 10, by = 0.1)

    result <- resolve_interval(
        start_interval = by_time(2, 5),
        end_interval = NULL,
        time_vec = time_vec
    )

    expect_true(result$has_start)
    expect_false(result$has_end)
    expect_equal(length(result$start_idx), 2)
    expect_null(result$end_idx)
})

test_that("resolve_interval returns paired start+end indices", {
    time_vec <- seq(0, 10, by = 0.1)

    result <- resolve_interval(
        start_interval = by_time(2, 5),
        end_interval = by_time(4, 8),
        time_vec = time_vec
    )

    expect_true(result$has_start)
    expect_true(result$has_end)
    expect_equal(length(result$start_idx), 2)
    expect_equal(length(result$end_idx), 2)
})

test_that("resolve_interval warns and truncates unequal lengths", {
    time_vec <- seq(0, 10, by = 0.1)

    expect_warning(
        result <- resolve_interval(
            start_interval = by_time(2, 5, 8),
            end_interval = by_time(4, 7),
            time_vec = time_vec
        ),
        "unequal lengths"
    )

    ## truncated to 2 paired intervals
    expect_equal(length(result$start_idx), 2)
    expect_equal(length(result$end_idx), 2)
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
        event_groups = "distinct",
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
        event_groups = "distinct",
        verbose = FALSE
    )

    expect_equal(result, input)
})

test_that("recycle_param recycles last element when param_length < n_events", {
    result <- recycle_param(
        list(c(-1, 1), c(-2, 2)),
        n_events = 4,
        event_groups = "distinct",
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
        event_groups = "distinct",
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
        event_groups = "distinct",
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
            event_groups = "distinct",
            verbose = TRUE
        ),
        regexp = "recycled.*unspecified"
    )

    expect_message(
        recycle_param(
            list(c(-1, 1), c(-2, 2), c(-3, 3)),
            n_events = 1,
            event_groups = "distinct",
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
        event_groups = list(c(1, 3, 5), c(2, 4)),
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
        event_groups = list(c(1, 2), c(3), c(4)),
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
        event_groups = list(c(4, 3), c(2), c(1)),
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
        event_groups = list(c(1, 3), c(2, 4)),
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
    ## event_groups specifies events 1-6 but only 4 actual events
    result <- recycle_param(
        param = list(c(-0.3, 0.3), c(-0.5, 0.5)),
        n_events = 4,
        event_groups = list(c(1, 3, 5), c(2, 4, 6)),
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
        event_groups = list(c(1, 4), c(2, 5)),
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
            event_groups = list(c(1, 3), c(2, 4)),
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
            event_groups = list(1),
            verbose = TRUE
        ),
        regexp = "exceeds.*ignored"
    )

    expect_message(
        recycle_param(
            list(c(-0.3, 0.3), c(-0.5, 0.5)),
            n_events = 6,
            event_groups = list(c(1, 2), c(3, 4), c(5, 6)),
            verbose = TRUE
        ),
        regexp = "recycled.*unspecified"
    )

    ## no message when single param recycled (common case)
    expect_no_message(
        recycle_param(
            list(c(-0.3, 0.3)),
            n_events = 6,
            event_groups = list(c(1, 2), c(3, 4), c(5, 6)),
            verbose = TRUE
        )
    )
})


## apply_span_to_indices() ================================================
test_that("apply_span_to_indices creates correct interval specification", {
    time_vec <- seq(0, 10, by = 0.1)
    interval_idx <- list(
        start_idx = c(20, 50, 80),
        end_idx = NULL,
        has_start = TRUE,
        has_end = FALSE
    )
    span <- list(c(-1, 1), c(-1, 1), c(-1, 1))

    result <- apply_span_to_indices(
        interval_idx,
        time_vec,
        span,
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
    expect_equal(result$start_idx, c(10, 40, 70))
    expect_equal(result$end_idx, c(30, 60, 90))
    expect_equal(result$interval_times, as.list(time_vec[c(20, 50, 80)]))
})

test_that("apply_span_to_indices creates correct specification with start, end", {
    time_vec <- seq(0, 10, by = 0.1)
    start_idx <- c(10, 40, 70)
    end_idx <- c(30, 60, 90)
    interval_idx <- list(
        start_idx = start_idx,
        end_idx = end_idx,
        has_start = TRUE,
        has_end = TRUE
    )

    result <- apply_span_to_indices(
        interval_idx,
        time_vec,
        span = list(c(0, 1), c(0, 1), c(0, 1)),
        verbose = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
    expect_equal(result$start_idx, c(10, 40, 70))
    expect_equal(result$end_idx, c(40, 70, 100))
    expect_equal(
        result$interval_times,
        Map(c, time_vec[start_idx], time_vec[end_idx])
    )
})

test_that("apply_span_to_indices calculates correct start/end indices", {
    time_vec <- seq(0, 10, by = 0.1)
    interval_idx <- list(
        start_idx = 51L,
        end_idx = NULL,
        has_start = TRUE,
        has_end = FALSE
    )

    result <- apply_span_to_indices(
        interval_idx,
        time_vec,
        span = list(c(-1, 2)),
        verbose = FALSE
    )

    ## span = c(-1, 2) around time = 5 means [4, 7]
    expect_equal(result$start_times, 4)
    expect_equal(result$end_times, 7)
    expect_equal(result$start_idx, 41) # index for time = 4
    expect_equal(result$end_idx, 71) # index for time = 7
})

test_that("apply_span_to_indices clips partial out-of-bounds intervals", {
    time_vec <- seq(0, 10, by = 0.1)
    interval_idx <- list(
        start_idx = 6L,
        end_idx = NULL,
        has_start = TRUE,
        has_end = FALSE
    )

    result <- apply_span_to_indices(
        interval_idx,
        time_vec,
        span = list(c(-2, 2)),
        verbose = FALSE
    )

    expect_equal(result$start_idx, 1) # clipped to 1

    ## returns warning with verbose = TRUE
    expect_warning(
        result <- apply_span_to_indices(
            interval_idx,
            time_vec,
            span = list(c(-2, 10)),
            verbose = TRUE
        ),
        "partially outside"
    )

    expect_equal(result$start_idx, 1) # clipped to 1
    expect_equal(result$end_idx, length(time_vec)) # clipped to max
})

test_that("apply_span_to_indices errors for entirely out-of-bounds", {
    time_vec <- seq(0, 10, by = 0.1)
    interval_idx <- list(
        start_idx = 50L,
        end_idx = NULL,
        has_start = TRUE,
        has_end = FALSE
    )

    expect_error(
        apply_span_to_indices(
            interval_idx,
            time_vec,
            span = list(c(100, 200)),
            verbose = FALSE
        ),
        regexp = "entirely outside"
    )
})

test_that("apply_span_to_indices applies span correctly with start+end", {
    time_vec <- seq(0, 10, by = 0.1)
    interval_idx <- list(
        start_idx = 21L, ## time = 2
        end_idx = 61L, ## time = 6
        has_start = TRUE,
        has_end = TRUE
    )

    ## span[1] shifts start, span[2] shifts end
    result <- apply_span_to_indices(
        interval_idx,
        time_vec,
        span = list(c(-1, 2)),
        verbose = FALSE
    )

    ## start: time 2 + (-1) = 1; end: time 6 + 2 = 8
    expect_equal(result$start_times, 1)
    expect_equal(result$end_times, 8)
})


## extract_interval_list() ==============================================
test_that("extract_interval_list returns correct number of intervals", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    interval_spec <- data.frame(
        start_idx = c(10, 50),
        end_idx = c(30, 70),
        span_before = c(-1, -1),
        span_after = c(2, 2)
    )
    interval_spec$interval_times <- list(1, 5) ## two start_times, no end_times

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
        span_before = -1,
        span_after = 1
    )
    interval_spec$interval_times <- list(3) ## one start_times, no end_times

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
        span_before = -0.5,
        span_after = 1.5
    )
    interval_spec$interval_times <- list(c(1.5, 3.0)) ## one start_times, one end_times

    result <- extract_interval_list(
        data = data,
        interval_spec = interval_spec,
        nirs_channels = list(c("smo2_left"))
    )

    expect_equal(attr(result[[1L]], "interval_times"), c(1.5, 3.0))
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
    expect_true(is.list(attr(result, "interval_times")))
    ## TODO 2026-03-07 update `interval_times` adheres to `zero_time` to represent interval times of returned data frame, not input data frame
    # expect_setequal(unlist(attr(result, "interval_times")), c(10, 20))
    expect_setequal(unlist(attr(result, "interval_times")), c(0, 0))
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

test_that("ensemble_intervals preserves all metadata attributes", {
    interval1 <- create_mock_interval(time_start = 10, n = 11, event_time = 10)
    interval2 <- create_mock_interval(time_start = 20, n = 11, event_time = 20)
    interval_list <- list(interval_1 = interval1, interval_2 = interval2)
    metadata <- list(
        time_channel = "time",
        sample_rate = 10,
        nirs_device = "MockDevice",
        event_channel = "event",
        start_timestamp = as.POSIXct("2024-01-01")
    )

    result <- ensemble_intervals(
        interval_list = interval_list,
        nirs_channels = c("smo2_left", "smo2_right"),
        metadata = metadata,
        verbose = FALSE
    )

    expect_equal(attr(result, "nirs_device"), "MockDevice")
    expect_equal(attr(result, "nirs_channels"), c("smo2_left", "smo2_right"))
    expect_equal(attr(result, "event_channel"), "event")
    expect_equal(
        attr(result, "start_timestamp"),
        as.POSIXct("2024-01-01")
    )
    ## class is preserved
    expect_true(inherits(result, "mnirs"))
})

test_that("ensemble_intervals deduplicates nirs_channels attr", {
    interval1 <- create_mock_interval(time_start = 0, n = 11, event_time = 0)
    interval_list <- list(i1 = interval1, i2 = interval1)
    metadata <- list(time_channel = "time", sample_rate = 10)

    ## duplicated channel name supplied; attr must be unique
    result <- ensemble_intervals(
        interval_list = interval_list,
        nirs_channels = c("smo2_left", "smo2_left"),
        metadata = metadata,
        verbose = FALSE
    )

    expect_equal(attr(result, "nirs_channels"), "smo2_left")
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
        event_groups = "distinct",
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
        event_groups = "ensemble",
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
        event_groups = list(c(1, 2), c(3, 4)),
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
            event_groups = list(c(1, 2), 4),
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
            event_groups = list(c(1, 2, 3), c(2, 4)),
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
        event_groups = "ensemble", # request ensemble but only 1 interval
        zero_time = FALSE,
        verbose = FALSE
    )

    expect_length(result, 1)
})

test_that("group_intervals (distinct) preserves all metadata on each interval", {
    interval1 <- create_mock_interval(time_start = 0, n = 11, event_time = 0)
    interval2 <- create_mock_interval(time_start = 10, n = 11, event_time = 10)
    interval_list <- list(interval_1 = interval1, interval_2 = interval2)
    metadata <- list(
        time_channel = "time",
        sample_rate = 10,
        nirs_device = "MockDevice",
        event_channel = "event",
        start_timestamp = as.POSIXct("2024-01-01")
    )

    result <- group_intervals(
        interval_list = interval_list,
        nirs_channels = list(
            c("smo2_left", "smo2_right"),
            c("smo2_left", "smo2_right")
        ),
        metadata = metadata,
        event_groups = "distinct",
        zero_time = FALSE,
        verbose = FALSE
    )

    for (iv in result) {
        expect_equal(attr(iv, "nirs_device"), "MockDevice")
        expect_equal(attr(iv, "nirs_channels"), c("smo2_left", "smo2_right"))
        expect_equal(attr(iv, "time_channel"), "time")
        expect_equal(attr(iv, "event_channel"), "event")
        expect_equal(attr(iv, "sample_rate"), 10)
        expect_equal(
            attr(iv, "start_timestamp"),
            as.POSIXct("2024-01-01")
        )
    }
    ## interval_times and interval_span forwarded from original interval attrs
    expect_true(inherits(result[[1]], "mnirs"))
    expect_equal(attr(result[[1]], "interval_times"), 0)
    expect_equal(attr(result[[1]], "interval_span"), c(-1, 4))
    expect_true(inherits(result[[2]], "mnirs"))
    expect_equal(attr(result[[2]], "interval_times"), 10)
    expect_equal(attr(result[[2]], "interval_span"), c(-1, 4))
})


test_that("group_intervals custom multi-interval groups preserve metadata", {
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
    metadata <- list(
        time_channel = "time",
        sample_rate = 10,
        nirs_device = "MockDevice",
        event_channel = "event"
    )

    result <- group_intervals(
        interval_list = interval_list,
        nirs_channels = rep(list(c("smo2_left", "smo2_right")), 4),
        metadata = metadata,
        event_groups = list(c(1, 2), c(3, 4)),
        # zero_time = FALSE, ## ensemble auto zeroes
        verbose = FALSE
    )

    for (iv in result) {
        expect_equal(attr(iv, "nirs_device"), "MockDevice")
        expect_equal(attr(iv, "sample_rate"), 10)
        expect_equal(attr(iv, "event_channel"), "event")
        expect_true(inherits(iv, "mnirs"))
    }
    ## ensemble sub-groups collect interval_times as a list
    expect_length(attr(result[[1]], "interval_times"), 2)
    expect_equal(
        attr(result[[1]], "interval_times"),
        list(0, 0), ## start times for two grouped intervals
        ignore_attr = TRUE
    )
    expect_length(attr(result[[2]], "interval_times"), 2)
    expect_equal(
        attr(result[[2]], "interval_times"),
        list(0, 0), ## adheres to `zero_time` to represent output data frame
        ignore_attr = TRUE
    )
})

test_that("group_intervals custom single-interval group retains original attrs", {
    interval1 <- create_mock_interval(
        time_start = 0,
        n = 11,
        event_time = 0,
        span = c(-2, 5)
    )
    interval2 <- create_mock_interval(
        time_start = 10,
        n = 11,
        event_time = 10,
        span = c(-1, 4)
    )
    interval3 <- create_mock_interval(
        time_start = 20,
        n = 11,
        event_time = 20,
        span = c(-1, 4)
    )
    interval_list <- list(
        interval_1 = interval1,
        interval_2 = interval2,
        interval_3 = interval3
    )
    metadata <- list(time_channel = "time", sample_rate = 10)

    ## intervals 1+2 ensembled; interval 3 returned as lone group (raw)
    result <- group_intervals(
        interval_list = interval_list,
        nirs_channels = rep(list(c("smo2_left", "smo2_right")), 3),
        metadata = metadata,
        event_groups = list(c(1, 2), 3),
        zero_time = FALSE,
        verbose = FALSE
    )

    lone <- result[["interval_3"]]
    ## original attrs are preserved on the lone interval
    expect_equal(attr(lone, "nirs_channels"), c("smo2_left", "smo2_right"))
    expect_equal(attr(lone, "time_channel"), "time")
    expect_equal(attr(lone, "interval_times"), 20)
    expect_equal(attr(lone, "interval_span"), c(-1, 4))

    ## with `zero_time = TRUE`
    result <- group_intervals(
        interval_list = interval_list,
        nirs_channels = rep(list(c("smo2_left", "smo2_right")), 3),
        metadata = metadata,
        event_groups = list(c(1, 2), 3),
        zero_time = TRUE,
        verbose = FALSE
    )

    lone <- result[["interval_3"]]
    expect_equal(attr(lone, "interval_times"), 0) ## adheres to `zero_time`
})


## extract_intervals() ===================================================
test_that("extract_intervals validates start/end args", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    ## unsupported types still error via as_mnirs_interval
    expect_error(
        extract_intervals(
            data,
            start = TRUE,
            span = c(-1, 1),
            verbose = FALSE
        ),
        "start.*must be"
    )

    expect_error(
        extract_intervals(
            data,
            end = list(1),
            span = c(-1, 1),
            verbose = FALSE
        ),
        "end.*must be"
    )
})

test_that("extract_intervals returns list of tibbles", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        start = by_time(2, 5),
        event_groups = "distinct",
        span = c(-1, 1),
        verbose = FALSE
    )

    expect_type(result, "list")
    expect_true(all(vapply(result, tibble::is_tibble, logical(1))))
    expect_equal(result[[1]]$time[1], 2 - 1)
    expect_equal(rev(result[[1]]$time)[1], 2 + 1)
    expect_equal(result[[2]]$time[1], 5 - 1)
    expect_equal(rev(result[[2]]$time)[1], 5 + 1)
})

test_that("extract_intervals works with start and end", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        start = by_time(2, 5),
        end = by_time(4, 8),
        event_groups = "distinct",
        span = c(0, 0),
        verbose = FALSE
    )

    expect_length(result, 2)
    expect_equal(result[[1]]$time[1], 2)
    expect_equal(rev(result[[1]]$time)[1], 4)
    expect_equal(result[[2]]$time[1], 5)
    expect_equal(rev(result[[2]]$time)[1], 8)
    ## interval_times is c(start, end) when both boundaries defined
    expect_equal(attr(result[[1]], "interval_times"), c(2, 4))
    expect_equal(attr(result[[2]], "interval_times"), c(5, 8))
})

test_that("extract_intervals works with by_sample", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        start = by_sample(21, 51),
        event_groups = "distinct",
        span = c(-1, 1),
        verbose = FALSE
    )

    expect_length(result, 2)
    expect_equal(result[[1]]$time[1], 2 - 1)
    expect_equal(rev(result[[1]]$time)[1], 2 + 1)
    expect_equal(result[[2]]$time[1], 5 - 1)
    expect_equal(rev(result[[2]]$time)[1], 5 + 1)
})

test_that("extract_intervals works with by_label", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    data$event[51] <- "marker"

    result <- extract_intervals(
        data = data,
        event_channel = "event",
        start = by_label("marker"),
        event_groups = "distinct",
        span = c(-1, 1),
        verbose = FALSE
    )

    expect_length(result, 1)
    expect_equal(result[[1]]$time[1], 5 - 1)
    expect_equal(rev(result[[1]]$time)[1], 5 + 1)
})

test_that("extract_intervals works with by_lap start only", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    ## replace character event with integer laps
    data$event <- rep(1:10, each = 10)

    result <- extract_intervals(
        data = data,
        event_channel = "event",
        start = by_lap(3),
        event_groups = "distinct",
        span = c(0, 0.5),
        verbose = FALSE
    )

    expect_length(result, 1)
    ## lap 3 starts at row 21 (time = 2.0), span c(0, 0.5) -> [2.0, 2.5]
    expect_equal(result[[1]]$time[1], 2.0)
    expect_equal(rev(result[[1]]$time)[1], 2.5)
})

test_that("extract_intervals works with by_lap start and end", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    data$event <- rep(1:10, each = 10)

    result <- extract_intervals(
        data = data,
        event_channel = "event",
        start = by_lap(2),
        end = by_lap(4),
        event_groups = "distinct",
        span = c(0, 0),
        verbose = FALSE
    )

    expect_length(result, 1)
    ## lap 2 first sample: row 11 (time = 1.0)
    ## lap 4 last sample: row 40 (time = 3.9)
    expect_equal(result[[1]]$time[1], 1.0)
    expect_equal(rev(result[[1]]$time)[1], 3.9)
    expect_equal(nrow(result[[1]]), 30) ## rows 11 to 40
})

test_that("extract_intervals works with multiple by_lap pairs", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    data$event <- rep(1:10, each = 10)

    result <- extract_intervals(
        data = data,
        event_channel = "event",
        start = by_lap(1, 5),
        end = by_lap(2, 7),
        event_groups = "distinct",
        span = c(0, 0),
        verbose = FALSE
    )

    expect_length(result, 2)
    ## lap 1 first sample: row 1 (time = 0.0)
    ## lap 2 last sample: row 20 (time = 1.9)
    expect_equal(result[[1]]$time[1], 0.0)
    expect_equal(rev(result[[1]]$time)[1], 1.9)
    ## interval 1: lap 1 first (row 1) to lap 2 last (row 20)
    expect_equal(nrow(result[[1]]), 20)
    ## lap 5 first sample: row 50 (time = 4.0)
    ## lap 7 last sample: row 70 (time = 6.9)
    expect_equal(result[[2]]$time[1], 4.0)
    expect_equal(rev(result[[2]]$time)[1], 6.9)
    ## interval 2: lap 5 first (row 41) to lap 7 last (row 70)
    expect_equal(nrow(result[[2]]), 30)
})

test_that("extract_intervals errors when by_lap used without event_channel", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    ## remove event_channel from metadata
    attr(data, "event_channel") <- NULL
    data$event <- NULL

    expect_error(
        extract_intervals(
            data = data,
            start = by_lap(1),
            span = c(0, 1),
            verbose = FALSE
        ),
        "event_channel.*required"
    )
})

test_that("extract_intervals coerces raw numeric to by_time", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        start = 2,
        event_groups = "distinct",
        span = c(-1, 1),
        verbose = FALSE
    )

    expect_length(result, 1)
    expect_equal(result[[1]]$time[1], 2 - 1)
    expect_equal(rev(result[[1]]$time)[1], 2 + 1)
})

test_that("extract_intervals coerces raw character to by_label", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    data$event[51] <- "marker"

    result <- extract_intervals(
        data = data,
        event_channel = "event",
        start = "marker",
        event_groups = "distinct",
        span = c(-1, 1),
        verbose = FALSE
    )

    expect_length(result, 1)
    expect_equal(result[[1]]$time[1], 5 - 1)
    expect_equal(rev(result[[1]]$time)[1], 5 + 1)
})

test_that("extract_intervals coerces raw integer to by_lap", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)
    data$event <- rep(1:10, each = 10)

    result <- extract_intervals(
        data = data,
        event_channel = "event",
        start = 2L,
        end = 4L,
        event_groups = "distinct",
        span = c(0, 0),
        verbose = FALSE
    )

    expect_length(result, 1)
    ## same as by_lap(2) / by_lap(4)
    expect_equal(result[[1]]$time[1], 1.0)
    expect_equal(rev(result[[1]]$time)[1], 3.9)
    expect_equal(nrow(result[[1]]), 30)
})

test_that("extract_intervals recycles positive span scalar", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        start = by_time(2),
        end = by_time(5),
        event_groups = "distinct",
        span = 1,
        verbose = FALSE
    )

    ## span = 1 → c(0, 1): start unchanged, end shifted +1
    expect_equal(result[[1]]$time[1], 2)
    expect_equal(rev(result[[1]]$time)[1], 6)
})

test_that("extract_intervals recycles negative span scalar", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        start = by_time(2),
        end = by_time(5),
        event_groups = "distinct",
        span = -1,
        verbose = FALSE
    )

    ## span = -1 → c(-1, 0): start shifted -1, end unchanged
    expect_equal(result[[1]]$time[1], 1)
    expect_equal(rev(result[[1]]$time)[1], 5)
})

test_that("extract_intervals applies zero_time correctly", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        start = by_time(5),
        event_groups = "distinct",
        span = c(-1, 1),
        zero_time = TRUE,
        verbose = FALSE
    )

    # Time should start at -1 (span before) after zero offset
    expect_equal(min(result[[1]]$time), -1)
})

test_that("extract_intervals handles grouping", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    result <- extract_intervals(
        data = data,
        start = by_time(2, 5, 8),
        event_groups = "ensemble",
        span = c(-0.5, 0.5), ## single span recycled to all events
        verbose = FALSE
    )

    expect_length(result, 1)
    expect_named(result, "ensemble")
    ## check interval span
    expect_setequal(range(result$ensemble$time), c(-0.5, 0.5))

    result <- extract_intervals(
        data = data,
        start = by_time(2, 4, 6, 8),
        event_groups = list(c(1, 3), c(2, 4)),
        span = list(c(-0.3, 0.3), c(-0.5, 0.5)),
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
        start = by_time(2, 5),
        event_groups = "distinct",
        span = list(c(-0.5, 0.5), c(-1, 1)),
        verbose = FALSE
    )

    expect_length(result, 2)
    # Second interval should be larger due to wider span
    expect_true(nrow(result[[2]]) > nrow(result[[1]]))
})

test_that("extract_intervals errors & messages", {
    data <- create_mock_mnirs(n = 100, sample_rate = 10)

    ## no interval specified
    expect_error(
        extract_intervals(
            data = data,
            span = c(-1, 1),
            verbose = FALSE
        ),
        regexp = "No interval specification"
    )

    ## edge case: event at data boundary
    expect_warning(
        result <- extract_intervals(
            data = data,
            start = by_time(0.5),
            event_groups = "distinct",
            span = c(-1, 1),
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
        start = by_time(1, 5),
        event_groups = "distinct",
        span = c(-1, 1),
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
        start = by_time(1, 5),
        event_groups = "ensemble",
        span = c(-1, 1),
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
        verbose = FALSE
    )

    result <- extract_intervals(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        start = by_time(870),
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
        verbose = FALSE
    ) |>
        resample_mnirs(verbose = FALSE)

    result <- extract_intervals(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        start = by_time(2150, 3168),
        event_groups = "ensemble",
        span = list(c(-30, 180)),
        zero_time = FALSE,
        verbose = FALSE
    )

    ## visual check
    plot(result[[1L]], time_labels = TRUE)

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
        start = by_time(2150, 3168),
        event_groups = "distinct",
        span = list(c(-30, 180)),
        zero_time = FALSE,
        verbose = FALSE
    )

    ## visual check
    # library(patchwork)
    # plot(result[[1L]]) + plot(result[[2L]])

    ## structure
    expect_length(result, 2)
    expect_length(result[[1L]], 3)
    expect_length(result[[2L]], 3)
    expect_named(result[[1L]], c("time", "smo2_left", "smo2_right"))
    expect_named(result[[2L]], c("time", "smo2_left", "smo2_right"))
    ## range of time_channel
    expect_lte(min(result[[1L]][[1]]), 2150 - 30)
    expect_equal(min(result[[1L]][[1]]), 2150 - 30, tolerance = 0.1)
    expect_lte(max(result[[1L]][[1]]), 2150 + 180)
    expect_equal(max(result[[1L]][[1]]), 2150 + 180, tolerance = 0.1)
    expect_lte(min(result[[2L]][[1]]), 3168 - 30)
    expect_equal(min(result[[2L]][[1]]), 3168 - 30, tolerance = 0.1)
    expect_lte(max(result[[2L]][[1]]), 3168 + 180)
    expect_equal(max(result[[2L]][[1]]), 3168 + 180, tolerance = 0.1)
})
