## helper to create test data with metadata
create_test_data <- function(
        time_max = 10,
        sample_rate = 10,
        add_metadata = TRUE
) {
    time = seq(0, time_max, by = 1/sample_rate)
    nrow <- length(time)
    data <- tibble(time = time,
                   nirs1 = rnorm(nrow, 50, 5),
                   nirs2 = rnorm(nrow, 60, 5),
                   nirs3 = rnorm(nrow, 80, 5),
                   event = c(1, rep(NA, nrow - 2), 2),
    )
    class(data) <- c("mnirs", class(data))

    if (add_metadata) {
        attr(data, "time_channel") <- "time"
        attr(data, "nirs_channels") <- c("nirs1", "nirs2")
        attr(data, "event_channel") <- "event"
        attr(data, "sample_rate") <- sample_rate
    }

    return(data)
}


## validate_numeric() ==================================
test_that("validate_numeric accepts valid numeric values", {
    expect_silent(validate_numeric(5))
    expect_silent(validate_numeric(c(1, 2, 3)))
    expect_silent(validate_numeric(1.5))
})

test_that("validate_numeric checks element count", {
    x <- c(1, 2)
    expect_error(validate_numeric(x, elements = 1), "numeric")
    expect_silent(validate_numeric(5, elements = 1))
    expect_silent(validate_numeric(c(1, 2, 3), elements = 3))
    ## fewer elements than specified fails
    expect_error(validate_numeric(1, elements = 2), "numeric")
})

test_that("validate_numeric checks range with inclusive bounds", {
    expect_silent(validate_numeric(5, range = c(0, 10)))
    expect_silent(validate_numeric(0, range = c(0, 10)))
    expect_silent(validate_numeric(10, range = c(0, 10)))
    expect_error(validate_numeric(-1, range = c(0, 10)), "numeric")
    expect_error(validate_numeric(11, range = c(0, 10)), "numeric")
})

test_that("validate_numeric checks range with exclusive bounds", {
    expect_silent(validate_numeric(5, range = c(0, 10), inclusive = FALSE))
    expect_error(
        validate_numeric(0, range = c(0, 10), inclusive = FALSE),
        "numeric"
    )
    expect_error(
        validate_numeric(10, range = c(0, 10), inclusive = FALSE),
        "numeric"
    )
})

test_that("validate_numeric rejects non-numeric input", {
    expect_error(validate_numeric("text"), "numeric")
    expect_error(validate_numeric(TRUE), "numeric")
    expect_error(validate_numeric(list(1, 2))) ## "not implemented for type `list`"
})

test_that("validate_numeric handles NA, NaN, Inf", {
    expect_silent(validate_numeric(c(1, NA)))
    expect_error(validate_numeric(NA), "numeric")
    expect_error(validate_numeric(NaN), "numeric")
    expect_silent(validate_numeric(Inf))
    expect_error(validate_numeric(Inf, range = c(0, 1)), "numeric")
    expect_silent(validate_numeric(c(1, NA, 3), elements = 2))
})

test_that("validate_numeric handles NULL", {
    expect_silent(validate_numeric(NULL))
})

## validate_mnirs_data() ========================================
test_that("validate_mnirs_data() accepts valid data frames", {
    data <- create_test_data()
    expect_silent(validate_mnirs_data(data))
})

test_that("validate_mnirs_data() rejects non-data frames", {
    expect_error(validate_mnirs_data(list(a = 1, b = 2)), "should be a data frame")
    expect_error(validate_mnirs_data(c(1, 2, 3)), "should be a data frame")
})

test_that("validate_mnirs_data() rejects data frames with < 2 columns", {
    expect_error(validate_mnirs_data(data.frame(x = 1:10)), "at least")
})


## validate_nirs_channels() ========================================
test_that("validate_nirs_channels() uses metadata when NULL", {
    data <- create_test_data()
    result <- validate_nirs_channels(data, NULL, inform = FALSE)
    expect_equal(result, c("nirs1", "nirs2"))
})

test_that("validate_nirs_channels() uses explicit channels when provided", {
    data <- create_test_data()
    result <- validate_nirs_channels(data, "nirs1")
    expect_equal(result, "nirs1")
})

test_that("validate_nirs_channels() works with nirs_channels = list()", {
    data <- create_test_data()
    nirs_list <- list("nirs1", "nirs2")
    result <- validate_nirs_channels(data, nirs_list)
    expect_equal(result, nirs_list)

    nirs_list <- list(c("nirs1", "nirs2"), "nirs3")
    result <- validate_nirs_channels(data, nirs_list)
    expect_equal(result, nirs_list)

    attr(data, "nirs_channels") <- nirs_list
    expect_warning(result <- validate_nirs_channels(data, NULL, inform = TRUE),
                   "All `nirs_channels`.*grouped")
    expect_equal(result, nirs_list)
})

test_that("validate_nirs_channels() errors when not in metadata or provided", {
    data <- create_test_data(add_metadata = FALSE)
    expect_error(validate_nirs_channels(data, NULL), "not found in metadata")
})

test_that("validate_nirs_channels() errors when columns don't exist", {
    data <- create_test_data()
    expect_error(validate_nirs_channels(data, "nonexistent"), "match exactly")
})

test_that("validate_nirs_channels() errors for non-numeric channels", {
    data <- create_test_data()
    data$nirs1 <- as.character(data$nirs1)
    expect_error(validate_nirs_channels(data, c("nirs1", "nirs2")), "must be .*numeric")
})

test_that("validate_nirs_channels() errors when < 2 valid values", {
    data <- create_test_data()
    data$nirs1 <- c(1, rep(NA, nrow(data)-1))
    expect_error(validate_nirs_channels(data, c("nirs1", "nirs2")), "at least two valid")
})


## validate_time_channel() ========================================
test_that("validate_time_channel() uses metadata when NULL", {
    data <- create_test_data()
    result <- validate_time_channel(data, NULL)
    expect_equal(result, "time")
})

test_that("validate_time_channel() uses explicit channel when provided", {
    data <- create_test_data()
    data$time_new <- data$time
    result <- validate_time_channel(data, "time_new")
    expect_equal(result, "time_new")
})

test_that("validate_time_channel() errors when not in metadata or provided", {
    data <- create_test_data(add_metadata = FALSE)
    expect_error(validate_time_channel(data, NULL), "not found in metadata")
})

test_that("validate_time_channel() errors when column doesn't exist", {
    data <- create_test_data()
    expect_error(validate_time_channel(data, "nonexistent"), "match exactly")
})

test_that("validate_time_channel() errors for non-numeric channel", {
    data <- create_test_data()
    data$time <- as.character(data$time)
    expect_error(validate_time_channel(data, "time"), "must be .*numeric")
})

test_that("validate_time_channel() errors when < 2 valid values", {
    data <- create_test_data()
    data$time <- c(1, rep(NA, nrow(data)-1))
    expect_error(validate_time_channel(data, "time"), "at least two valid")
})


## validate_event_channel() ========================================
test_that("validate_event_channel() uses metadata when NULL", {
    data <- create_test_data()
    result <- validate_event_channel(data, NULL)
    expect_equal(result, "event")
})

test_that("validate_event_channel() uses explicit channel when provided", {
    data <- create_test_data()
    data$lap <- data$event
    result <- validate_event_channel(data, "lap")
    expect_equal(result, "lap")
})

test_that("validate_event_channel() errors when not in metadata or provided", {
    data <- create_test_data(add_metadata = FALSE)
    expect_error(validate_event_channel(data, NULL), "not found in metadata")

    ## no metadata, required = FALSE
    expect_equal(validate_event_channel(data, NULL, require = FALSE), NULL)
})

test_that("validate_event_channel() errors when column doesn't exist", {
    data <- create_test_data()
    expect_error(validate_event_channel(data, "nonexistent"), "match exactly")
})

test_that("validate_event_channel() errors when all NA", {
    data <- create_test_data()
    data_one <- data[1:(nrow(data)-1),] ## one valid should work
    expect_equal(validate_event_channel(data_one, "event"), "event")
    data_na <- data[2:(nrow(data)-1),]
    expect_error(validate_event_channel(data_na, "event"), "at least one valid")
})



## between() ===============================================================
test_that("between() handles basic inclusive range (default)", {
    expect_equal(between(5, 1, 10), TRUE)
    expect_equal(between(1, 1, 10), TRUE)
    expect_equal(between(10, 1, 10), TRUE)
    expect_equal(between(0, 1, 10), FALSE)
    expect_equal(between(11, 1, 10), FALSE)
})

test_that("between() handles vectorised inputs", {
    expect_equal(
        between(c(0, 1, 5, 10, 11), 1, 10),
        c(FALSE, TRUE, TRUE, TRUE, FALSE)
    )
    expect_equal(
        between(1:10, 3, 7),
        c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    )
})

test_that("between() handles invalid `left` and `right` values", {
    expect_error(
        between(1:5, c(2, 3), 4),
        "valid one-element.*numeric"
    )
    expect_error(
        between(1:5, 3, c(4, 5)),
        "valid one-element.*numeric"
    )
})

test_that("between() handles inclusive/exclusive", {
    ## both exclusive
    expect_false(between(1, 1, 10, inclusive = FALSE))
    expect_false(between(10, 1, 10, inclusive = FALSE))
    expect_true(between(5, 1, 10, inclusive = FALSE))
    expect_equal(between(c(1, 5, 10), 1, 10, inclusive = FALSE), c(FALSE, TRUE, FALSE))

    ## left inclusive
    expect_true(between(1, 1, 10, inclusive = "left"))
    expect_false(between(10, 1, 10, inclusive = "left"))
    expect_equal(between(c(1, 5, 10), 1, 10, inclusive = "left"), c(TRUE, TRUE, FALSE))

    ## right inclusive
    expect_false(between(1, 1, 10, inclusive = "right"))
    expect_true(between(10, 1, 10, inclusive = "right"))
    expect_equal(between(c(1, 5, 10), 1, 10, inclusive = "right"), c(FALSE, TRUE, TRUE))
})

test_that("between() detects positive non-zero values", {
    expect_true(between(0, 0, Inf))
    expect_true(between(Inf, 0, Inf))
    expect_false(between(0, 0, Inf, inclusive = FALSE))
    expect_true(between(1, 0, Inf, inclusive = FALSE))
    expect_false(between(-1, 0, Inf))
    expect_false(between(-1, 0, Inf, inclusive = FALSE))
    expect_equal(
        between(c(-1, 0, 0.001, 1), 0, Inf, inclusive = FALSE),
        c(FALSE, FALSE, TRUE, TRUE)
    )
})

test_that("between() handles NA values", {
    expect_equal(between(NA, 1, 10), NA)
    expect_equal(between(c(1, NA, 5), 1, 10), c(TRUE, NA, TRUE))
    expect_error(between(5, NA, 10), "`left`.*valid.*numeric")
    expect_error(between(5, 1, NA), "`right`.*valid.*numeric")
})

test_that("between() handles infinite values", {
    expect_true(between(Inf, 1, Inf), TRUE)
    expect_false(between(Inf, 1, Inf, inclusive = FALSE), FALSE)
    expect_true(between(-Inf, -Inf, 10), TRUE)
    expect_false(between(-Inf, -Inf, 10, inclusive = FALSE), FALSE)
})

test_that("between() handles negative ranges", {
    expect_true(between(-5, -10, -1))
    expect_true(between(-10, -10, -1))
    expect_false(between(-11, -10, -1))
})

test_that("between() handles degenerate ranges where left equals right", {
    expect_true(between(5, 5, 5), TRUE)
    expect_false(between(5, 5, 5, inclusive = FALSE))
    expect_false(between(5, 5, 5, inclusive = "left"))
    expect_false(between(5, 5, 5, inclusive = "right"))
})

test_that("between is equivalent to dplyr::between()", {
    expect_equal(
        between(c(0, 5, 10, 15), 1, 10),
        dplyr::between(c(0, 5, 10, 15), 1, 10)
    )

    expect_equal(between(NA, 1, 10), dplyr::between(NA, 1, 10))
    expect_equal(
        between(c(1, NA, 5), 1, 10),
        dplyr::between(c(1, NA, 5), 1, 10))
})


## estimate_sample_rate() ======================================
test_that("estimate_sample_rate works correctly", {
    # Regular 100 Hz data
    time_100hz <- seq(0, 1, by = 0.01)
    expect_equal(estimate_sample_rate(time_100hz), 100)

    # 50 Hz data
    time_50hz <- seq(0, 1, by = 0.02)
    expect_equal(estimate_sample_rate(time_50hz), 50)

    ## irregular sampling
    set.seed(13)
    x <- seq(0, 1, by = 0.02)
    x <- x + rnorm(length(x), 0, 0.001)
    expect_equal(estimate_sample_rate(x), 50, tolerance = 1)
    expect_true(estimate_sample_rate(x) %in% c(49.5, 50, 50.5))
    expect_equal(estimate_sample_rate(x), 50, tolerance = 1)

    # With NAs in diffs (not in x directly, as diff removes one element)
    time_with_gaps <- c(0, 0.01, NA, 0.03, 0.04)
    expect_type(estimate_sample_rate(time_with_gaps), "double")

    # Edge case: single diff value
    expect_equal(estimate_sample_rate(c(0, 0.01)), 100)
    expect_error(estimate_sample_rate(NA), "numeric")
})


## validate_sample_rate() ========================================
test_that("validate_sample_rate() uses metadata when NULL", {
    data <- create_test_data(sample_rate = 15)
    result <- validate_sample_rate(data, "time", NULL)
    expect_equal(result, 15)
})

test_that("validate_sample_rate() uses explicit value with warning when provided", {
    data <- create_test_data(sample_rate = 10)
    expect_equal(validate_sample_rate(data, "time", 20), 20) |>
        expect_warning("appears to be inconsistent with estimated")
    ## uses explicit without warning
    expect_equal(validate_sample_rate(data, "time", 20, inform = FALSE), 20) |>
        expect_silent()
})

test_that("validate_sample_rate() does not warn for integer time_channel", {
    data <- create_test_data(time_max = 100, sample_rate = 1)
    expect_silent(validate_sample_rate(data, "time", 5, FALSE))
})

test_that("validate_sample_rate() estimates from time_channel when NULL", {
    rate = 9.5
    data <- create_test_data(time_max = 10, sample_rate = rate, add_metadata = FALSE)
    result <- validate_sample_rate(data, "time", NULL, inform = FALSE)
    expect_equal(result, round(rate))

    rate = 0.8
    data <- create_test_data(time_max = 10, sample_rate = rate, add_metadata = FALSE)
    result <- validate_sample_rate(data, "time", NULL, inform = FALSE)
    expect_equal(result, round(rate))

    rate = 11
    data <- create_test_data(time_max = 10, sample_rate = rate, add_metadata = FALSE)
    result <- validate_sample_rate(data, "time", NULL, inform = FALSE)
    expect_equal(result, 10)

    rate = 44
    data <- create_test_data(time_max = 10, sample_rate = rate, add_metadata = FALSE)
    result <- validate_sample_rate(data, "time", NULL, inform = FALSE)
    expect_equal(result, 50)

    rate = 98
    data <- create_test_data(time_max = 10, sample_rate = rate, add_metadata = FALSE)
    result <- validate_sample_rate(data, "time", NULL, inform = FALSE)
    expect_equal(result, 100)
})

test_that("validate_sample_rate() shows message when estimating", {
    data <- create_test_data(add_metadata = FALSE)
    expect_message(
        validate_sample_rate(data, "time", NULL, inform = TRUE),
        "Estimated"
        ) |>
        expect_message("Overwrite")
})

test_that("validate_sample_rate() errors for non-single non-numeric non-positive", {
    data <- create_test_data()
    expect_error(validate_sample_rate(data, "time", "10", FALSE), "must be .*numeric")
    expect_error(validate_sample_rate(data, "time", NA, FALSE), "must be .*numeric")
    expect_error(validate_sample_rate(data, "time", 0, FALSE), "must be .*positive")
    expect_error(validate_sample_rate(data, "time", -5, FALSE), "must be .*positive")
    expect_error(validate_sample_rate(data, "time", c(10, 20), FALSE), "must be .*one-element")
})


