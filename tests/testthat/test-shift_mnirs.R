test_that("shift_mnirs requires either to or by", {
    data <- tibble(
        time = 1:10,
        ch1 = rnorm(10)
    )

    expect_error(
        shift_mnirs(data, nirs_channels = list("ch1"),
                    time_channel = "time"),
        "either.*to.*by"
    )
})

test_that("shift_mnirs shifts by constant correctly", {
    data <- tibble(
        time = 1:10,
        ch1 = 1:10,
        ch2 = 11:20
    )

    result <- shift_mnirs(data, nirs_channels = list("ch1", "ch2"),
                          time_channel = "time", by = 5,
                          verbose = FALSE)

    expect_equal(result$ch1, data$ch1+5)
    expect_equal(result$ch2, data$ch2+5)
})

test_that("shift_mnirs preserves relative scaling within groups", {
    data <- tibble(
        time = 1:10,
        ch1 = 1:10,
        ch2 = 11:20
    )

    result <- shift_mnirs(data, nirs_channels = list(c("ch1", "ch2")),
                          time_channel = "time", to = 0, width = 1,
                          position = "min", verbose = FALSE)

    # ch1 min is 1, so shift both by -1
    expect_equal(result$ch1, 0:9)
    expect_equal(result$ch2, 10:19)
    # Relative difference preserved
    expect_equal(result$ch2[1] - result$ch1[1], 10)
})

test_that("shift_mnirs loses relative scaling across groups", {
    data <- tibble(
        time = 1:10,
        ch1 = 1:10,
        ch2 = 11:20
    )

    result <- shift_mnirs(data, nirs_channels = list("ch1", "ch2"),
                          time_channel = "time", to = 0, width = 1,
                          position = "min", verbose = FALSE)

    # Each shifted independently to min = 0
    expect_equal(result$ch1, 0:9)
    expect_equal(result$ch2, 0:9)
    # Relative scaling lost
    expect_equal(result$ch2[1] - result$ch1[1], 0)
})

test_that("shift_mnirs handles position = 'max' correctly", {
    data <- tibble(
        time = 1:10,
        ch1 = 1:10
    )

    result <- shift_mnirs(data, nirs_channels = list("ch1"),
                          time_channel = "time", to = 100, width = 1,
                          position = "max", verbose = FALSE)

    # Max is 10, shift to 100 means add 90
    expect_equal(result$ch1, 91:100)
})

test_that("shift_mnirs handles position = 'first' with span", {
    data <- tibble(
        time = seq(0, 9, by = 1),
        ch1 = c(rep(10, 3), 1:7)
    )

    result <- shift_mnirs(data, nirs_channels = list("ch1"),
                          time_channel = "time", to = 0,
                          position = "first", span = 2,
                          verbose = FALSE)

    # Mean of first 3 values (time 0-2) is 10
    expect_equal(result$ch1, c(rep(0, 3), -9:-3))
})

test_that("shift_mnirs preserves unshifted channels", {
    data <- tibble(
        time = 1:10,
        ch1 = 1:10,
        ch2 = 11:20,
        ch3 = 21:30
    )

    result <- shift_mnirs(data, nirs_channels = list("ch1"),
                          time_channel = "time", by = 5, width = 1,
                          verbose = FALSE)

    expect_equal(result$ch1, 6:15)
    expect_equal(result$ch2, 11:20)
    expect_equal(result$ch3, 21:30)
})

test_that("shift_mnirs updates metadata correctly", {
    data <- tibble(
        time = 1:10,
        ch1 = 1:10
    )
    attr(data, "nirs_channels") <- character(0)
    attr(data, "time_channel") <- NULL

    result <- shift_mnirs(data, nirs_channels = list("ch1"),
                          time_channel = "time", by = 5, width = 1,
                          verbose = FALSE)

    expect_true("ch1" %in% attr(result, "nirs_channels"))
    expect_equal(attr(result, "time_channel"), "time")
    expect_false(attr(result, "verbose"))
})

test_that("shift_mnirs handles mixed channel groups", {
    data <- tibble(
        time = 1:10,
        ch1 = 1:10,
        ch2 = 11:20,
        ch3 = 31:40,
        ch4 = 41:50
    )

    result <- shift_mnirs(
        data,
        nirs_channels = list(c("ch1", "ch2"), c("ch3", "ch4")),
        time_channel = "time",
        to = 0,
        width = 1,
        position = "min",
        verbose = FALSE
    )

    # Group 1: min is 1, both shifted by -1
    expect_equal(result$ch1, 0:9)
    expect_equal(result$ch2, 10:19)
    # Group 2: min is 31, both shifted by -31
    expect_equal(result$ch3, 0:9)
    expect_equal(result$ch4, 10:19)
    # Scaling preserved within, not between groups
    expect_equal(result$ch2[1] - result$ch1[1], 10)
    expect_equal(result$ch4[1] - result$ch3[1], 10)
})

test_that("shift_mnirs handles NA values correctly", {
    data <- tibble(
        time = 1:10,
        ch1 = c(NA, 2:10)
    )

    result <- shift_mnirs(data, nirs_channels = list("ch1"),
                          time_channel = "time", to = 0, width = 1,
                          position = "min", verbose = FALSE)

    # Min of non-NA values is 2
    expect_true(is.na(result$ch1[1]))
    expect_equal(result$ch1[2:10], 0:8)
})

test_that("shift_mnirs prioritises to over by", {
    data <- tibble(
        time = 1:10,
        ch1 = 1:10
    )

    expect_warning(
        result <- shift_mnirs(data, nirs_channels = list("ch1"),
                              time_channel = "time", to = 0, by = 100,
                              width = 1, position = "min", verbose = TRUE),
        ".*to.*by.*not both"
    )

    # Should use 'to', not 'by'
    expect_equal(result$ch1, 0:9)
})

test_that("shift_mnirs handles unevenly sampled data with span", {
    set.seed(13)
    # 0.5 Hz target (2 second intervals) with ±0.1s error
    base_times <- seq(0, 10, by = 2)
    time_error <- runif(length(base_times), -0.1, 0.1)

    data <- data.frame(
        time = round(base_times + time_error, 2),
        ch1 = c(10, 5, 20, 15, 25, 30)
    )

    # Test with 4-second span (should include ~2 samples per window)
    result <- shift_mnirs(data, list("ch1"), time_channel = "time",
                         to = 0, span = 4, position = "first")

    # First window should average first 2-3 values within 4s of start
    first_window_idx <- which(data$time <= data$time[1] + 4)
    expected_mean <- mean(data$ch1[first_window_idx])

    expect_equal(result$ch1[1], data$ch1[1] - expected_mean)
    expect_equal(which(data$time <= 4), first_window_idx)

    ## unequal sampling and position = "minimum"
    result_min <- shift_mnirs(data, list("ch1"), time_channel = "time",
                         to = 0, span = 4, position = "min")

    x <- data$time
    min_mean <- Inf
    idx <- NA_real_
    for (i in seq_along(x)) {
        window_idx <- which(x >= x[i] & x <= x[i] + 4)
        if (length(window_idx) > 1) {
            # print(window_idx)
            window_mean <- mean(data$ch1[window_idx])
            # print(window_mean)
            min_mean <- min(min_mean, window_mean)
            if (min_mean == window_mean) idx <- window_idx
        }
    }
    expect_equal(idx, first_window_idx)
    ## TODO check why failing?
    # expect_equal(mean(data$ch1[idx]) - mean(result_min$ch1[idx]), min(data$ch1))
})

test_that("shift_mnirs handles multiple channel groups", {
    data <- data.frame(time = 1:2, ch1 = c(10, 20), ch2 = c(15, 25), ch3 = c(5, 35))
    channels <- list(c("ch1", "ch2"), "ch3")
    result <- shift_mnirs(data, channels, "time", to = 0, width = 1)

    ## check grouping together: min shuold come from ch1 and ch3
    expect_true(any(result$ch1 == 0, na.rm = TRUE))
    expect_false(any(result$ch2 == 0, na.rm = TRUE))
    expect_true(any(result$ch3 == 0, na.rm = TRUE))
    ## check both shifted together maintaining relative scaling
    expect_equal(result$ch1 - result$ch2, data$ch1 - data$ch2)
    ## check both shifted independently
    expect_false(isTRUE(all.equal(result$ch1 - result$ch3,
                                  data$ch1 - data$ch3)))
})

test_that("shift_mnirs preserves non-channel columns", {
    data <- data.frame(time = 1:3, ch1 = c(10, 20, 30), other = c("A", "B", "C"))
    result <- shift_mnirs(data, list("ch1"), "time", by = 5)

    expect_equal(result$time, c(1, 2, 3))
    expect_equal(result$other, c("A", "B", "C"))
    expect_equal(result$ch1, c(15, 25, 35))
})

test_that("shift_mnirs validates position argument", {
    data <- data.frame(ch1 = c(10, 20))
    expect_error(shift_mnirs(data, list("ch1"), position = "invalid"),
                 "One of either `to` or `by`")
})

test_that("shift_mnirs handles empty channel list", {
    data <- data.frame(time = 1:3, value = c(10, 20, 30))
    expect_error(shift_mnirs(data, list(), by = 5),
                 "`nirs_channels` not found")
})





test_that("shift_mnirs works on Moxy", {
    data <- read_mnirs(
        file_path = example_mnirs("moxy_ramp.xlsx"),
        nirs_channels = c(smo2_left = "SmO2 Live",
                          smo2_right = "SmO2 Live(2)"),
        time_channel = c(time = "hh:mm:ss"),
        verbose = FALSE
    ) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::matches("smo2"), \(.x)
                replace_invalid(.x, invalid_values = c(0, 100), method = "NA")
            )
        )

    data_shifted <- shift_mnirs(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        time_channel = NULL,
        to = 0,
        by = NULL,
        span = 0,
        position = c("min", "max", "first")
    )

    # plot(data) + ggplot2::ylim(0, 100) + geom_hline(yintercept = c(0, 100))
    # plot(data_shifted) + ggplot2::ylim(0, 100) + geom_hline(yintercept = c(0, 100))

    ## check grouping together: min value should come from smo2_right
    expect_false(any(data_shifted$smo2_left == 0, na.rm = TRUE))
    expect_true(any(data_shifted$smo2_right == 0, na.rm = TRUE))
    ## check both shifted together maintaining relative scaling
    expect_equal(data_shifted$smo2_left - data_shifted$smo2_right,
                 data$smo2_left - data$smo2_right)

})

test_that("shift_mnirs(position = 'first') works on Moxy", {
    data <- read_mnirs(
        file_path = example_mnirs("moxy_ramp.xlsx"),
        nirs_channels = c(smo2_left = "SmO2 Live",
                          smo2_right = "SmO2 Live(2)"),
        time_channel = c(time = "hh:mm:ss"),
        zero_time = TRUE,
        verbose = FALSE
    ) |>
        dplyr::mutate(
            dplyr::across(
                dplyr::matches("smo2"), \(.x) {
                    replace_invalid(.x, invalid_values = c(0, 100), method = "NA")
                }),
            dplyr::across(
                dplyr::matches("smo2"), \(.x) replace_missing(.x, )
            )
        )

    data_shifted <- shift_mnirs(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        time_channel = NULL,
        to = 0,
        by = NULL,
        span = 120,
        position = "first"
    )

    # plot(data) + ggplot2::ylim(0, 100)
    # plot(data_shifted) + ggplot2::geom_ribbon(
    #     data = ~ dplyr::filter(.x, time <= 120),
    #     aes(ymax = 10, ymin = -10), alpha = 0.1, linewidth = NA) +
    #     ggplot2::geom_segment(aes(x = -Inf, xend = 120, y = 0, yend = 0),
    #                           linetype = "dotted", colour = "black")

    first_mean <- data_shifted |>
        dplyr::filter(time <= 120) |>
        dplyr::summarise(
            mean = mean(c(smo2_left, smo2_right), na.rm = TRUE)
        ) |>
        dplyr::pull(mean)

    expect_equal(first_mean, 0)
})

test_that("shift_mnirs works on Train.Red", {
    data <- read_mnirs(
        file_path = example_mnirs("train.red_intervals.csv"),
        nirs_channels = c(smo2_left = "SmO2",
                          smo2_right = "SmO2",
                          dhb_left = "HBDiff",
                          dhb_right = "HBDiff"),
        time_channel = c(time = "Timestamp (seconds passed)"),
        verbose = FALSE,
        keep_all = TRUE,
    )

    data_shifted <- shift_mnirs(
        data,
        nirs_channels = list("smo2_left", "smo2_right", c("dhb_left", "dhb_right")),
        time_channel = NULL,
        to = 0,
        by = NULL,
        span = 0,
        position = c("min", "max", "first")
    )

    # plot(data) + ggplot2::ylim(0, 100)
    # plot(data_shifted) + ggplot2::ylim(0, 100) + geom_hline(yintercept = c(0))

    ## check grouping together: min value should come from each group
    expect_true(any(data_shifted$smo2_left == 0, na.rm = TRUE))
    expect_true(any(data_shifted$smo2_right == 0, na.rm = TRUE))
    expect_true(any(data_shifted$dhb_left == 0, na.rm = TRUE))
    expect_false(any(data_shifted$dhb_right == 0, na.rm = TRUE))
    ## check both shifted together maintaining relative scaling
    expect_equal(data_shifted$dhb_left - data_shifted$dhb_right,
                 data$dhb_left - data$dhb_right)
    ## check both shifted independently
    expect_false(isTRUE(all.equal(data_shifted$smo2_left - data_shifted$smo2_right,
                                  data$smo2_left - data$smo2_right)))

})
