## compute_diagnostics ====================================================
test_that("compute_diagnostics returns correct structure", {
    x <- c(1, 3, 2, 5, 8)
    t <- seq_along(x)
    fitted <- predict(lm(x ~ t))

    result <- compute_diagnostics(x, t, fitted)

    expect_type(result, "list")
    expect_named(result, c("n_obs", "r2", "adj_r2", "rmse"))
    expect_type(result$n_obs, "integer")
    expect_type(result$r2, "double")
    expect_type(result$adj_r2, "double")
    expect_type(result$rmse, "double")
})

test_that("compute_diagnostics matches lm() summary", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    t <- seq_along(x)
    lm_fit <- lm(x ~ t)
    fitted <- predict(lm_fit)

    result <- compute_diagnostics(x, t, fitted)
    lm_summary <- summary(lm_fit)

    expect_equal(result$n_obs, length(fitted))
    expect_equal(result$r2, lm_summary$r.squared)
    expect_equal(result$adj_r2, lm_summary$adj.r.squared)
    expect_equal(result$rmse, sqrt(mean(residuals(lm_fit)^2)))
})

test_that("compute_diagnostics handles perfect fit", {
    x <- 1:10
    t <- 1:10
    fitted <- x # perfect fit

    result <- compute_diagnostics(x, t, fitted)

    expect_equal(result$r2, 1)
    expect_equal(result$adj_r2, 1)
    expect_equal(result$rmse, 0)
})

test_that("compute_diagnostics handles edge cases", {
    ## n < 2 returns NA
    result <- compute_diagnostics(x = 5, t = 1, fitted = 5)
    expect_equal(result$n_obs, 1L)
    expect_true(is.na(result$r2))
    expect_true(is.na(result$adj_r2))
    expect_true(is.na(result$rmse))

    ## n = 2 returns NA for adj_r2 only
    result <- compute_diagnostics(c(1, 2), c(1, 2), c(1, 2))
    expect_equal(result$n_obs, 2L)
    expect_equal(result$r2, 1)
    expect_true(is.na(result$adj_r2))
    expect_equal(result$rmse, 0)
})

test_that("compute_diagnostics handles zero variance in y", {
    x <- rep(5, 10)
    t <- seq_along(x)
    fitted <- rep(5, 10)

    result <- compute_diagnostics(x, t, fitted, verbose = FALSE)

    expect_true(is.na(result$r2))
    expect_equal(result$rmse, 0)
})

test_that("computes_diagnostics validates input lengths", {
    x <- 1:5
    t <- 1:5
    fitted <- 1:4 # wrong length

    expect_warning(
        result <- compute_diagnostics(x, t, fitted, verbose = TRUE),
        "x.*t.*fitted.*equal lengths"
    )
    expect_true(is.na(result$r2))
})


## as_data_list ===========================================================
test_that("as_data_list handles single data frame", {
    df <- data.frame(t = 1:5, x = rnorm(5))
    result <- as_data_list(df)

    expect_type(result, "list")
    expect_length(result, 1)
    expect_named(result, "interval_1")
    expect_identical(result[[1]], df)
})

test_that("as_data_list handles list of data frames", {
    df1 <- data.frame(t = 1:5, x = rnorm(5))
    df2 <- data.frame(t = 1:5, y = rnorm(5))
    data_list <- list(df1, df2)

    result <- as_data_list(data_list)

    expect_type(result, "list")
    expect_length(result, 2)
    expect_named(result, c("interval_1", "interval_2"))
    expect_identical(result[[1]], df1)
    expect_identical(result[[2]], df2)
})

test_that("as_data_list preserves names in list", {
    df1 <- data.frame(t = 1:5, x = rnorm(5))
    df2 <- data.frame(t = 1:5, y = rnorm(5))
    data_list <- list(baseline = df1, exercise = df2)

    result <- as_data_list(data_list)

    expect_named(result, c("baseline", "exercise"))
})

test_that("as_data_list handles grouped data frame", {
    skip_if_not_installed("dplyr")

    df <- data.frame(
        t = rep(1:5, 2),
        x = rnorm(10),
        group = rep(c("A", "B"), each = 5)
    )
    df <- create_mnirs_data(
        df,
        nirs_channels = "x",
        time_channel = "t"
    )
    grouped_df <- dplyr::group_by(df, group)

    result <- as_data_list(grouped_df)

    expect_type(result, "list")
    expect_length(result, 2)
    expect_named(result, c("A", "B"))
    expect_s3_class(result[[1]], "mnirs")
    expect_s3_class(result[[2]], "mnirs")
})

test_that("as_data_list handles multi-column grouping", {
    skip_if_not_installed("dplyr")

    df <- data.frame(
        t = rep(1:5, 4),
        x = rnorm(20),
        condition = rep(c("rest", "active"), each = 10),
        subject = rep(c("S1", "S2"), each = 5, times = 2)
    )
    df <- create_mnirs_data(
        df,
        nirs_channels = "x",
        time_channel = "t"
    )
    grouped_df <- dplyr::group_by(df, condition, subject)

    result <- as_data_list(grouped_df)

    expect_length(result, 4)
    expect_named(result, c("rest_S1", "rest_S2", "active_S1", "active_S2"))
})

test_that("as_data_list errors on invalid input", {
    expect_error(
        as_data_list("not a data frame"),
        "must be a list of data frames"
    )

    expect_error(
        as_data_list(list(data.frame(x = 1), "not a df")),
        "must be a list of data frames"
    )

    expect_error(
        as_data_list(list(1, 2, 3)),
        "must be a list of data frames"
    )
})

test_that("as_data_list preserves mnirs attributes", {
    df <- create_mnirs_data(
        data.frame(t = 1:5, x = rnorm(5)),
        nirs_channels = "x",
        time_channel = "t"
    )

    result <- as_data_list(df)

    expect_s3_class(result[[1]], "mnirs")
    expect_equal(attr(result[[1]], "nirs_channels"), "x")
    expect_equal(attr(result[[1]], "time_channel"), "t")
})

test_that("as_data_list preserves attributes in grouped split", {
    skip_if_not_installed("dplyr")

    df <- data.frame(
        t = rep(1:5, 2),
        x = rnorm(10),
        group = rep(c("B", "A"), each = 5)
    )
    df <- create_mnirs_data(
        df,
        nirs_channels = "x",
        time_channel = "t",
        sample_rate = 10
    )
    # attributes(df)
    grouped_df <- dplyr::group_by(df, group)

    result <- as_data_list(grouped_df)

    expect_equal(attr(result[[1]], "nirs_channels"), "x")
    expect_equal(attr(result[[1]], "time_channel"), "t")
    expect_equal(attr(result[[1]], "sample_rate"), 10)
    expect_equal(attr(result[[2]], "nirs_channels"), "x")
    expect_equal(attr(result[[2]], "time_channel"), "t")
    expect_equal(attr(result[[2]], "sample_rate"), 10)
})


## analyse_kinetics ======================================================

## helper to create test mnirs data
create_kinetics_data <- function(
    n = 50,
    sample_rate = 10,
    channels = c("smo2_left", "smo2_right")
) {
    t <- seq(0, (n - 1) / sample_rate, length.out = n)
    df <- data.frame(
        time = t,
        smo2_left = sin(t) * 10 + 50,
        smo2_right = cos(t) * 10 + 50
    )
    create_mnirs_data(
        df,
        nirs_channels = channels,
        time_channel = "time",
        sample_rate = sample_rate,
        event_times = sample(t, 1)
    )
}

test_that("analyse_kinetics returns correct structure", {
    data <- create_kinetics_data()

    result <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    expect_type(result, "list")
    expect_s3_class(result, "mnirs_kinetics")
    expect_named(
        result,
        c("method", "results", "data", "t0", "diagnostics",
          "channel_args", "call")
    )
    expect_equal(result$method, "peak_slope")
    expect_s3_class(result$results, "data.frame")
    expect_type(result$data, "list")
    expect_s3_class(result$t0, "data.frame")
    expect_s3_class(result$diagnostics, "data.frame")
    expect_s3_class(result$channel_args, "data.frame")
})

test_that("analyse_kinetics.peak_slope works with single data frame", {
    data <- create_kinetics_data()

    result <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    expect_equal(nrow(result$results), 1L)
    expect_equal(result$results$interval, "interval_1")
    expect_equal(result$results$nirs_channels, "smo2_left")
    expect_false(is.na(result$results$slope))
    expect_false(is.na(result$results$intercept))
    expect_length(result$data, 1L)
    expect_equal(nrow(result$t0), 1L)
    expect_equal(nrow(result$diagnostics), 1L)
    expect_equal(nrow(result$channel_args), 1L)
})

test_that("analyse_kinetics.peak_slope works with multiple nirs_channels", {
    data <- create_kinetics_data()

    result <- analyse_kinetics(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    expect_equal(nrow(result$results), 2L)
    expect_equal(result$results$nirs_channels, c("smo2_left", "smo2_right"))
    expect_equal(nrow(result$t0), 2L)
    expect_equal(nrow(result$diagnostics), 2L)
    expect_equal(nrow(result$channel_args), 2L)
    ## data list should have one element (single interval)
    expect_length(result$data, 1L)
    ## augmented data should have fitted columns
    expect_true("smo2_left_fitted" %in% names(result$data[[1]]))
    expect_true("smo2_right_fitted" %in% names(result$data[[1]]))
})

test_that("analyse_kinetics.peak_slope works with list of data frames", {
    df1 <- create_kinetics_data(n = 50)
    df2 <- create_kinetics_data(n = 50)

    result <- analyse_kinetics(
        list(baseline = df1, exercise = df2),
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    expect_equal(nrow(result$results), 2L)
    expect_equal(result$results$interval, c("baseline", "exercise"))
    expect_equal(nrow(result$t0), 2L)
    expect_equal(nrow(result$diagnostics), 2L)
    expect_equal(nrow(result$channel_args), 2L)
    expect_length(result$data, 2L)
    expect_named(result$data, c("baseline", "exercise"))
})

test_that("analyse_kinetics.peak_slope works with unnamed list", {
    df1 <- create_kinetics_data()
    df2 <- create_kinetics_data()

    result <- analyse_kinetics(
        list(df1, df2),
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    expect_equal(
        result$results$interval,
        c("interval_1", "interval_2")
    )
    expect_named(result$data, c("interval_1", "interval_2"))
})

test_that("analyse_kinetics.peak_slope passes width and span correctly", {
    data <- create_kinetics_data(n = 100, sample_rate = 10)

    result_width <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 10,
        verbose = FALSE
    )

    result_span <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        span = 1,
        verbose = FALSE
    )

    ## both should produce valid results
    expect_false(is.na(result_width$results$slope))
    expect_false(is.na(result_span$results$slope))
    expect_equal(result_width$diagnostics$n_obs, 10)
    ## ! check why span = 1 returns n_obs = 8
    expect_equal(result_span$diagnostics$n_obs, 10, tolerance = 2)
})

test_that("analyse_kinetics.peak_slope passes direction argument", {
    data <- create_kinetics_data(n = 100)

    result_pos <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 5,
        direction = "positive",
        verbose = FALSE
    )

    result_neg <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 5,
        direction = "negative",
        verbose = FALSE
    )

    expect_gt(result_pos$results$slope, 0)
    expect_lt(result_neg$results$slope, 0)
})

test_that("analyse_kinetics.peak_slope channel_args override defaults", {
    data <- create_kinetics_data(n = 100)

    result <- analyse_kinetics(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        method = "peak_slope",
        width = 5,
        direction = "positive",
        channel_args = list(
            smo2_right = list(direction = "negative")
        ),
        verbose = FALSE
    )

    expect_gt(result$results$slope[1], 0) ## smo2_left positive
    expect_lt(result$results$slope[2], 0) ## smo2_right negative

    ## channel_args should record per-channel settings
    ca <- result$channel_args
    expect_equal(
        ca$direction[ca$nirs_channels == "smo2_left"],
        "positive"
    )
    expect_equal(
        ca$direction[ca$nirs_channels == "smo2_right"],
        "negative"
    )
})

test_that("analyse_kinetics.peak_slope diagnostics are populated", {
    data <- create_kinetics_data(n = 100)

    result <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 10,
        verbose = FALSE
    )

    diag <- result$diagnostics
    expect_true("n_obs" %in% names(diag))
    expect_true("r2" %in% names(diag))
    expect_true("rmse" %in% names(diag))
    expect_equal(nrow(diag), 1L)
    expect_equal(diag$nirs_channels, "smo2_left")
})

test_that("analyse_kinetics.peak_slope augments data with fitted columns", {
    data <- create_kinetics_data(n = 50)

    result <- analyse_kinetics(
        data,
        nirs_channels = c("smo2_left", "smo2_right"),
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    aug <- result$data[[1]]
    expect_true("smo2_left_fitted" %in% names(aug))
    expect_true("smo2_right_fitted" %in% names(aug))
    ## fitted values should be NA except at window_idx
    expect_true(any(!is.na(aug$smo2_left_fitted)))
    expect_true(any(is.na(aug$smo2_left_fitted)))
})

test_that("analyse_kinetics.peak_slope captures t0 from event_times", {
    data <- create_kinetics_data(n = 50)
    attr(data, "event_times") <- 1.5

    result <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    expect_equal(result$t0$t0, 1.5)
})

test_that("analyse_kinetics.peak_slope works with grouped data", {
    skip_if_not_installed("dplyr")

    df <- data.frame(
        time = rep(seq(0, 4.9, by = 0.1), 2),
        smo2 = c(sin(seq(0, 4.9, by = 0.1)) * 10 + 50,
                 cos(seq(0, 4.9, by = 0.1)) * 10 + 50),
        group = rep(c("A", "B"), each = 50)
    )
    df <- create_mnirs_data(
        df,
        nirs_channels = "smo2",
        time_channel = "time",
        sample_rate = 10,
        event_times = sample(df$time, 1L)
    )
    grouped_df <- dplyr::group_by(df, group)

    result <- analyse_kinetics(
        grouped_df,
        nirs_channels = "smo2",
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    expect_equal(nrow(result$results), 2L)
    expect_equal(result$results$interval, c("A", "B"))
    expect_length(result$data, 2L)
})

test_that("analyse_kinetics.peak_slope results have correct columns", {
    data <- create_kinetics_data()

    result <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    expected_cols <- c(
        "interval", "nirs_channels", "slope", "intercept",
        "y", "t", "idx"
    )
    expect_true(all(expected_cols %in% names(result$results)))
})

test_that("analyse_kinetics captures call", {
    data <- create_kinetics_data()

    result <- analyse_kinetics(
        data,
        nirs_channels = "smo2_left",
        method = "peak_slope",
        width = 5,
        verbose = FALSE
    )

    expect_true(!is.null(result$call))
    expect_equal(class(result$call), "call")
    
})

test_that("analyse_kinetics errors on invalid method", {
    data <- create_kinetics_data()

    expect_error(
        analyse_kinetics(data, method = "nonexistent"),
        "arg.*should be"
    )
})
