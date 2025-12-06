## Test compute_local_windows() =========================================
test_that("compute_local_windows validates inputs", {
    t <- 1:10

    expect_error(
        compute_local_windows(t, width = NULL, span = NULL),
        "width.*span.*must be defined"
    )

    expect_message(
        compute_local_windows(t, width = 2, span = 1),
        "width.*overrides.*span"
    )

    expect_error(
        compute_local_windows(t, width = -1),
        "width.*valid.*integer"
    )

    expect_error(
        compute_local_windows(t, span = -1),
        "span.*valid.*numeric"
    )
})

test_that("compute_local_windows returns correct structure", {
    t <- 1:10
    width <- 2
    result <- compute_local_windows(t, width = width, span = NULL)

    expect_type(result, "list")
    expect_length(result, length(t))
    expect_true(all(vapply(result, is.numeric, logical(1))))

    ## with span
    t <- seq(0, 10, by = 0.5)
    span <- 1
    result <- compute_local_windows(t, width = NULL, span = span)

    expect_type(result, "list")
    expect_length(result, length(t))
})

test_that("compute_local_windows excludes central value", {
    skip("compute_local_outliers requires inclusion of local median")
    t <- 1:10
    width <- 2
    result <- compute_local_windows(t, width = width, span = NULL)

    # Central value should be excluded from each window
    for (i in seq_along(result)) {
        expect_false(i %in% result[[i]])
    }

    ## with span
    t <- seq(0, 10, by = 0.5)
    span <- 1
    result <- compute_local_windows(t, width = NULL, span = span)

    # Central value should be excluded from each window
    for (i in seq_along(result)) {
        expect_false(i %in% result[[i]])
    }
})

test_that("compute_local_windows respects width boundaries", {
    t <- 1:10
    width <- 4
    result <- compute_local_windows(t, width = width, span = NULL)

    # First element: can only look forward
    expect_equal(result[[1]], 1:3)
    # Last element: can only look backward
    expect_equal(result[[10]], 8:10)
    # Middle element: symmetric window
    expect_equal(result[[5]], c(3:7))

    ## with span
    t <- seq(0, 10, by = 0.5)
    span <- 2
    result <- compute_local_windows(t, width = NULL, span = span)

    # First element: can only look forward
    expect_equal(result[[1]], 1:3)
    expect_equal(t[result[[1]]], t[1:3])
    # Last element: can only look backward
    expect_equal(result[[21]], 19:21)
    expect_equal(t[result[[21]]], t[19:21])
    # Middle element: symmetric window
    expect_equal(result[[10]], c(8:12))
    expect_equal(t[result[[10]]], t[c(8:12)])
})

test_that("compute_local_windows width and span >= length(x) works", {
    t <- 1:10
    result <- compute_local_windows(t, width = 20)
    expect_equal(result, rep(list(t), length(t)))

    result <- compute_local_windows(t, span = 20)
    expect_equal(result, rep(list(t), length(t)))
})

test_that("compute_local_windows works with width = 1 and span = 0", {
    t <- 1:10
    result <- compute_local_windows(t, width = 1)
    expect_equal(result, as.list(t))

    result <- compute_local_windows(t, span = 0)
    expect_equal(result, as.list(t))
})

## test compute_window_of_valid_neighbours() ===========================
test_that("compute_window_of_valid_neighbours() validates inputs", {
    x <- c(1, 2, NA, 4, 5)

    # Must specify width or span
    expect_error(
        compute_window_of_valid_neighbours(x),
        "width.*span.*must be defined"
    )

    # width must be positive integer
    expect_error(
        compute_window_of_valid_neighbours(x, width = -1),
        "width.*one-element positive.*integer"
    )
    expect_error(
        compute_window_of_valid_neighbours(x, width = 1.5),
        "width.*one-element positive.*integer"
    )

    # span must be positive
    expect_error(
        compute_window_of_valid_neighbours(x, span = -1),
        "span.*one-element positive.*numeric"
    )

    # Both width and span warns and uses width
    expect_message(
        compute_window_of_valid_neighbours(x, width = 2, span = 0.5),
        "width.*overrides.*span"
    )
})

test_that("compute_window_of_valid_neighbours() works with width", {
    x <- c(1, 2, NA, 4, 5, NA, 7)

    # width = 1
    result <- compute_window_of_valid_neighbours(x, width = 2)
    expect_length(result, 2) # Two NAs
    expect_equal(result[[1]], c(2, 4))
    expect_equal(result[[2]], c(5, 7))

    # width = 2
    x <- c(1, 2, 3, NA, 5, 6, 7)
    result <- compute_window_of_valid_neighbours(x, width = 4)
    expect_equal(result[[1]], c(2, 3, 5, 6))

    # Edge case: NA at start
    x <- c(NA, 20, 30, 40)
    result <- compute_window_of_valid_neighbours(x, width = 2)
    expect_equal(result[[1]], 2)

    # Edge case: NA at end
    x <- c(10, 20, 30, NA)
    result <- compute_window_of_valid_neighbours(x, width = 2)
    expect_equal(result[[1]], 3)
})

test_that("compute_window_of_valid_neighbours() works with span", {
    t <- 0:4
    x <- c(1, 2, NA, 4, 5)

    # span = 1 (includes t[2] = 1 and t[4] = 3)
    result <- compute_window_of_valid_neighbours(x, t = t, span = 1)
    expect_length(result, 1)
    expect_equal(result[[1]], c(2, 4))

    # span = 0.5 (no values of t within t ± span should return same as width = 1)
    result <- compute_window_of_valid_neighbours(x, t = t, span = 0.5)
    expect_equal(result[[1]], c(2, 4))
    expect_equal(result, compute_window_of_valid_neighbours(x, width = 2))

    # Multiple NAs
    x <- c(1, NA, 3, NA, 5)
    result <- compute_window_of_valid_neighbours(x, t = t, span = 1)
    expect_length(result, 2)
    expect_equal(result[[1]], c(1, 3))
    expect_equal(result[[2]], c(3, 5))

    # Edge case: NA at start
    x <- c(NA, 2, 3, 4)
    expect_equal(compute_window_of_valid_neighbours(x, span = 1)[[1]], 2)
    expect_equal(compute_window_of_valid_neighbours(x, span = 0.5)[[1]], 2)

    # Edge case: NA at end
    x <- c(1, 2, 3, NA)
    expect_equal(compute_window_of_valid_neighbours(x, span = 1)[[1]], 3)
    expect_equal(compute_window_of_valid_neighbours(x, span = 0.5)[[1]], 3)
})

test_that("compute_window_of_valid_neighbours width and span >= length(x) works", {
    x <- c(1, 2, NA, 4, 5)
    result <- compute_window_of_valid_neighbours(x, width = 8)
    expect_equal(result, list(x[!is.na(x)]))

    result <- compute_window_of_valid_neighbours(x, span = 8)
    expect_equal(result, list(x[!is.na(x)]))
})

test_that("compute_window_of_valid_neighbours() handles no NAs", {
    skip("no NA condition should be defined before reaching this stage")
    x <- c(1, 2, 3, 4, 5)
    ## TODO returns empty list, is this good?
    expect_length(compute_window_of_valid_neighbours(x, width = 1), 0)
    expect_length(compute_window_of_valid_neighbours(x, span = 1), 0)
})

test_that("compute_window_of_valid_neighbours() handles all NAs", {
    skip("all NA condition should be defined before reaching this stage")
    x <- c(NA, NA, NA)
    ## TODO returns list of empty vectors, is this good?
    result <- compute_window_of_valid_neighbours(x, width = 1)
    expect_length(result, 3)
    expect_equal(result[[1]], integer(0))
    expect_equal(result[[2]], integer(0))
    expect_equal(result[[3]], integer(0))
})

test_that("compute_window_of_valid_neighbours works with width = 1 and span = 0", {
    x <- c(1, 2, NA, 4, 5)
    result <- compute_window_of_valid_neighbours(x, width = 1)
    expect_equal(result[[1]], c(2, 4))

    result <- compute_window_of_valid_neighbours(x, span = 0)
    expect_equal(result[[1]], c(2, 4))
})

## Test compute_local_fun() =========================================
test_that("compute_local_fun returns correct length", {
    x <- c(1, 2, 3, 4, 5)
    window_idx <- compute_local_windows(x, width = 1)
    result <- compute_local_fun(x, window_idx, fn = median)

    expect_type(result, "double")
    expect_length(result, length(x))
})

test_that("compute_local_fun calculates correct medians", {
    x <- c(10, 20, 30, 40, 50)
    # Manual windows: each looks at neighbours
    window_idx <- compute_local_windows(x, width = 2)
    result <- compute_local_fun(x, window_idx, fn = median)

    expect_equal(result[1], median(x[1:2])) # median of x[2]
    expect_equal(result[2], median(x[1:3])) # median of x[c(1,3)]
    expect_equal(result[5], median(x[4:5])) # median of x[4]
})

test_that("compute_local_fun handles NA values", {
    ## this shouldn't happen with handle_na, but just in case
    x <- c(1, NA, 3, 4, 5)
    window_idx <- compute_local_windows(x, width = 2)
    result <- compute_local_fun(x, window_idx, fn = median)

    expect_false(all(is.na(result[1:3])))
    expect_equal(result[1], median(x[1]))
    expect_equal(result[2], median(x[c(1, 3)]))
    expect_equal(result[3], median(x[c(3, 4)]))
})

test_that("compute_local_fun() handles empty windows", {
    x <- c(1, 2, 3)
    window_idx <- list(integer(0))
    ## TODO returns NA, is this good?
    result <- compute_local_fun(x, window_idx, fn = median)
    expect_true(is.na(result))
})

test_that("compute_local_fun() handles single values", {
    x <- c(10, 20, 30)
    window_idx <- list(1, 2, 3)
    result <- compute_local_fun(x, window_idx, fn = median)
    expect_equal(result, x)
})


## Test compute_outliers() ==============================================
test_that("compute_outliers returns logical vector", {
    x <- c(1, 2, 3, 100, 5)
    t <- 1:5
    window_idx <- compute_local_windows(t, width = 2, span = NULL)
    local_medians <- compute_local_fun(x, window_idx, fn = median)
    result <- compute_outliers(x, window_idx, local_medians, outlier_cutoff = 3)

    expect_type(result, "logical")
    expect_length(result, length(x))
    expect_true(result[4]) # 100 should be flagged
    expect_false(any(result[-4])) # Normal values should not be flagged
})

test_that("compute_outliers threshold sensitivity via outlier_cutoff", {
    x <- c(1, 2, 3, 10, 5)
    t <- 1:5
    window_idx <- compute_local_windows(t, width = 2, span = NULL)
    local_medians <- compute_local_fun(x, window_idx, fn = median)

    # Strict threshold
    strict <- compute_outliers(x, window_idx, local_medians, outlier_cutoff = 1)
    # Lenient threshold
    lenient <- compute_outliers(
        x,
        window_idx,
        local_medians,
        outlier_cutoff = 10
    )

    expect_true(sum(strict) > sum(lenient))
})

test_that("compute_outliers validates outlier_cutoff", {
    x <- 1:5
    t <- 1:5
    window_idx <- compute_local_windows(t, width = 2, span = NULL)
    local_medians <- compute_local_fun(x, window_idx, fn = median)

    expect_error(
        compute_outliers(x, window_idx, local_medians, outlier_cutoff = -1),
        "positive.*integer"
    )
})

test_that("compute_outliers handles no outliers", {
    x <- 1:5
    t <- 1:5
    window_idx <- compute_local_windows(t, width = 2, span = NULL)
    local_medians <- compute_local_fun(x, window_idx, fn = median)
    result <- compute_outliers(x, window_idx, local_medians, outlier_cutoff = 3)

    expect_true(all(!result))
})

test_that("compute_outliers handles NA", {
    ## this shouldn't happen with handle_na, but just in case
    x <- c(1, 2, NA, 100, 5)
    t <- 1:5
    window_idx <- compute_local_windows(t, width = 2, span = NULL)
    local_medians <- compute_local_fun(x, window_idx, fn = median)
    result <- compute_outliers(x, window_idx, local_medians, outlier_cutoff = 3)

    expect_true(all(!result))
})
