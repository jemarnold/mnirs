## slope ===========================================================
test_that("slope returns correct structure", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- slope(x)

    expect_type(result, "double")
    expect_equal(length(result), 1)
    expect_gt(result, 0)
})

test_that("slope calculates correctly", {
    x <- c(1, 3, 5, 7, 9)
    result_null <- slope(x)
    result_seq <- slope(x, t = seq_along(x))
    expect_equal(result_null, result_seq)
    expect_gt(result_null, 0)

    ## identity = 1
    expect_equal(slope(1:5, 1:5), 1)
    ## single value = NA
    expect_true(is.na(slope(5)))
    ## all identical values = 0
    expect_true(slope(rep(5, 10)) == 0)
    ## all NA = error
    expect_error(is.na(slope(rep(NA, 5))), "valid.*numeric")
    ## all x values identical = NA
    expect_true(is.na(slope(1:5, rep(1, 5))))

    ## all invalid
    expect_error(is.na(slope(list())), "valid.*numeric")
    expect_error(is.na(slope(NULL)), "valid.*numeric")
    expect_error(is.na(slope(rep(NA, 4))), "valid.*numeric")
    expect_error(is.na(slope(rep(NaN, 4))), "valid.*numeric")
    expect_true(is.na(slope(rep(Inf, 4))))
})

test_that("slope returns same as lm model", {
    ## irregular time
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    t_irreg <- sort(runif(length(x), 0, 10))
    expect_equal(slope(x, t_irreg), unname(coef(lm(x ~ t_irreg))[2]))
    
    x <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
    result <- slope(x, na.rm = TRUE)
    coef <- coef(lm(x ~ seq_along(x)))[[2L]]
    expect_equal(result, coef)

    ## boundary NAs
    x <- c(NA, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, NA)
    result <- slope(x, na.rm = TRUE)
    coef <- coef(lm(x ~ seq_along(x)))[[2L]]
    expect_equal(result, coef)   
})

test_that("slope handles NA values correctly", {
    x_na <- c(1, NA, 3, 4, 5)

    # NA propagates by default
    expect_true(is.na(slope(x_na)))

    # na.rm removes NA observations
    expect_equal(
        slope(x_na, na.rm = TRUE),
        slope(c(1, 3, 4, 5), t = c(1L, 3L, 4L, 5L))
    )

    # NA in t
    t_na <- c(1, 2, NA, 4, 5)
    expect_true(is.na(slope(1:5, t_na)))
    expect_equal(
        slope(1:5, t_na, na.rm = TRUE),
        unname(coef(lm(1:5 ~ t_na))[2])
    )

    # NA in both - pairwise complete
    expect_equal(
        slope(x_na, t_na, na.rm = TRUE),
        slope(c(1, 4, 5), t = c(1, 4, 5))
    )
})

test_that("slope returns NA for edge cases", {
    # Fewer than 2 points
    expect_true(is.na(slope(1)))
    expect_error(slope(numeric(0)), "valid.*numeric")

    # All NA after removal
    expect_error(slope(c(NA, NA), na.rm = TRUE), "valid.*numeric")

    # Zero variance in t
    expect_true(is.na(slope(c(1, 2, 3), t = c(5, 5, 5))))
})

test_that("slope validates inputs", {
    expect_error(slope("a"), "valid.*numeric")
    expect_error(slope(1:5, "b"), "valid.*numeric")
    expect_error(slope(1:5, 1:3), "equal length")
})

test_that("slope default t argument works", {
    x <- c(2, 4, 6, 8)
    expect_equal(slope(x), slope(x, t = 1:4))
})

test_that("slope handles large integer sequences without overflow", {
    n <- 1e6
    x <- rep(1, n)
    expect_equal(slope(x), 0)

    x <- as.double(seq_len(n))
    expect_equal(slope(x), 1)
})

## rolling_slope ===========================================================