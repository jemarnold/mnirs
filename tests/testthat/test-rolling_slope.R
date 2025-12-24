## slope ===========================================================
test_that("slope returns correct structure", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- slope(x)

    expect_type(result, "double")
    expect_equal(length(result), 1)
    expect_gt(result, 0)
    ## default t works
    expect_equal(slope(x), slope(x, t = 1:10))
})

test_that("slope handles negative values correctly", {
    x <- c(-5, -3, -1, 1, 3, 5)
    expect_equal(slope(x), 2)

    ## negative slope
    x_desc <- c(10, 8, 6, 4, 2)
    expect_equal(slope(x_desc), -2)

    ## mixed with irregular t
    x <- c(-2, 0, 2, 4)
    t <- c(1, 2.1, 3.3, 4.4)
    expect_lt(slope(x, t), 2)
})

test_that("slope calculates edge cases correctly", {
    ## identity = 1
    expect_equal(slope(1:5, 1:5), 1)
    ## single value = NA
    expect_true(is.na(slope(5)))
    ## all identical values = 0
    expect_true(slope(rep(5, 10)) == 0)
    ## all NA_real_ = NA
    expect_true(is.na(slope(x = rep(NA, 5))))
    ## all t values identical = NA
    expect_true(is.na(slope(1:5, rep(1, 5))))
    ## TODO character = valid.*numeric? or sum type error?
    # expect_error(is.na(slope(x = letters[1:4])), "valid.*numeric")
    expect_error(is.na(slope(x = letters[1:4])), "type.*character")
    expect_error(is.na(slope(1:4, t = letters[1:4])), "valid.*numeric")
    ## unequal length = error
    expect_error(slope(1:5, 1:3), "equal length")
    ## all invalid = NA
    expect_true(is.na(slope(list())))
    expect_true(is.na(slope(numeric(0))))
    expect_true(is.na(slope(NULL)))
    expect_true(is.na(slope(rep(NA, 4))))
    expect_true(is.na(slope(rep(NA, 4), na.rm = TRUE)))
    expect_true(is.na(slope(rep(NaN, 4))))
    expect_true(is.na(slope(rep(Inf, 4))))
    ## na.rm removes NaN & Inf
    expect_equal(slope(c(1, NaN, 3, Inf, 5), na.rm = TRUE), 1)
})

test_that("slope returns same as lm model", {
    ## irregular time
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    t_irreg <- sort(runif(length(x), 0, 10))
    expect_equal(slope(x, t_irreg), unname(coef(lm(x ~ t_irreg))[2]))
    
    x <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
    expect_equal(slope(x, na.rm = TRUE), coef(lm(x ~ seq_along(x)))[[2L]])

    ## boundary NAs
    x <- c(NA, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, NA)
    expect_equal(slope(x, na.rm = TRUE), coef(lm(x ~ seq_along(x)))[[2L]])
})

test_that("slope handles NA values correctly", {
    x_na <- c(1, NA, 3, 4, 5)

    # NA propagates by default
    expect_true(is.na(slope(x_na)))
    # na.rm removes NA observations
    expect_equal(slope(x_na, na.rm = TRUE), 1)

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

test_that("slope works with min_obs", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    expect_gt(slope(x, partial = TRUE), 1)
    expect_gt(slope(x, min_obs = length(x)), 1)
    ## min_obs > length(x) = NA
    expect_true(is.na(slope(x, min_obs = 20)))
    ## n < min_obs = NA
    expect_true(is.na(slope(x[1:2], min_obs = 3, partial = TRUE)))
    ## min_obs < 2 ## TODO silent argument, no warning
    # expect_warning(
    #     expect_gt(slope(x, min_obs = 1, verbose = TRUE), 1),
    #     "set to .*2"
    # )
    # expect_silent(
    #     expect_gt(slope(x, min_obs = 1, verbose = FALSE), 1)
    # )
    # expect_warning(
    #     expect_gt(slope(x, min_obs = 1, bypass_checks = FALSE), 1),
    #     "min_obs.*2"
    # )
    ## bypass doesn't warn about min_obs change
    # expect_silent(
    #     expect_gt(slope(x, min_obs = 1, bypass_checks = TRUE), 1)
    # )
})

test_that("slope handles large integer sequences without overflow", {
    n <- 1e6
    x <- rep(1, n)
    expect_equal(slope(x), 0)

    x <- as.double(seq_len(n))
    expect_equal(slope(x), 1)
})

test_that("slope respects bypass_checks", {
    x <- c(1, 3, 2, 5, 8)
    expect_error(
        slope(x, t = 1:4, bypass_checks = FALSE),
        "numeric.*equal length"
    )
    ## bypass doesn't validate length, returns bad result and warning
    expect_warning(slope(x, t = 1:4, bypass_checks = TRUE), "not a multiple")
})


## rolling_lm() ============================================
test_that("rolling_lm handles min_obs correctly", {
    skip_if_not_installed("roll")
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)

    ## min_obs < 2 warns and adjusts
    expect_warning(
        rolling_lm(x, width = 3, min_obs = 1, verbose = TRUE),
        "min_obs.*set to"
    )

    ## min_obs > width warns and adjusts
    expect_warning(
        rolling_lm(x, width = 3, min_obs = 5, verbose = TRUE),
        "min_obs.*set to"
    )

    ## silent when verbose = FALSE
    expect_silent(
        rolling_lm(x, width = 3, min_obs = 1, verbose = FALSE)
    )
})

test_that("rolling_lm align options all work", {
    skip_if_not_installed("roll")
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)

    result_center <- rolling_lm(x, width = 3, align = "center")
    result_left <- rolling_lm(x, width = 3, align = "left")
    result_right <- rolling_lm(x, width = 3, align = "right")

    expect_length(result_center, length(x))
    expect_length(result_left, length(x))
    expect_length(result_right, length(x))

    ## different alignments should produce different NA patterns
    expect_false(identical(
        which(is.na(result_center)),
        which(is.na(result_left))
    ))
    expect_equal(wrap(result_right, 2), result_left)
    expect_equal(wrap(result_center, 1), result_left)

    ## irregular t
    t <- c(0.5, 1.5, 3, 3.5, 4, 5.5, 7, 8, 10, 11)
    expect_equal(
        rolling_lm(x, t, width = 3, align = "right"),
        wrap(rolling_lm(x, t, width = 3, align = "center"), -1)
    )
    expect_equal(
        rolling_lm(x, t, width = 3, align = "right"),
        wrap(rolling_lm(x, t, width = 3, align = "left"), -2)
    )
})

test_that("rolling_lm matches rolling_slope with width only", {
    skip_if_not_installed("roll")
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)

    expect_equal(
        rolling_lm(x, width = 3),
        rolling_slope(x, width = 3)
    )
})


## rolling_slope ===========================================================
test_that("rolling_slope returns correct structure", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    result <- rolling_slope(x, width = 3)

    expect_type(result, "double")
    expect_equal(length(result), length(x))
    ## default t works
    expect_equal(slope(x), slope(x, t = 1:10))
})

test_that("rolling_slope handles negative values correctly", {
    x <- c(-5, -3, -1, 1, 3, 5)
    expect_all_equal(rolling_slope(x, width = 3, partial = TRUE), 2)
    expect_all_equal(
        rolling_slope(x, width = 3, span = 0, partial = TRUE, verbose = FALSE),
        2
    )
    expect_all_equal(rolling_slope(x, span = 2, partial = TRUE), 2)

    ## negative slope
    x_desc <- c(10, 8, 6, 4, 2)
    expect_all_equal(rolling_slope(x_desc, width = 3, partial = TRUE), -2)
    expect_all_equal(
        rolling_slope(x_desc, width = 3, span = 0, partial = TRUE, verbose = TRUE),
        -2
    )
    expect_all_equal(rolling_slope(x_desc, span = 2, partial = TRUE), -2)
})

test_that("rolling_slope works with width using roll_lm", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)

    ## width = 3, center aligned
    result <- rolling_slope(
        x,
        width = 3,
        align = "center",
        verbose = FALSE
    )
    expect_equal(length(result), length(x))
    expect_all_true(is.na(result[c(1, 10)])) ## boundary < width
    expect_all_false(is.na(result[-c(1, 10)]))
    expect_equal(result[5], slope(x[4:6])) # center window

    ## width = 3, left aligned
    result_left <- rolling_slope(
        x,
        width = 3,
        align = "left",
        verbose = FALSE
    )
    expect_equal(length(result_left), length(x))
    expect_all_true(is.na(result_left[9:10])) ## boundary < width
    expect_all_false(is.na(result_left[-c(9:10)]))
    expect_equal(result_left[5], slope(x[5:7]))

    ## width = 3, right aligned
    result_right <- rolling_slope(
        x,
        width = 3,
        align = "right",
        verbose = FALSE
    )
    expect_equal(length(result_right), length(x))
    expect_all_true(is.na(result_right[1:2])) ## boundary < width
    expect_all_false(is.na(result_right[-c(1:2)]))
    expect_equal(result_right[5], slope(x[3:5]))
})

test_that("rolling_slope works with width using compute_local_fun", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)

    ## width = 3, center aligned
    result <- rolling_slope(
        x,
        width = 3,
        span = 0,
        align = "center",
        verbose = FALSE
    )
    expect_equal(length(result), length(x))
    expect_all_true(is.na(result[c(1, 10)])) ## boundary < width
    expect_all_false(is.na(result[-c(1, 10)]))
    expect_equal(result[5], slope(x[4:6])) # center window

    ## width = 3, left aligned
    result_left <- rolling_slope(
        x,
        width = 3,
        span = 0,
        align = "left",
        verbose = FALSE
    )
    expect_equal(length(result_left), length(x))
    expect_all_true(is.na(result_left[9:10])) ## boundary < width
    expect_all_false(is.na(result_left[-c(9:10)]))
    expect_equal(result_left[5], slope(x[5:7]))

    ## width = 3, right aligned
    result_right <- rolling_slope(
        x,
        width = 3,
        span = 0,
        align = "right",
        verbose = FALSE
    )
    expect_equal(length(result_right), length(x))
    expect_all_true(is.na(result_right[1:2])) ## boundary < width
    expect_all_false(is.na(result_right[-c(1:2)]))
    expect_equal(result_right[5], slope(x[3:5]))
})

test_that("rolling_slope works with span using compute_local_fun", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)

    ## span = 7, center aligned
    ## TODO span < 7 centered effectively returns partial = TRUE
    result <- rolling_slope(
        x,
        span = 7,
        align = "center",
        verbose = FALSE
    )
    expect_equal(length(result), length(x))
    expect_all_true(is.na(result[c(1, 10)])) ## boundary < width
    expect_all_false(is.na(result[-c(1, 10)]))
    expect_equal(result[5], slope(x[2:8])) # center window

    ## span = 7, left aligned
    result_left <- rolling_slope(
        x,
        span = 7,
        align = "left",
        verbose = FALSE
    )
    expect_equal(length(result_left), length(x))
    expect_all_true(is.na(result_left[7:10])) ## boundary < width
    expect_all_false(is.na(result_left[-c(7:10)]))
    expect_equal(result_left[3], slope(x[3:10]))

    ## span = 7, right aligned
    result_right <- rolling_slope(
        x,
        span = 7,
        align = "right",
        verbose = FALSE
    )
    expect_equal(length(result_right), length(x))
    expect_all_true(is.na(result_right[1:4])) ## boundary < width
    expect_all_false(is.na(result_right[-c(1:4)]))
    expect_equal(result_right[7], slope(x[1:7]))
})

test_that("rolling_slope handles width larger than data", {
    x <- c(1, 3, 2, 5, 8)

    ## width > n
    result <- rolling_slope(x, width = 10)
    expect_all_true(is.na(result))

    ## width > n but partial = TRUE
    result <- rolling_slope(x, width = 10, partial = TRUE)
    expect_equal(length(result), length(x))
    expect_equal(var(result), 0) ## all same slope (full data)
})

test_that("rolling_slope calculates identity", {
    ## roll_lm width
    expect_all_equal(rolling_slope(x=1:5, t=1:5, width = 3, partial = TRUE), 1)
    expect_equal(
        rolling_slope(1:5, 1:5, width = 3),
        c(NA, rep(1, 3), NA)
    )
    ## compute_local_fun width
    expect_all_equal(
        rolling_slope(1:5, 1:5, width = 3, span = 0, partial = TRUE),
        1
    ) |>
        expect_message("width.*span")
    expect_equal(
        rolling_slope(1:5, 1:5, width = 3, span = 0),
        c(NA, rep(1, 3), NA)
    ) |>
        expect_message("width.*span")
    ## compute_local_fun span
    expect_all_equal(rolling_slope(1:5, 1:5, span = 2, partial = TRUE), 1)
    
    expect_warning(rolling_slope(1:5, 1:5, span = 1), "Less than.*2.*valid")
})

# fmt: skip
test_that("rolling_slope calculates all identical values", {
    expect_all_equal(rolling_slope(rep(5, 5), width = 3, partial = TRUE), 0)
    expect_equal(
        rolling_slope(rep(5, 5), width = 3),
        c(NA, rep(0, 3), NA)
    )
    ## all t values identical = NA
    expect_all_true(is.na(rolling_slope(1:5, rep(1, 5), width = 3)))
    
    expect_all_equal(
        rolling_slope(
            rep(5, 5), width = 3, span = 0, partial = TRUE, verbose = FALSE
        ),
        0
    )
    expect_equal(
        rolling_slope(rep(5, 5), width = 3, span = 0, verbose = FALSE),
        c(NA, rep(0, 3), NA)
    )
    expect_all_true(is.na(
        rolling_slope(1:5, rep(1, 5), width = 3, span = 0, verbose = FALSE)
    ))
    expect_all_true(is.na(rolling_slope(
        1:5, rep(1, 5), width = 3, span = 0, verbose = FALSE
    )))
    
    expect_all_equal(rolling_slope(rep(5, 5), span = 3, partial = TRUE), 0)
    expect_equal(
        ## TODO span < 7 effectively partial = TRUE
        rolling_slope(rep(5, 7), span = 7),
        c(NA, rep(0, 5), NA)
    )
    expect_all_true(is.na(rolling_slope(
        1:7, rep(1, 7), span = 3, verbose = FALSE
    )))
})

test_that("rolling_slope calculates invalid", {
    ## single value
    expect_true(is.na(rolling_slope(x = 5, width = 3)))
    ## non numeric NA errors
    expect_error(rolling_slope(x = rep(NA, 5), width = 3), "valid.*numeric")
    ## NA_real_ = NA_real_
    expect_all_true(is.na(rolling_slope(x = rep(NA_real_, 5), width = 3)))
    ## TODO empty returns empty or NA?
    expect_equal(length(rolling_slope(numeric(0), width = 3)), 0)
    ## invalid type = error
    expect_error(rolling_slope(list(), width = 3), "valid.*numeric")
    expect_error(rolling_slope(NULL, width = 3), "valid.*numeric")
    ## NA_real_, NaN, Inf = NA_real_
    expect_all_true(is.na(rolling_slope(rep(NA_real_, 4), width = 3)))
    expect_all_true(is.na(
        rolling_slope(rep(NA_real_, 4), na.rm = TRUE, width = 3)
    ))
    expect_all_true(is.na(rolling_slope(rep(NaN, 4), width = 3)))
    expect_all_true(is.na(rolling_slope(rep(Inf, 4), width = 3)))
    ## charater = error
    expect_error(rolling_slope(x = letters[1:4]), "valid.*numeric")
    expect_error(rolling_slope(1:4, t = letters[1:4]), "valid.*numeric")
    ## unequal length = error
    expect_error(rolling_slope(1:5, 1:3), "equal length")
})

test_that("rolling_slope handles NaN & Inf", {
    ## NaN propagates
    expect_all_true(is.nan(rolling_slope(
        x = c(1, NaN, 2, Inf, 8),
        width = 3,
        partial = TRUE,
        na.rm = FALSE
    )))
    expect_all_true(is.nan(rolling_slope(
        x = c(1, NaN, 2, Inf, 8),
        span = 2,
        na.rm = FALSE
    )))

    # rolling_slope(
    #     x <- c(1, 3, NaN, 5, 8, Inf, 9, 12),
    #     width = 3,
    #     partial = TRUE,
    #     na.rm = TRUE
    # )

    ## ! reconcile different results between methods
    ## na.rm removes NaN & Inf
    expect_equal(
        rolling_slope(
            x = c(1, 2, NaN, 4, 5, Inf, 7, 8),
            width = 2,
            na.rm = TRUE
        ),
        c(1, NA, NA, 1, NA, NA, NA, NA)
    )
    expect_equal(
        rolling_slope(
            x = c(1, 2, NaN, 4, 5, Inf, 7, 8),
            width = 2,
            span = 0,
            na.rm = TRUE,
            verbose = FALSE
        ),
        c(1, NA, NA, 1, NA, NA, 1, NA)
    )
    expect_all_equal(
        rolling_slope(
            x = c(1, 2, NaN, 4, 5, Inf, 7, 8),
            span = 2,
            na.rm = TRUE
        ),
        1
    )
})

test_that("rolling_slope span returns same as lm model", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
    
    ## span
    result <- rolling_slope(x, span = 3, partial = TRUE)
    t <- seq_along(x)
    span <- 3
    n <- length(x)
    lm_result <- sapply(t, \(.x) {
        start_t <- max(t[1], .x - span / 2)
        end_t <- min(t[n], .x + span / 2)
        window_idx <- which(t >= start_t & t <= end_t)
        # ## exception from lm() NA handling for all NA
        if (sum(!is.na(x[window_idx])) > 1) {
            coef(lm(x[window_idx] ~ t[window_idx]))[[2L]]
        } else {NA}
    })

    expect_equal(result, lm_result)

    ## width roll_lm with NA edges
    result <- rolling_slope(x, width = 3)
    width <- 3
    lm_result <- sapply(t, \(.x) {
        start_idx <- .x - floor(width / 2)
        end_idx <- .x + floor(width / 2)
        window_idx <- start_idx:end_idx
        ## exception from lm() NA handling for all NA
        if (all(window_idx >= 1 & window_idx <= n)) {
            coef(lm(x[window_idx] ~ t[window_idx]))[[2L]]
        } else {
            NA
        }        
    })

    expect_equal(result, lm_result)

    ## visual confirmation
    # library(ggplot2)
    # tibble(x = t, y = .env$x) |> 
    #     ggplot(aes(x, y)) +
    #     scale_x_continuous(breaks = scales::breaks_pretty(n=10)) +
    #     geom_line() +
    #     geom_point(aes(y = result, colour = "rolling_slope"),
    #                shape = 21, size = 5, stroke = 1) +
    #     geom_point(aes(y = lm_result, colour = "lm"),
    #                shape = 21, size = 3, stroke = 1)
})

# fmt: skip
test_that("rolling_slope returns same as zoo::rollapply()", {
    skip_if_not_installed("zoo")
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
    coef.fn <- \(x) coef(lm(x ~ seq_along(x)))[[2L]]

    ## align = "center"
    rolling_slope_center <- rolling_slope(
        x, width = 3, partial = TRUE, align = "center"
    )
    rollapply_center <- zoo::rollapply(
        x,
        FUN = coef.fn,
        width = 3,
        align = "center",
        partial = TRUE
    )
    expect_equal(rollapply_center, rolling_slope_center)
    rolling_slope_center <- rolling_slope(
        x, width = 3, span = 0, partial = TRUE, align = "center", verbose = FALSE
    )
    expect_equal(rollapply_center, rolling_slope_center)
    
    ## align = "left"
    rolling_slope_left <- rolling_slope(
        x, width = 3, partial = TRUE, align = "left"
    )
    rollapply_left <- zoo::rollapply(
        x,
        FUN = coef.fn,
        width = 3,
        align = "left",
        partial = TRUE
    )
    expect_equal(rollapply_left, rolling_slope_left)
    rolling_slope_left <- rolling_slope(
        x, width = 3, span = 0, partial = TRUE, align = "left", verbose = FALSE
    )
    expect_equal(rollapply_left, rolling_slope_left)
    
    ## align = "right"
    rolling_slope_right <- rolling_slope(
        x, width = 3, partial = TRUE, align = "right"
    )
    rollapply_right <- zoo::rollapply(
        x,
        FUN = coef.fn,
        width = 3,
        align = "right",
        partial = TRUE
    )
    expect_equal(rollapply_right, rolling_slope_right)
    rolling_slope_right <- rolling_slope(
        x, width = 3, span = 0, partial = TRUE, align = "right", verbose = FALSE
    )
    expect_equal(rollapply_right, rolling_slope_right)
})

# test_that("slope handles NA values correctly", {
#     x_na <- c(1, NA, 3, 4, 5)

#     # NA propagates by default
#     expect_true(is.na(slope(x_na)))
#     # na.rm removes NA observations
#     expect_equal(slope(x_na, na.rm = TRUE), 1)

#     # NA in t
#     t_na <- c(1, 2, NA, 4, 5)
#     expect_true(is.na(slope(1:5, t_na)))
#     expect_equal(
#         slope(1:5, t_na, na.rm = TRUE),
#         unname(coef(lm(1:5 ~ t_na))[2])
#     )

#     # NA in both - pairwise complete
#     expect_equal(
#         slope(x_na, t_na, na.rm = TRUE),
#         slope(c(1, 4, 5), t = c(1, 4, 5))
#     )
# })


# fmt: skip
test_that("rolling_slope handles NA with na.rm", {
    x_na = c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
    w <- 3
    df <- data.frame(
        x = x_na,
        span = rolling_slope(x_na, span = w - 1, partial = TRUE),
        width = rolling_slope(x_na, width = w, span = 0, partial = TRUE, verbose = FALSE),
        ## TODO roll_lm DOES NOT RETURN SAME if na.rm = FALSE, need to use compute_local_fun width
        # roll_lm = rolling_slope(x_na, width = w, partial = TRUE),
        span_na = rolling_slope(x_na, span = w - 1, partial = TRUE, na.rm = TRUE),
        width_na = rolling_slope(x_na, width = w , span = 0, partial = TRUE, na.rm = TRUE, verbose = FALSE),
        roll_lm_na = rolling_slope(x_na, width = w, partial = TRUE, na.rm = TRUE)
    )
    expect_equal(df$span, df$width)
    # expect_equal(df$span, df$roll_lm)
    # expect_equal(df$width, df$roll_lm)
    expect_equal(df$span_na, df$width_na)
    expect_equal(df$span_na, df$roll_lm_na)
    expect_equal(df$width_na, df$roll_lm_na)

    ## 2025-08-13 na.rm = FALSE returns NA where any local x is NA
    result <- rolling_slope(x_na, width = 3, na.rm = FALSE)
    expect_equal(which(!is.na(result)), 5:7)

    ## 2025-08-13 na.rm = TRUE returns NA where target x is NA
    result <- rolling_slope(x_na, width = 3, partial = TRUE, na.rm = TRUE)
    expect_equal(which(is.na(result)), 9:11)
})

test_that("rolling_slope handles span in units of t", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    t <- seq_along(x)
    t_5hz <- t / 5

    expect_equal(
        rolling_slope(x, t, span = 5, partial = TRUE) * 5,
        rolling_slope(x, t_5hz, span = 1, partial = TRUE)
    )

    t <- c(0.5, 1.5, 3, 3.5, 4, 5.5, 7, 8, 10, 11)
    n <- length(x)
    span <- 3
    result <- rolling_slope(x, t, span = span, partial = TRUE)

    lm_result <- sapply(seq_len(n), \(.x) {
        current_t <- t[.x]
        start_t <- max(t[1L], current_t - span / 2)
        end_t <- min(t[n], current_t + span / 2)
        window_idx <- which(t >= start_t & t <= end_t)
        
        coef(lm(x[window_idx] ~ t[window_idx]))[[2L]]
    })
    
    expect_equal(result, lm_result)

    x <- c(1, 3, 2, 5, 8)
    ## span & min_obs > n = NA
    result <- rolling_slope(x, span = 10)
    expect_length(result, length(x))
    expect_all_true(is.na(result))
    ## min_obs < n (partial) = identical slope values
    result <- rolling_slope(x, span = 10, partial = TRUE)
    expect_length(result, length(x))
    expect_equal(var(result), 0)
})

test_that("rolling_slope handles irregular time series with span", {
    x <- c(1, 3, 2, 5, 8)
    t <- c(0, 1, 3, 6, 10)
    span <- 4
    idx_list <- list()
    coef_result <- rep(NA, length(t))
    slope_result <- rep(NA, length(t))
    for (i in seq_along(t)) {
        t_idx <- t[i]
        idx <- t >= t_idx & t <= t_idx + span
        idx_list[[i]] <- which(idx)
        coef_result[i] <- coef(lm(x[idx] ~ t[idx]))[[2L]]
        slope_result[i] <- mnirs:::slope(x[idx], t[idx])
    }

    result <- rolling_slope(
        x,
        t,
        span = 4,
        align = "left",
        partial = TRUE
    )
    expect_equal(length(result), length(x))
    expect_equal(result, coef_result)
    expect_equal(result, slope_result)
})

test_that("rolling_slope handles invalid args", {
    ## TODO width = 1 = NA or error?
    expect_all_true(is.na(rolling_slope(1:5, width = 1)))
    ## TODO span results in width = 1 = NA or error?
    expect_all_true(is.na(rolling_slope(1:5, span = 0)))
    
    expect_error(rolling_slope(1:5), "width.*span.*must be defined")
    expect_error(rolling_slope(1:5, span = NA), "valid.*numeric")
    expect_error(rolling_slope(1:5, width = NA), "valid.*integer")
    expect_error(rolling_slope(1:5, span = -1), "valid.*numeric")
    expect_error(rolling_slope(1:5, width = -1), "valid.*integer")
    
    ## invalid align
    expect_error(
        rolling_slope(1:5, width = 3, align = "invalid"),
        "arg.*should be one of"
    )
})

test_that("rolling_slope works with partial", {
    x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 14)
    expect_all_true(rolling_slope(x, width = 3, partial = TRUE) >= 0)
    expect_all_true(
        is.na(rolling_slope(x, width = 3, partial = FALSE)[c(1, 10)])
    )

    ## n < 2 = NA regardless of partial
    expect_true(is.na(rolling_slope(x[1L], width = 3, partial = TRUE)))
})

## integration ============================================================
test_that("rolling_slope works visually", {
    # x <- c(1, 3, 2, 5, 8, 7, 9, 12, 11, 15, 14, 17, 18)
    x <- c(1, 3, NA, 5, 8, 7, 9, 12, NA, NA, NA, 17, 18)
    t <- seq_along(x)
    window_idx <- compute_local_windows(t, width = 3)

    ## partial = TRUE, na.rm = TRUE
    slopes <- vapply(window_idx, \(.idx) {
        df <- data.frame(t = t[.idx], x = x[.idx])
        tryCatch(coef(lm(x ~ t, df))[[2L]], error = \(e) NA_real_)
        }, numeric(1))

    expect_equal(
        rolling_slope(x, t, width = 3, partial = TRUE, na.rm = TRUE),
        slopes
    )

    ## partial = FALSE, na.rm = FALSE
    slopes <- vapply(window_idx, \(.idx) {
        df <- data.frame(t = t[.idx], x = x[.idx])
        if (length(df$t) < 3 || any(is.na(df$x))) {
            return(NA_real_)
        }
        tryCatch(coef(lm(x ~ t, df))[[2L]], error = \(e) NA_real_)
        }, numeric(1))

    expect_equal(
        rolling_slope(x, t, width = 3, partial = FALSE, na.rm = FALSE),
        slopes
    )

    ## visual confirmation
    # library(ggplot2)
    # predicted <- lapply(window_idx, \(.idx) {
    #     # coef(lm(x[.idx] ~ t[.idx]))[[2L]]
    #     df <- data.frame(t = t[.idx], x = x[.idx])
    #     y <- tryCatch(predict(lm(x ~ t, df), df), error = \(e) NA_real_)
    #     # df$y <- if (all(is.na(y)) || all.equal(y)) NA_real_ else y
    #     df$y <- if (!is.null(attr(y, "non-estim"))) NA_real_ else y
    #     df
    # })[2:12]

    # data.frame(t, x) |>
    #     ggplot(aes(t, x)) +
    #     theme_mnirs() +
    #     geom_line(linewidth = 2) +
    #     lapply(predicted, \(.df) {
    #         list(
    #             geom_line(
    #                 data = .df,
    #                 aes(t, y),
    #                 colour = "dodgerblue"
    #             ),
    #             geom_point(
    #                 data = .df,
    #                 aes(t, y),
    #                 colour = c("dodgerblue", "red", "dodgerblue"),
    #                 shape = c(19, 18, 19),
    #                 size = c(2, 5, 2)
    #             )
    #         )
    #     }) +
    #     NULL
})

test_that("rolling_slope works on example_mnirs() data", {
    skip("visual checks")
    
    data <- read_mnirs(
        example_mnirs("moxy_ramp"),
        nirs_channels = c(
            smo2_right = "SmO2 Live(2)"
        ),
        time_channel = c(time = "hh:mm:ss"),
        verbose = FALSE
    ) |> 
        filter_mnirs(method = "butterworth", order = 4, W = 0.05)

    data$slope <- rolling_slope(
        x = data$smo2_right,
        t = data$time,
        span = 10,
        partial = FALSE,
        na.rm = TRUE
    )

    plot(data) +
        ggplot2::geom_hline(
            yintercept = 50,
            linetype = "dotted",
            colour = "red"
        ) +
        ggplot2::geom_line(ggplot2::aes(y = 50 + slope * 10), colour = "red")

    data <- read_mnirs(
        example_mnirs("train.red"),
        nirs_channels = c(
            smo2_right = "SmO2 unfiltered"
        ),
        time_channel = c(time = "Timestamp (seconds passed)"),
        verbose = FALSE
    ) |>
        filter_mnirs(method = "butterworth", order = 4, W = 0.05)

    data$slope <- rolling_slope(
        x = data$smo2_right,
        t = data$time,
        span = 10,
        partial = FALSE,
        na.rm = TRUE
    )

    plot(data) +
        ggplot2::geom_hline(
            yintercept = 65,
            linetype = "dotted",
            colour = "red"
        ) +
        ggplot2::geom_line(ggplot2::aes(y = 65 + slope * 10), colour = "red")
})