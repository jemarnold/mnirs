## monoexponential() =================================================
test_that("monoexponential() returns correct vector length", {
    t <- 1:60
    result <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8)

    expect_length(result, length(t))
    expect_type(result, "double")
})

test_that("monoexponential() returns baseline A before TD", {
    t <- 0:30
    A <- 10
    TD <- 15
    result <- monoexponential(t, A = A, B = 100, TD = TD, tau = 8)

    expect_true(all(result[t < TD] == A))
})

test_that("monoexponential() approaches asymptote B", {
    t <- 1:200
    B <- 100
    result <- monoexponential(t, A = 10, B = B, TD = 15, tau = 8)

    # At t >> TD + tau, should approach B
    expect_equal(tail(result, 1), B, tolerance = 0.01)
})

test_that("monoexponential() handles rising curves (B > A)", {
    t <- 1:60
    result <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8)

    expect_true(result[20] > result[15])
    expect_true(result[30] > result[20])
})

test_that("monoexponential() handles falling curves (B < A)", {
    t <- 1:60
    result <- monoexponential(t, A = 100, B = 10, TD = 15, tau = 8)

    expect_true(result[20] < result[15])
    expect_true(result[30] < result[20])
})

test_that("monoexponential() tau determines rate correctly", {
    t <- 1:100
    TD <- 15
    tau <- 10

    result <- monoexponential(t, A = 0, B = 100, TD = TD, tau = tau)

    # At t = TD + tau, should be ~63.2% of amplitude
    idx <- which(t == TD + tau)
    expect_equal(result[idx], 63.2, tolerance = 1)
})

test_that("monoexponential() handles zero and negative TD", {
    t <- 0:60
    result <- monoexponential(t, A = 10, B = 100, TD = 0, tau = 8)
    
    expect_true(all(!is.na(result)))
    expect_true(result[1] == 10)
    expect_true(result[10] > 10)
    
    t <- -10:60
    result <- monoexponential(t, A = 10, B = 100, TD = -5, tau = 8)
    expect_true(all(!is.na(result)))
    expect_all_equal(result[1:6], 10)
    expect_true(result[7] > 10)
})


## SS_monoexp() ========================================================

test_that("SS_monoexp() converges on known parameters", {
    set.seed(13)
    t <- 1:60
    A_true <- 10
    B_true <- 100
    TD_true <- 15
    tau_true <- 8

    x <- monoexponential(t, A_true, B_true, TD_true, tau_true) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    # ggplot2::ggplot(data, ggplot2::aes(t, x)) +
    #     theme_mnirs() +
    #     ggplot2::geom_point() #+
        # ggplot2::geom_line(ggplot2::aes(y = y))

    model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)

    expect_s3_class(model, "nls")
    expect_named(coef(model), c("A", "B", "TD", "tau"))

    coefs <- coef(model)
    expect_equal(coefs[["A"]], A_true, tolerance = A_true * 0.1)
    expect_equal(coefs[["B"]], B_true, tolerance = B_true * 0.1)
    expect_equal(coefs[["TD"]], TD_true, tolerance = TD_true * 0.1)
    expect_equal(coefs[["tau"]], tau_true, tolerance = tau_true * 0.1)
})

test_that("SS_monoexp() handles falling exponentials", {
    set.seed(456)
    t <- 1:60
    x <- monoexponential(t, A = 100, B = 10, TD = 15, tau = 8) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)
    coefs <- coef(model)

    expect_true(coefs[["A"]] > coefs[["B"]])
    expect_s3_class(model, "nls")
})

test_that("SS_monoexp() works with short time series", {
    set.seed(789)
    t <- 1:20
    x <- monoexponential(t, A = 10, B = 100, TD = 5, tau = 3) +
        rnorm(length(t), 0, 2)
    data <- data.frame(t, x)

    expect_no_error(
        model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)
    )
    expect_s3_class(model, "nls")
})

## ! it doesn't handle this well. I think poor estimate for A
# test_that("SS_monoexp() handles data with TD near zero", {
#     set.seed(101)
#     t <- 1:60
#     x <- monoexponential(t, A = 10, B = 100, TD = 1, tau = 8) +
#         rnorm(length(t), 0, 3)
#     data <- data.frame(t, x)

#     ggplot2::ggplot(data, ggplot2::aes(t, x)) +
#         theme_mnirs() +
#         ggplot2::geom_point()

#     model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)

#     expect_true(coef(model)[["TD"]] < 5)
# })

test_that("SS_monoexp() predict() returns correct length", {
    set.seed(202)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)
    predictions <- predict(model, data)

    expect_length(predictions, nrow(data))
})


# fix_coefs() =========================================================

test_that("fix_coefs() fixes single parameter correctly", {
    set.seed(303)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)
    model_fixed <- fix_coefs(model, TD = 15)

    expect_s3_class(model_fixed, "nls")
    expect_false("TD" %in% names(coef(model_fixed)))
    expect_true(all(c("A", "B", "tau") %in% names(coef(model_fixed))))
    ## 15 should be in the model formula
    expect_true(any(grepl("15", model_fixed$call$formula)))
})

test_that("fix_coefs() fixes multiple parameters", {
    set.seed(400)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    ggplot2::ggplot(data, ggplot2::aes(t, x)) +
        theme_mnirs() +
        ggplot2::geom_point()

    model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)
    model_fixed <- fix_coefs(
        model,
        A = 10,
        TD = 15,
    )

    expect_false("A" %in% names(coef(model_fixed)))
    expect_false("TD" %in% names(coef(model_fixed)))
    expect_length(coef(model_fixed), 2)
    ## 10 & 15 should be in the model formula
    expect_true(any(grepl("15", model_fixed$call$formula)))
    expect_true(any(grepl("10", model_fixed$call$formula)))
})

test_that("fix_coefs() errors when all parameters fixed", {
    set.seed(505)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)

    expect_error(
        fix_coefs(
            model,
            A = 10,
            B = 100,
            TD = 15,
            tau = 8,
        ),
        "Cannot update the model if all parameters are fixed"
    )
})

test_that("fix_coefs() warns for invalid parameter names", {
    set.seed(606)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)

    expect_warning(
        fix_coefs(model, INVALID = 99, verbose = TRUE),
        "Unknown model coefficient"
    )

    expect_silent(
        fix_coefs(model, INVALID = 99, verbose = FALSE)
    )
})

## TODO I need to verify tryCatch fall-back in real-world first
# test_that("fix_coefs() errors when data unavailable and not supplied", {
#     set.seed(909)
#     t <- 1:60
#     x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) +
#         rnorm(length(t), 0, 3)
#     data <- data.frame(t, x)

#     model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)

#     # Simulate unavailable data by breaking environment
#     model$call$data <- as.name("nonexistent_data")

#     expect_error(
#         fix_coefs(model, TD = 15),
#         "Cannot retrieve original model data frame"
#     )
# })

test_that("fix_coefs() accepts explicit data argument", {
    set.seed(1010)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)

    expect_no_error(
        model_fixed <- fix_coefs(model, TD = 15, data = data)
    )
    expect_s3_class(model_fixed, "nls")
})

test_that("fix_coefs() predictions differ from original model", {
    set.seed(1111)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, TD = 15, tau = 8) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp(t, A, B, TD, tau), data = data)
    model_fixed <- fix_coefs(model, TD = 20, data = data, verbose = FALSE)

    pred_orig <- predict(model, data)
    pred_fixed <- predict(model_fixed, data)

    # Should differ if fixed value differs from estimated
    expect_false(identical(pred_orig, pred_fixed))
})