## monoexponential() =================================================
test_that("monoexponential() returns correct vector length", {
    t <- 1:60
    result <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15)

    expect_length(result, length(t))
    expect_type(result, "double")
})

test_that("monoexponential() returns baseline A before TD", {
    t <- 0:30
    A <- 10
    TD <- 15
    result <- monoexponential(t, A = A, B = 100, tau = 8, TD = TD)

    expect_true(all(result[t < TD] == A))
})

test_that("monoexponential() approaches asymptote B", {
    t <- 1:200
    B <- 100
    result <- monoexponential(t, A = 10, B = B, tau = 8, TD = 15)

    # At t >> TD + tau, should approach B
    expect_equal(tail(result, 1), B, tolerance = 0.01)
})

test_that("monoexponential() handles rising curves (B > A)", {
    t <- 1:60
    result <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15)

    expect_true(result[20] > result[15])
    expect_true(result[30] > result[20])
})

test_that("monoexponential() handles falling curves (B < A)", {
    t <- 1:60
    result <- monoexponential(t, A = 100, B = 10, tau = 8, TD = 15)

    expect_true(result[20] < result[15])
    expect_true(result[30] < result[20])
})

test_that("monoexponential() tau determines rate correctly", {
    t <- 1:100
    TD <- 15
    tau <- 10

    result <- monoexponential(t, A = 0, B = 100, tau = tau, TD = TD)

    # At t = TD + tau, should be ~63.2% of amplitude
    idx <- which(t == TD + tau)
    expect_equal(result[idx], 63.2, tolerance = 1)
})

test_that("monoexponential() handles zero and negative TD", {
    t <- 0:60
    result <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 0)
    
    expect_true(all(!is.na(result)))
    expect_true(result[1] == 10)
    expect_true(result[10] > 10)
    
    t <- -10:60
    result <- monoexponential(t, A = 10, B = 100, tau = 8, TD = -5)
    expect_true(all(!is.na(result)))
    expect_all_equal(result[1:6], 10)
    expect_true(result[7] > 10)
})


## SS_monoexp4() ========================================================

test_that("SS_monoexp4() converges on known parameters", {
    set.seed(13)
    t <- 1:60
    A_true <- 10
    B_true <- 100
    TD_true <- 15
    tau_true <- 8

    x <- monoexponential(t, A_true, B_true, tau_true, TD_true) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    # ggplot2::ggplot(data, ggplot2::aes(t, x)) +
    #     theme_mnirs() +
    #     ggplot2::geom_point() #+
        # ggplot2::geom_line(ggplot2::aes(y = y))

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)

    expect_s3_class(model, "nls")
    expect_named(coef(model), c("A", "B", "tau", "TD"))

    coefs <- coef(model)
    expect_equal(coefs[["A"]], A_true, tolerance = A_true * 0.1)
    expect_equal(coefs[["B"]], B_true, tolerance = B_true * 0.1)
    expect_equal(coefs[["tau"]], tau_true, tolerance = tau_true * 0.1)
    expect_equal(coefs[["TD"]], TD_true, tolerance = TD_true * 0.1)
})

test_that("SS_monoexp4() handles falling exponentials", {
    set.seed(456)
    t <- 1:60
    x <- monoexponential(t, A = 100, B = 10, tau = 8, TD = 15) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)
    coefs <- coef(model)

    expect_true(coefs[["A"]] > coefs[["B"]])
    expect_s3_class(model, "nls")
})

test_that("SS_monoexp4() works with short time series", {
    set.seed(789)
    t <- 1:20
    x <- monoexponential(t, A = 10, B = 100, tau = 3, TD = 5) +
        rnorm(length(t), 0, 2)
    data <- data.frame(t, x)

    expect_no_error(
        model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)
    )
    expect_s3_class(model, "nls")
})

test_that("SS_monoexp4() predict() returns correct length", {
    set.seed(202)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)
    predictions <- predict(model, data)

    expect_length(predictions, nrow(data))
})

## SS_monoexp3() ========================================================
test_that("SS_monoexp3() converges on known parameters", {
    set.seed(13)
    t <- 1:60
    A_true <- 10
    B_true <- 100
    TD_true <- 0
    tau_true <- 8

    x <- monoexponential(t, A_true, B_true, tau_true, TD_true) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    # ggplot2::ggplot(data, ggplot2::aes(t, x)) +
    #     theme_mnirs() +
    #     ggplot2::geom_point() #+
    # ggplot2::geom_line(ggplot2::aes(y = y))

    model <- nls(x ~ SS_monoexp3(t, A, B, tau), data = data)

    expect_s3_class(model, "nls")
    expect_named(coef(model), c("A", "B", "tau"))

    coefs <- coef(model)
    expect_equal(coefs[["A"]], A_true, tolerance = A_true * 0.1)
    expect_equal(coefs[["B"]], B_true, tolerance = B_true * 0.1)
    expect_equal(coefs[["tau"]], tau_true, tolerance = tau_true * 0.1)
    expect_disjoint(names(coefs), "TD")
})

test_that("SS_monoexp3() handles falling exponentials", {
    set.seed(456)
    t <- 1:60
    x <- monoexponential(t, A = 100, B = 10, tau = 8, TD = NULL) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp3(t, A, B, tau), data = data)
    coefs <- coef(model)

    expect_true(coefs[["A"]] > coefs[["B"]])
    expect_s3_class(model, "nls")
    expect_disjoint(names(coefs), "TD")
})

test_that("SS_monoexp3() handles data with TD near zero", {
    set.seed(101)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 1) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    # ggplot2::ggplot(data, ggplot2::aes(t, x)) +
    #     theme_mnirs() +
    #     ggplot2::geom_point()

    ## ! SS_monoexp4() fails for this test. Would a better initialisation succeed?
    expect_no_error(
        model <- nls(x ~ SS_monoexp3(t, A, B, tau), data = data)
    )

    expect_s3_class(model, "nls")
    expect_equal(coef(model)[["tau"]], 8, tolerance = 1)
    expect_disjoint(names(coef(model)), "TD")
})

## TODO add repeated occlusion tests with few data samples

test_that("SS_monoexp3() handles OxCap with few data points same as SSasymp", {
    # fmt: skip
    df <- data.frame(
        time = c(
            0, 15.1, 30.8, 45.8, 61, 76, 91.2, 106.3, 121.1, 135.9, 151.1, 
            165.8, 181, 196.1, 211.1, 225.8
        ),
        slope = c(
            2.5762, 1.3261, 0.8707, 0.733, 0.4198, 0.3614, 0.2806, 0.3069, 
            0.3728, 0.4752, 0.5505, 0.5538, 0.5384, 0.5036, 0.4911, 0.5088
        )
    )

    expect_no_error(
        model <- nls(
            slope ~ SS_monoexp3(time, A, B, tau),
            data = df,
        )
    )

    model_asym <- nls(
        slope ~ SSasymp(time, Asym, R0, lrc),
        data = df,
    )
    asym_tau <- exp(-coef(model_asym)[["lrc"]])

    expect_equal(coef(model)[["A"]], coef(model_asym)[["R0"]], tolerance = 0.1)
    expect_equal(coef(model)[["B"]], coef(model_asym)[["Asym"]], tolerance = 0.1)
    expect_equal(coef(model)[["tau"]], asym_tau, tolerance = 0.1)
})

test_that("SS_monoexp3() handles OxCap with few data points better than SSasymp", {
    # fmt: skip
    df <- data.frame(
        time = c(0, 15.9, 30.9, 46, 60.9, 76, 91.5, 106.3, 121.2, 136.2, 151, 
            166.3, 181, 195.8, 211, 225.6),
        slope = c(
            2.5868, 1.1626, 0.6287, 0.3786, 0.162, 0.2219, 0.173, 0.1864, 
            0.3341, 0.2669, 0.3361, 0.4534, 0.3756, 0.4536, 0.3664, 0.4137
        )
      )

    expect_no_error(
        model <- nls(
            slope ~ SS_monoexp3(time, A, B, tau),
            data = df,
        )
    )

    ## ! SSasymp returns singular gradient
    expect_error(
        model_asym <- nls(
            slope ~ SSasymp(time, Asym, R0, lrc),
            data = df,
        ),
        "singular gradient"
    )
})



# fix_coefs() =========================================================

test_that("fix_coefs() fixes single parameter correctly", {
    set.seed(303)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)
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
    x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    ggplot2::ggplot(data, ggplot2::aes(t, x)) +
        theme_mnirs() +
        ggplot2::geom_point()

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)
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
    x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)

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
    x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)

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
#     x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
#         rnorm(length(t), 0, 3)
#     data <- data.frame(t, x)

#     model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)

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
    x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)

    expect_no_error(
        model_fixed <- fix_coefs(model, TD = 15, data = data)
    )
    expect_s3_class(model_fixed, "nls")
})

test_that("fix_coefs() predictions differ from original model", {
    set.seed(1111)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)
    model_fixed <- fix_coefs(model, TD = 20, data = data, verbose = FALSE)

    pred_orig <- predict(model, data)
    pred_fixed <- predict(model_fixed, data)

    # Should differ if fixed value differs from estimated
    expect_false(identical(pred_orig, pred_fixed))
})

## integration tests =============================================

test_that("extract model coefs", {
    set.seed(1111)
    t <- 1:60
    x <- monoexponential(t, A = 10, B = 100, tau = 8, TD = 15) +
        rnorm(length(t), 0, 3)
    data <- data.frame(t, x)

    model <- nls(x ~ SS_monoexp4(t, A, B, tau, TD), data = data)
    tau <- coef(model)[["tau"]]
    expect_equal(tau, 8, tolerance = 1)

})