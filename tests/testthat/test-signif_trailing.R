## signif_trailing() =========================================================
test_that("signif_trailing preserves trailing zeros with format = 'digits'", {
    x <- c(123, 123.4, 123.45, 123.456)

    result <- signif_trailing(x, digits = 2, format = "digits")
    expect_type(result, "character")
    expect_equal(result, c("123.00", "123.40", "123.45", "123.46"))
    expect_equal(
        signif_trailing(x, digits = 1, format = "digits"),
        c("123.0", "123.4", "123.4", "123.5")
    )
    expect_equal(
        signif_trailing(x, digits = 0, format = "digits"),
        c("123", "123", "123", "123")
    )
})

test_that("signif_trailing handles negative digits (rounds to integer place)", {
    x <- c(123, 123.4, 123.45)

    expect_equal(
        signif_trailing(x, digits = -1, format = "digits"),
        c("120", "120", "120")
    )
    expect_equal(signif_trailing(1234, digits = -2, format = "digits"), "1200")
    expect_equal(signif_trailing(1234, digits = -3, format = "digits"), "1000")
})

test_that("signif_trailing works with format = 'signif'", {
    x <- c(123, 123.4, 123.45, 123.456)

    expect_equal(
        signif_trailing(x, digits = 5, format = "signif"),
        c("123.00", "123.40", "123.45", "123.46")
    )
    expect_equal(
        signif_trailing(x, digits = 4, format = "signif"),
        c("123.0", "123.4", "123.4", "123.5")
    )
    expect_equal(
        signif_trailing(x, digits = 0, format = "signif"),
        c("123", "123", "123", "123")
    )
})

test_that("signif_trailing removes trailing decimal point", {
    expect_false(grepl(
        "\\.$",
        signif_trailing(123, digits = 0, format = "digits")
    ))
    expect_equal(signif_trailing(100, digits = 0, format = "signif"), "100")
})

test_that("signif_trailing handles NA values", {
    result <- signif_trailing(c(1.5, NA, 2.5), digits = 2)
    expect_equal(result, c("1.50", "", "2.50"))
})

test_that("signif_trailing handles special numeric values", {
    expect_equal(signif_trailing(0, digits = 2), "0.00")

    ## what do I want Inf, NaN to display?
    expect_equal(signif_trailing(Inf, digits = 2), "Inf")
    expect_equal(signif_trailing(-Inf, digits = 2), "-Inf")
    expect_error(signif_trailing(NaN), "valid.*numeric")
    expect_equal(signif_trailing(c(1, NaN), digits = 2), c("1.00", "NaN"))
})

## signif_whole() =========================================================
test_that("signif_whole returns correct significant figures", {
    x <- c(123, 123.4, 123.45, 123.456)
    result <- signif_whole(x, digits = 5)
    expect_type(result, "double")
    expect_equal(result, c(123, 123.4, 123.45, 123.46))
    expect_equal(signif_whole(x, digits = 4), c(123, 123.4, 123.4, 123.5))
    expect_equal(signif_whole(x, digits = 2), c(123, 123, 123, 123))
})

test_that("signif_whole uses banker's rounding", {
    # Round half to even behaviour from signif() and round()
    expect_equal(signif_whole(123.45, digits = 4), signif(123.45, 4))
    expect_equal(signif_whole(123.55, digits = 4), signif(123.55, 4))
    expect_equal(signif_whole(123.35, digits = 4), signif(123.35, 4))
    expect_equal(signif_whole(124.45, digits = 4), signif(124.45, 4))
})

test_that("signif_whole rounds to whole when digits <= whole part", {
    expect_equal(signif_whole(1234.5, digits = 2), 1234)
    expect_equal(signif_whole(1234.5, digits = 4), 1234)
    expect_equal(signif_whole(1234.6, digits = 4), 1235)
})

test_that("signif_whole treats negative digits as digits = 0", {
    expect_equal(signif_whole(123, digits = -5), 123)
    expect_equal(signif_whole(123.45, digits = -1), 123)
    expect_equal(
        signif_whole(c(123, 123.4, 123.45), digits = -1),
        c(123, 123, 123)
    )
})

test_that("signif_whole handles special values", {
    ## what do I want Inf, NaN to display?
    expect_equal(
        signif_whole(c(0, NA, NaN, Inf, -Inf), digits = 3),
        c(0, NA, NaN, Inf, -Inf)
    )
})

test_that("signif_whole handles small decimals", {
    expect_equal(
        signif_whole(0.001234, digits = 3),
        signif(0.001234, digits = 3)
    )
    expect_equal(
        signif_whole(0.001234, digits = -1),
        signif(0.001234, digits = -1)
    )
    expect_equal(
        signif_whole(0.001234, digits = -1),
        signif(0.001234, digits = -1)
    )
})

test_that("signif_whole handles negative numbers", {
    expect_equal(signif_whole(-123.45, digits = 3), -123)
    expect_equal(signif_whole(-123.45, digits = 4), -123.4)
})


## signif_pvalue() =========================================================
test_that("signif_pvalue formats values correctly with display = 'value'", {
    p_vals <- c(0.0001, 0.003, 0.02, 0.08, 0.15)

    result <- signif_pvalue(p_vals, digits = 3)
    expect_type(result, "character")
    expect_setequal(result, c("< 0.001", "0.003", "0.020", "0.080", "0.150"))
})

test_that("signif_pvalue respects digits for threshold", {
    expect_equal(signif_pvalue(0.005, digits = 2), "< 0.01")
    expect_equal(signif_pvalue(0.005, digits = 3), "0.005")
    expect_equal(signif_pvalue(0.0005, digits = 3), "< 0.001")
})

test_that("signif_pvalue uses alpha as threshold when digits = 1", {
    # Default alpha = 0.05
    expect_equal(signif_pvalue(0.03, digits = 1), "< 0.05")
    expect_equal(signif_pvalue(0.06, digits = 1), "0.06")

    # Custom alpha = 0.01
    expect_equal(signif_pvalue(0.005, digits = 1, alpha = 0.01), "< 0.01")
    expect_equal(signif_pvalue(0.005, digits = 2, alpha = 0.01), "< 0.01")
    expect_equal(signif_pvalue(0.02, digits = 1, alpha = 0.01), "0.02")
})

test_that("signif_pvalue returns symbols", {
    p_vals <- c(0.0001, 0.003, 0.02, 0.08, 0.15)

    result <- signif_pvalue(p_vals, display = "symbol")
    expect_equal(result, c("*", "*", "*", "", ""))

    ## repeats symbols
    result <- signif_pvalue(p_vals, display = "symbol", symbol_repeat = TRUE)
    expect_equal(result, c("***", "**", "*", "", ""))
})

test_that("signif_pvalue respects custom alpha", {
    p_vals <- c(0.005, 0.02, 0.08)

    result <- signif_pvalue(p_vals, display = "symbol", alpha = 0.01)
    expect_equal(result, c("*", "", ""))
})

test_that("signif_pvalue respects custom symbol", {
    result <- signif_pvalue(0.01, display = "symbol", symbol = "†")
    expect_equal(result, "†")

    result_repeat <- signif_pvalue(
        0.0001,
        display = "symbol",
        symbol = "†",
        symbol_repeat = TRUE
    )
    expect_equal(result_repeat, "†††")
})

test_that("signif_pvalue handles boundary values for symbol_repeat", {
    expect_equal(signif_pvalue(0.05, display = "symbol", alpha = 0.05), "")
    expect_equal(signif_pvalue(0.049, display = "symbol", alpha = 0.05), "*")
    expect_equal(
        signif_pvalue(0.001, display = "symbol", symbol_repeat = TRUE),
        "**"
    )
    expect_equal(
        signif_pvalue(0.0009, display = "symbol", symbol_repeat = TRUE),
        "***"
    )
    expect_equal(
        signif_pvalue(0.01, display = "symbol", symbol_repeat = TRUE),
        "*"
    )
    expect_equal(
        signif_pvalue(0.0099, display = "symbol", symbol_repeat = TRUE),
        "**"
    )
})

test_that("signif_pvalue handles edge case p-values", {
    expect_equal(signif_pvalue(0, digits = 3), "< 0.001")
    expect_equal(signif_pvalue(1, digits = 3), "1.000")
    expect_equal(signif_pvalue(0.9995, digits = 3), "1.000")

    ## what do I want Inf, NaN to display?
    expect_setequal(
        signif_pvalue(c(1, NA, NaN, Inf, -Inf)),
        c("1.000", NA, NA, "Inf", "< 0.001")
    )
})


## seq_range() =========================================================
test_that("seq_range generates correct ascending sequence", {
    x <- c(1.2, 3.7, 2.1, 4.9, 2.8)

    result <- seq_range(x, by = 1)
    expect_equal(result[1], 1.2)
    expect_lte(result[length(result)], max(x))
    expect_equal(diff(result), rep(1, length(result) - 1))
})

test_that("seq_range respects by argument", {
    x <- c(1, 5)

    result <- seq_range(x, by = 0.5)
    expect_equal(result, seq(1, 5, by = 0.5))

    result2 <- seq_range(x, by = 2)
    expect_equal(result2, c(1, 3, 5))
})

test_that("seq_range generates descending sequence", {
    x <- c(1, 5)

    result <- seq_range(x, direction = "down")
    expect_equal(result, 5:1)
})

test_that("seq_range rounds endpoints correctly", {
    x <- c(1.23, 4.87)

    result <- seq_range(x, by = 1, digits = 0)
    expect_equal(result[1], 1)
    expect_equal(result[length(result)], 5)

    result2 <- seq_range(x, by = 0.5, digits = 1)
    expect_equal(result2[1], 1.2)
    expect_equal(result2[length(result2)], 4.7)
})

test_that("seq_range handles edge cases", {
    ## NA values in input
    expect_equal(seq_range(c(1, NA, 5, NA)), 1:5)
    ## single value input
    expect_equal(seq_range(5, by = 1), 5)
    ## negative values
    expect_equal(seq_range(c(-5, -1)), -5:-1)
})


## wrap() ===============================================================
test_that("wrap rotates elements correctly", {
    x <- 1:5

    expect_equal(wrap(x, 0), 1:5)
    expect_equal(wrap(x, 1), c(2, 3, 4, 5, 1))
    expect_equal(wrap(x, 4), c(5, 1, 2, 3, 4))
})

test_that("wrap handles n larger than vector length", {
    x <- 1:5

    expect_equal(wrap(x, 5), 1:5)
    expect_equal(wrap(x, 6), wrap(x, 1))
    expect_equal(wrap(x, 7), wrap(x, 2))
    expect_equal(wrap(x, 12), wrap(x, 2))
})

test_that("wrap handles negative n", {
    x <- 1:5

    expect_equal(wrap(x, -1), c(5, 1, 2, 3, 4))
    expect_equal(wrap(x, -2), c(4, 5, 1, 2, 3))
})

test_that("wrap works with character vectors", {
    x <- letters[1:4]

    expect_equal(wrap(x, 1), c("b", "c", "d", "a"))
    expect_equal(wrap(x, 2), c("c", "d", "a", "b"))
})

test_that("wrap handles empty vector", {
    expect_equal(wrap(integer(0), 1), integer(0))
    expect_equal(wrap(character(0), 2), character(0))
})

test_that("wrap handles single element vector", {
    expect_equal(wrap(5, 0), 5)
    expect_equal(wrap(5, 1), 5)
    expect_equal(wrap(5, 10), 5)
})

test_that("wrap preserves vector type", {
    expect_type(wrap(1:5, 2), "integer")
    expect_type(wrap(c(1.5, 2.5), 1), "double")
    expect_type(wrap(letters[1:3], 1), "character")
    expect_type(wrap(c(TRUE, FALSE), 1), "logical")
})

test_that("wrap preserves attributes", {
    x <- c(a = 1, b = 2, c = 3)
    result <- wrap(x, 1)
    expect_named(result, c("b", "c", "a"))
})
