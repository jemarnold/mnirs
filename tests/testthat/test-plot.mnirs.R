skip_if_not_installed("ggplot2")

## theme_mnirs() ============================================================
test_that("theme_mnirs returns a ggplot2 theme object", {
    theme_obj <- theme_mnirs()
    expect_s3_class(theme_obj, "theme")
    expect_s3_class(theme_obj, "gg")
})

test_that("theme_mnirs border argument works correctly", {
    partial <- theme_mnirs(border = "partial")
    full <- theme_mnirs(border = "full")

    expect_s3_class(partial$panel.border, "element_blank")
    expect_s3_class(full$panel.border, "element_rect")
})

test_that("theme_mnirs accepts custom colours", {
    custom <- theme_mnirs(ink = "red", paper = "blue", accent = "#ff0000")
    expect_s3_class(custom, "theme")
})

## palette_mnirs() ========================================
test_that("palettes returns correct colour vector", {
    all_colours <- palette_mnirs()
    expect_type(all_colours, "character")
    expect_length(all_colours, 12)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}", all_colours)))
})

test_that("palettes subset by number works", {
    expect_length(palette_mnirs(3), 3)
    expect_length(palette_mnirs(1), 1)
    expect_error(palette_mnirs(2:4))
})

test_that("palettes subset by name works", {
    red <- palette_mnirs("red")
    expect_named(red, "red")
    expect_equal(red[["red"]], "#ED0000FF")

    multi <- palette_mnirs(c("red", "blue"))
    expect_length(multi, 2)
    expect_named(multi, c("red", "blue"))
})

test_that("palette_mnirs interpolates when n > 12", {
    many <- palette_mnirs(20)
    expect_length(many, 20)
})

## scale_colour_mnirs() ==================================================
test_that("scale_color_mnirs is an alias for scale_colour_mnirs", {
    expect_identical(scale_color_mnirs, scale_colour_mnirs)
})

test_that("scale_*_mnirs returns a ggplot2 Scale object", {
    expect_s3_class(scale_colour_mnirs(), "Scale")
    expect_s3_class(scale_colour_mnirs(), "ScaleDiscrete")

    expect_s3_class(scale_fill_mnirs(), "Scale")
    expect_s3_class(scale_fill_mnirs(), "ScaleDiscrete")
})

test_that("scale_colour_mnirs uses correct aesthetics", {
    expect_equal(scale_colour_mnirs()$aesthetics, "colour")
    expect_equal(scale_fill_mnirs()$aesthetics, "fill")
})

test_that("scale functions pass through additional arguments", {
    scale <- scale_colour_mnirs(name = "Test")
    expect_equal(scale$name, "Test")
})

test_that("scale functions use palette_mnirs", {
    # Extract the palette function and call it
    expect_equal(scale_colour_mnirs()$palette(5), palette_mnirs(5))
    expect_equal(scale_fill_mnirs()$palette(5), palette_mnirs(5))

    # Test with NULL argument
    expect_equal(scale_colour_mnirs()$palette(NULL), palette_mnirs(NULL))

    # Test with character argument
    expect_equal(
        scale_colour_mnirs()$palette(c("light blue", "dark red")),
        palette_mnirs(c("light blue", "dark red"))
    )

    ## test with na.value
    expect_equal(scale_colour_mnirs()$na.value, "grey10")
    expect_equal(scale_fill_mnirs()$na.value, "grey10")
})

test_that("scale functions work in ggplot2 plots", {
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt, colour = factor(cyl))) +
        ggplot2::geom_point() +
        scale_colour_mnirs()

    expect_s3_class(p, "gg")
    expect_s3_class(p$scales$get_scales("colour"), "ScaleDiscrete")
})

## breaks_timespan() ==================================================
test_that("breaks_timespan returns a function", {
    breaks_fn <- breaks_timespan()
    expect_type(breaks_fn, "closure")
})

test_that(" breaks_timespan corresponds to nice_steps for each scale level", {

    # Test scale = 1 (diff <= 5 * 60)
    nice_steps_sec <- c(1, 2, 5, 10, 15, 20, 30, 60, 120)
    x_sec <- c(0, 150)  # 2.5 min range
    breaks_sec <- breaks_timespan("secs", n = 5)(x_sec)
    steps_sec <- unique(diff(breaks_sec))
    expect_true(all(steps_sec %in% nice_steps_sec))
    expect_type(breaks_sec, "double")
    expect_true(all(breaks_sec >= 0 & breaks_sec <= 150))
    expect_equal(length(breaks_sec), 5, tolerance = 2)

    # Test scale = 60 (5 * 60 < diff <= 5 * 3600)
    nice_steps_min <- c(1, 2, 5, 10, 15, 20, 30, 60, 120) * 60
    x_min <- c(0, 7200)  # 2 hour range
    breaks_min <- breaks_timespan("secs", n = 5)(x_min)
    steps_min <- unique(diff(breaks_min))
    expect_true(all(steps_min %in% nice_steps_min))
    expect_type(breaks_min, "double")
    expect_true(all(breaks_min >= 0 & breaks_min <= 7200))
    expect_equal(length(breaks_min), 5, tolerance = 2)

    # Test scale = 3600 (5 * 3600 < diff <= 5 * 86400)
    nice_steps_hr <- c(0.25, 0.5, 1, 2, 3, 4, 6, 8, 12, 24) * 3600
    x_hr <- c(0, 86400)  # 1 day range
    breaks_hr <- breaks_timespan("secs", n = 5)(x_hr)
    steps_hr <- unique(diff(breaks_hr))
    expect_true(all(steps_hr %in% nice_steps_hr))
    expect_type(breaks_hr, "double")
    expect_true(all(breaks_hr >= 0 & breaks_hr <= 86400))
    expect_equal(length(breaks_hr), 5, tolerance = 2)

    # Test scale = 86400 (diff > 5 * 86400)
    nice_steps_day <- c(1, 7, 28) * 86400
    x_day <- c(0, 86400 * 28)  # 28 day range
    breaks_day <- breaks_timespan("secs", n = 5)(x_day)
    steps_day <- unique(diff(breaks_day))
    expect_true(all(steps_day %in% nice_steps_day))
    expect_type(breaks_day, "double")
    expect_true(all(breaks_day >= 0 & breaks_day <= 86400 * 28))
    expect_equal(length(breaks_day), 5, tolerance = 2)
})

## format_hmmss() ==================================================
test_that("format_hmmss formats time correctly", {
    ## seconds
    expect_equal(format_hmmss(0), "00:00")
    expect_equal(format_hmmss(30), "00:30")
    expect_equal(format_hmmss(90), "01:30")
    expect_equal(format_hmmss(3599), "59:59")

    ## hours
    expect_equal(format_hmmss(3600), "1:00:00")
    expect_equal(format_hmmss(3661), "1:01:01")
    expect_equal(format_hmmss(7325), "2:02:05")

    ## negative
    expect_equal(format_hmmss(-30), "-00:30")
    expect_equal(format_hmmss(-3661), "-1:01:01")
})

test_that("format_hmmss handles vectors", {
    result <- format_hmmss(c(0, 30, 90, 3600))
    expect_length(result, 4)
    expect_equal(result, c("0:00:00", "0:00:30", "0:01:30", "1:00:00"))
})

test_that("format_hmmss handles NA values", {
    result <- format_hmmss(c(30, NA, 90))
    expect_length(result, 3)
    expect_true(is.na(result[2]))
})


## plot.mnirs works on real data ===============================
test_that("plot.mnirs moxy.perfpro works", {
    file_path <- system.file("extdata/moxy_ramp.xlsx",
                             package = "mnirs")

    df <- read_mnirs(
        file_path = file_path,
        nirs_channels = c(smo2_left = "SmO2 Live",
                         smo2_right = "SmO2 Live(2)"),
        time_channel = c(time = "hh:mm:ss"),
        inform = FALSE)

    ## visual check
    plot <- plot(df, na.omit = TRUE, label_time = TRUE, n = 8)
    expect_s3_class(plot, "ggplot")
})
