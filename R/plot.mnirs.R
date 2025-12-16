#' Plot *{mnirs}* objects
#'
#' Create a simple plot for objects returned from [create_mnirs_data()].
#'
#' @param x Object of class *"mnirs"* returned from [create_mnirs_data()]
#' @param ... Additional arguments:
#'  \describe{
#'      \item{`na.omit`}{A logical to omit missing (`NA`) values for better
#'      display of connected lines. `na.omit = FALSE` (the *default*) can be
#'      used to identify missing values.}
#'      \item{`label_time`}{A logical to display x-axis time values
#'      formatted as *"hh:mm:ss"* using [label_time()].
#'      `label_time = FALSE` (the *default*) will display simple numeric
#'      values on the x-axis.}
#'      \item{`n`}{A numeric value to define the number of breaks in both x-
#'      and y-axes.}
#'  }
#'
#' @returns A [ggplot2][ggplot2::ggplot()] object.
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' ## call an example *{mnirs}* data file
#' file_path <- example_mnirs("moxy_ramp")
#'
#' data_table <- read_mnirs(
#'     file_path,
#'     nirs_channels = c(smo2_right = "SmO2 Live",
#'                       smo2_left = "SmO2 Live(2)"),
#'     time_channel = c(time = "hh:mm:ss"),
#'     verbose = FALSE
#' )
#'
#' ## note the hidden plot option to display time values as `hh:mm:ss`
#' plot(data_table, label_time = TRUE)
#'
#' @export
plot.mnirs <- function(x, ...) {
    rlang::check_installed(
        c("ggplot2", "tidyr", "scales"),
        reason = "to plot mNIRS data"
    )

    args <- list(...)
    na.omit <- args$na.omit %||% FALSE
    label_time <- args$label_time %||% FALSE
    n <- args$n %||% 5

    nirs_channels <- attr(x, "nirs_channels")
    time_channel <- attr(x, "time_channel")

    ## pre-compute conditionals
    x_name <- if (label_time) {
        paste(time_channel, "(mm:ss)")
    } else {
        ggplot2::waiver()
    }
    x_breaks <- if (label_time) {
        breaks_timespan(n = n)
    } else if (rlang::is_installed("scales")) {
        scales::breaks_pretty(n = n)
    } else {
        ggplot2::waiver()
    }
    x_labels <- if (label_time) {
        format_hmmss
    } else {
        ggplot2::waiver()
    }
    y_breaks <- if (rlang::is_installed("scales")) {
        scales::breaks_pretty(n = n)
    } else {
        ggplot2::waiver()
    }

    ## TODO can I remove tidyr dependency?
    ## pivot all `nirs_channels` to `y` and plot by group
    plot_data <- tidyr::pivot_longer(
        x,
        cols = tidyr::all_of(nirs_channels),
        names_to = "nirs_channels",
        values_to = "y"
    )

    if (na.omit) {
        plot_data <- plot_data[stats::complete.cases(plot_data["y"]), ]
    }

    ## plot
    plot <- ggplot2::ggplot(plot_data) +
        ggplot2::aes(
            x = .data[[time_channel]],
            y = .data$y,
            colour = nirs_channels
        ) +
        theme_mnirs() +
        ggplot2::labs(
            x = x_name,
            y = "signal"
        ) +
        ggplot2::scale_x_continuous(
            breaks = x_breaks,
            labels = x_labels,
            expand = ggplot2::expansion(mult = 0.01)
        ) +
        ggplot2::scale_y_continuous(
            breaks = y_breaks,
            expand = ggplot2::expansion(mult = 0.01)
        ) +
        scale_colour_mnirs(name = NULL) +
        ggplot2::guides(
            colour = ggplot2::guide_legend(override.aes = list(linewidth = 1))
        ) +
        ggplot2::geom_line()

    return(plot)
}


#' Custom *{mnirs}* ggplot2 theme
#'
#' A `[ggplot2][ggplot2::ggplot2-package]` theme for display.
#'
#' @param base_size Base font size, given in pts.
#' @param base_family Base font family.
#' @param border Define either a *partial* or *full* border around plots.
#' @param ink Colour for text and lines. *Default* is *"black"*.
#' @param paper Background colour. *Default* is *"white"*.
#' @param accent Accent colour for highlights. *Default* is *"#0080ff"*.
#' @param ... Additional arguments to add to `[theme()][ggplot2::theme()]`.
#'
#' @details
#' - `axis.title = element_text(face = "bold")` by *default* Modify to *"plain"*.
#'
#' - `panel.grid.major` & `panel.grid.major` set to blank. Modify to
#'   `= element_line()` for visible grid lines.
#'
#' - `legend.position = "top"` by *default* Modify `"none"` to remove legend
#'   entirely.
#'
#' - `border = "partial"` uses `panel.border = element_blank()` and
#'   `axis.line = element_line()`.
#'
#' - `border = "full"` uses `panel.border = element_rect(colour = "black",`
#'   `linewidth = 1)` and `axis.line = element_line()`.
#'
#' - `base_family = "sans"` by *default*. `"Merriweather Sans"` is a nice
#'   alternative font which can be installed from
#'   <https://fonts.google.com/specimen/Merriweather+Sans>.
#'
#' @returns A [ggplot2][ggplot2::ggplot()] object.
#'
#' @seealso [palette_mnirs()] [scale_colour_mnirs()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(ggplot2)
#'
#' ## set theme for the current script
#' theme_set(theme_mnirs())
#'
#' ## plot example data
#' read_mnirs(
#'     file_path = example_mnirs("moxy_ramp"),
#'     nirs_channels = c(smo2_left = "SmO2 Live", smo2_right = "SmO2 Live(2)"),
#'     time_channel = c(time = "hh:mm:ss"),
#'     verbose = FALSE
#' ) |>
#'     plot(label_time = TRUE)
#'
#' @export
theme_mnirs <- function(
    base_size = 14,
    base_family = "sans",
    border = c("partial", "full"),
    ink = "black",
    paper = "white",
    accent = "#0080ff",
    ...
) {
    rlang::check_installed("ggplot2", reason = "to plot mNIRS data")
    border <- match.arg(border)
    half_line <- base_size * 0.5

    if (border == "partial") {
        panel.border <- ggplot2::element_blank()
        axis.line <- ggplot2::element_line()
    } else {
        panel.border <- ggplot2::element_rect(colour = "black", linewidth = 1)
        axis.line <- ggplot2::element_blank()
    }

    ggplot2::theme_bw(
        base_size = base_size,
        base_family = base_family,
        ink = ink,
        paper = paper,
        accent = accent
    ) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(
                size = ggplot2::rel(1.2),
                lineheight = 1.1
            ),
            plot.subtitle = ggplot2::element_text(lineheight = 1.1),
            plot.caption = ggplot2::element_text(colour = "grey50"),
            panel.border = panel.border,
            axis.line = axis.line,
            axis.title = ggplot2::element_text(face = "bold"),
            strip.background = ggplot2::element_rect(fill = "grey95"),
            strip.text = ggplot2::element_text(
                margin = ggplot2::margin_auto(t = half_line * 0.5)
            ),
            plot.margin = ggplot2::margin_part(r = base_size, unit = "pt"),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position = "top",
            legend.justification = "right",
            legend.direction = "horizontal",
            legend.margin = ggplot2::margin_auto(t = 1),
            legend.box.spacing = ggplot2::unit(half_line * 0.5, "pt")
        ) +
        ggplot2::theme(...)
}


#' Custom *{mnirs}* colour palette
#'
#' @param n A numeric vector specifying the number of colours to return.
#' @param names A character vector specifying colour names to return.
#'
#' @returns Named or unnamed character vector of hex colours.
#'
#' @seealso [theme_mnirs()] [scale_colour_mnirs()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' scales::show_col(palette_mnirs())
#' scales::show_col(palette_mnirs(n = 2))
#' scales::show_col(palette_mnirs(names = c("red", "orange")))
#'
#' @export
palette_mnirs <- function(n = NULL, names = NULL) {
    # fmt: skip
    colours <- c(
        `light blue`  = "#0080ff",      ## "VL"
        `dark red`    = "#ba2630",      ## "FCR"
        `light green` = "#5b8c52",      ## "BB" "#7dbf70"
        `pink`        = "#ff80ff",      ## "VM"
        `orange`      = "#ff7f00",      ## "SCM"
        `dark blue`   = "#00468Bff",    ## "TA"
        `light red`   = "#db5555",      ## "ECR"
        `green`       = "#42B540FF",    ## "DL"
        `purple`      = "#9f79ee",      ## "RF"
        `brown`       = "#8b4726",      ## "PS"
        `blue`        = "#0000ff",      ## "HHb"
        `red`         = "#ED0000FF"     ## "O2Hb"
    )

    if (!is.null(names) && !is.null(n)) {
        cli_abort(c("x" = "Cannot specify both {.arg n} and {.arg names}"))
    }

    if (!is.null(names)) {
        names <- match.arg(names, choices = names(colours), several.ok = TRUE)
        return(colours[names])
    }

    if (is.null(n)) {
        return(unname(colours))
    }

    validate_numeric(n, 1, c(1, Inf), msg1 = "one-element positive")
    if (n <= length(colours)) {
        return(unname(colours[seq_len(n)]))
    }
    ## interpolate if more colours needed, but this probably won't look good!
    return(grDevices::colorRampPalette(colours)(n))
}


#' Scales for custom *{mnirs}* palette
#'
#' @param ... Arguments passed to `ggplot2::discrete_scale()`.
#' @param aesthetics A character vector with aesthetic(s) passed to
#'   `ggplot2::discrete_scale()`. *Default* is `"colour"`.
#'
#' @returns A [ggplot2][ggplot2::ggplot()] scale object.
#'
#' @seealso [theme_mnirs()] [palette_mnirs()]
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(ggplot2)
#'
#' ## plot example data
#' df <- read_mnirs(
#'     file_path = example_mnirs("moxy_ramp"),
#'     nirs_channels = c(smo2_left = "SmO2 Live", smo2_right = "SmO2 Live(2)"),
#'     time_channel = c(time = "hh:mm:ss"),
#'     verbose = FALSE
#' )
#'
#' ggplot(df, aes(x = time)) +
#'     theme_mnirs() +
#'     scale_colour_mnirs(name = NULL) +
#'     geom_line(aes(y = smo2_left, colour = "smo2_left")) +
#'     geom_line(aes(y = smo2_right, colour = "smo2_right"))
#'
#' @rdname scale_colour_mnirs
#' @export
scale_colour_mnirs <- function(..., aesthetics = "colour") {
    rlang::check_installed("ggplot2", reason = "to plot mNIRS data")

    ggplot2::discrete_scale(
        aesthetics = aesthetics,
        palette = palette_mnirs,
        na.value = "grey10",
        ...
    )
}

#' @rdname scale_colour_mnirs
#' @export
scale_color_mnirs <- scale_colour_mnirs

#' @rdname scale_colour_mnirs
#' @export
scale_fill_mnirs <- function(..., aesthetics = "fill") {
    rlang::check_installed("ggplot2", reason = "to plot mNIRS data")

    ggplot2::discrete_scale(
        aesthetics = aesthetics,
        palette = palette_mnirs,
        na.value = "grey10",
        ...
    )
}


#' Breaks for timespan data
#'
#' Pretty timespan breaks for plotting in units of 5, 15, 30, 60 sec, etc.
#' Modified from [scales::breaks_timespan()].
#'
#' @param unit The time unit used to interpret numeric data input (*defaults*
#'   to *"secs"*).
#' @param n Desired number of breaks. You may get slightly more or fewer breaks
#'   than requested.
#'
#' @returns Returns a function for generating breaks.
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(ggplot2)
#' x = 0:120
#' y = sin(2 * pi * x / 15) + rnorm(length(x), 0, 0.2)
#'
#' ggplot(data.frame(x, y)) +
#'     aes(x = x, y = y) +
#'     theme_mnirs() +
#'     scale_x_continuous(breaks = breaks_timespan()) +
#'     geom_line()
#'
#' @keywords internal
#' @export
breaks_timespan <- function(
    unit = c("secs", "mins", "hours", "days", "weeks"),
    n = 5
) {
    unit <- match.arg(unit)
    force(n)
    function(x) {
        x <- as.numeric(as.difftime(x, units = unit), units = "secs")
        range <- range(x, na.rm = TRUE)
        diff <- range[2L] - range[1L]

        ## scale of time range
        ## define nice steps for each unit
        if (diff <= 5 * 60) {
            scale <- 1 ## sec
            nice_steps <- c(1, 2, 5, 10, 15, 20, 30, 60, 120)
        } else if (diff <= 5 * 3600) {
            scale <- 60 ## min
            nice_steps <- c(1, 2, 5, 10, 15, 20, 30, 60, 120) * 60
        } else if (diff <= 5 * 86400) {
            scale <- 3600 ## hr
            nice_steps <- c(0.25, 0.5, 1, 2, 3, 4, 6, 8, 12, 24) * 3600
        } else {
            scale <- 86400 ## days
            nice_steps <- c(1, 7, 28) * 86400
        }

        ## scale to scale units
        range_scaled <- range / scale
        scaled_steps <- nice_steps / scale

        ## find optimal step size from nice_steps
        target_step <- diff(range_scaled) / n
        best_step <- scaled_steps[which.min(abs(scaled_steps - target_step))]

        ## generate breaks
        breaks_scaled <- seq(
            floor(range_scaled[1L] / best_step) * best_step,
            ceiling(range_scaled[2L] / best_step) * best_step,
            by = best_step
        )

        ## convert back to seconds
        round(as.numeric(as.difftime(breaks_scaled * scale, units = "secs")))
    }
}


#' Format timespan data as h:mm:ss
#'
#' Convert numeric timespan data to `h:mm:ss` format for pretty plotting.
#' Inspired by [ggplot2::scale_x_time()].
#'
#' @param x A numeric vector.
#'
#' @details
#' If all values are less than 3600 (1 hour), then format is returned as
#'   `mm:ss`. If any value is greater than 3600, format is returned as
#'   `h:mm:ss` with leading zeroes.
#'
#' @returns A character vector the same length as `x`.
#'
#' @examplesIf (identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' library(ggplot2)
#'
#' x = 0:120
#' y = sin(2 * pi * x / 15) + rnorm(length(x), 0, 0.2)
#'
#' ggplot(data.frame(x, y)) +
#'     aes(x = x, y = y) +
#'     theme_mnirs() +
#'     scale_x_continuous(
#'         breaks = breaks_timespan(),
#'         labels = format_hmmss
#'     ) +
#'     geom_line()
#'
#' @keywords internal
#' @export
format_hmmss <- function(x) {
    # validate_numeric(x)
    x <- as.numeric(x)
    ## logical whether to handle NAs
    handle_na <- any(is.na(x))

    if (handle_na) {
        na_info <- preserve_na(x)
        x <- na_info$x_valid
    }

    sign <- ifelse(x < 0, "-", "")
    hrs <- abs(x) %/% 3600
    mins <- (abs(x) %% 3600) %/% 60
    secs <- abs(x) %% 60

    hmmss_string <- if (any(hrs > 0, na.rm = TRUE)) {
        sprintf("%s%d:%02d:%02d", sign, hrs, mins, secs)
    } else {
        sprintf("%s%02d:%02d", sign, mins, secs)
    }

    ## return y to original x length with NAs if handled
    if (handle_na) {
        return(restore_na(hmmss_string, na_info))
    } else {
        return(hmmss_string)
    }
}
