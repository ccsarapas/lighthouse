
#' Nicer default theme and palettes for `ggplot2`
#'
#' Changes default theme and color scales for `ggplot2`.
#'
#' Theme is based on `hrbrthemes::theme_ipsum_rc()`, unless otherwise specified
#' in `base_theme` argument. The theme is further modified as follows:
#' * Axis titles are centered
#' * Legend title is omitted
#' * Minor gridlines are omitted
#' * Facet labels are placed outside axes
#' * Various tweaks to text size and margins
#'
#' Default color and fill palettes are set based on scale type:
#' * For discrete scales, `RColorBrewer` palette "Set1," unless otherwise
#' specified in `brewer_pal_discrete` argument
#' * For continuous and binned scales, `RColorBrewer` palette "Blues"
#' * For ordinal scales, `viridisLite` palette "viridis"
#'
#' Default font family for `geom_text()` and `geom_label()` is set to match
#' `base_theme`.
#'
#' @export
set_ggplot_opts <- function(base_theme = hrbrthemes::theme_ipsum_rc,
                            brewer_pal_discrete = "Set1",
                            ...) {
  scale_color_brewer_d <- function(...) {
    ggplot2::scale_color_brewer(palette = brewer_pal_discrete, ...)
  }
  scale_fill_brewer_d <- function(...) {
    ggplot2::scale_fill_brewer(palette = brewer_pal_discrete, ...)
  }
  options(
    ggplot2.continuous.colour = ggplot2::scale_color_distiller,
    ggplot2.continuous.fill = ggplot2::scale_fill_distiller,
    ggplot2.binned.colour = ggplot2::scale_color_fermenter,
    ggplot2.binned.fill = ggplot2::scale_fill_fermenter,
    ggplot2.discrete.colour = scale_color_brewer_d,
    ggplot2.discrete.fill = scale_fill_brewer_d,
    ggplot2.ordinal.colour = ggplot2::scale_color_viridis_d,
    ggplot2.ordinal.fill = ggplot2::scale_fill_viridis_d
  )
  theme_family <- base_theme()$text$family
  ggplot2::update_geom_defaults("text", list(family = theme_family))
  ggplot2::update_geom_defaults("label", list(family = theme_family))
  ggplot2::theme_set(
    base_theme() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(
          size = 12,
          hjust = .5,
          margin = ggplot2::margin(t = 5)
        ),
        axis.title.y = ggplot2::element_text(
          size = 12,
          hjust = .5,
          margin = ggplot2::margin(r = 5)
        ),
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 11),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(15, 15, 15, 15),
        strip.placement = "outside",
        ...
      )
  )
}


#' Translate colors before and after alpha blending
#'
#' These functions translate colors between their original RGB values and their
#' RGB values after alpha blending with a background color. `before_opacity`
#' calculates the original color given the blended color, while `after_opacity`
#' calculates the blended color given the original color.
#'
#' @param color The starting color as a color name, hex code, or RGB triplet.
#' @param alpha The opacity of the foreground color, a number between 0 and 1.
#' @param bg The background color for blending, as a color name, hex code, or RGB triplet. Defaults to "white".
#'
#' @return
#' - `before_opacity`: The original color before alpha blending, as a hex code.
#' - `after_opacity`: The blended color after alpha blending, as a hex code.
#'
#' @examples
#' red <- "red"
#' red_50 <- after_opacity(red, 0.5)
#' red_back <- before_opacity(red_50, 0.5)
#' scales::show_col(c(red, red_50, red_back), ncol = 3)
#'
#' color_blends <- sapply(
#'   c("red", "blue", "yellow", "white", "black", "gray50"),
#'   after_opacity,
#'   color = "red",
#'   alpha = 0.5
#' )
#' scales::show_col(color_blends)
#'
#' @name opacity
#' @export
after_opacity <- function(color, alpha, bg = "white") {
  before <- as.numeric(col2rgb(color))
  bg <- as.numeric(col2rgb(bg))
  after <- pmin(pmax((((1 - alpha) * bg) + before * alpha), 0), 255)
  do.call(rgb, c(as.list(after), list(maxColorValue = 255)))
}
#' @rdname opacity
#' @export
before_opacity <- function(color, alpha, bg = "white") {
  after <- as.numeric(col2rgb(color))
  bg <- as.numeric(col2rgb(bg))
  before <- pmin(pmax((after - ((1 - alpha) * bg)) / alpha, 0), 255)
  do.call(rgb, c(as.list(before), list(maxColorValue = 255)))
}

#' Add crossings to a dataframe for area charts
#'
#' Augments a dataframe with x-values where y = f(x) = 0. This is useful for creating area charts with different fills for values less than versus greater than 0.
#'
#' @param data A data frame containing the original x and y values.
#' @param x x-axis values.
#' @param y y-axis values.
#' @param .by Grouping variable(s). Useful for computing crossings for faceted plots.
#'
#' @details
#' The returned dataframe will include columns `pos_neg` and `cross_group`. Within `geom_area()`, `cross_group` should be mapped to `group`, while `pos_neg` should be mapped to aesthetics such as `fill` or `color`.
#'
#' @return The input data frame with additional rows representing the crossings (where y = 0), and two new columns:
#' - `pos_neg`: Indicates whether the y-value is positive ("pos") or negative ("neg").
#' - `cross_grp`: A grouping variable for the segments between crossings.
#'
#' @examples
#' nile_flow <- tibble::tibble(
#'   Year = time(Nile),
#'   Flow = as.numeric(Nile),
#'   FlowDelta = (Flow - Flow[[1]]) / Flow[[1]]
#' )
#' nile_flow_x0 <- add_crossings(nile_flow, Year, FlowDelta)
#' ggplot2::ggplot(nile_flow_x0, ggplot2::aes(Year, FlowDelta)) +
#'   ggplot2::geom_area(
#'     ggplot2::aes(group = cross_grp, color = pos_neg, fill = pos_neg),
#'     alpha = 0.25,
#'     show.legend = FALSE
#'   ) +
#'   ggplot2::geom_hline(yintercept = 0, linewidth = 0.25) +
#'   ggplot2::scale_color_manual(
#'     values = c("darkred", "blue"),
#'     aesthetics = c("color", "fill")
#'   ) +
#'   ggplot2::scale_y_continuous(
#'     "Nile River Annual Flow:\n% Change from 1871",
#'     labels = scales::percent
#'   ) +
#'   ggplot2::theme_minimal()
#'
#' @export
add_crossings <- function(data, x, y, .by = NULL) {
  crossings <- data %>%
    dplyr::mutate(
      xzero = -(({{y}} * (dplyr::lead({{x}}) - {{x}})) / (dplyr::lead({{y}}) - {{y}})) + {{x}},
      {{x}} := dplyr::if_else(xzero >= {{x}} & xzero < dplyr::lead({{x}}), xzero, NA),
      {{y}} := dplyr::if_else(is.na({{x}}), NA, 0),
      xzero = NULL,
      .by = {{.by}}
    ) %>%
    tidyr::drop_na({{x}})
  data %>%
    dplyr::filter({{y}} != 0) %>%
    dplyr::mutate(
      pos_neg = dplyr::if_else({{y}} >= 0, "pos", "neg")
    ) %>%
    dplyr::bind_rows(
      crossings, crossings
    ) %>%
    dplyr::arrange({{x}}) %>%
    dplyr::mutate(
      pos_neg = dplyr::coalesce(pos_neg, dplyr::lag(pos_neg), dplyr::lead(pos_neg)),
      cross_grp = dplyr::consecutive_id(pos_neg),
      .by = {{.by}}
    )
}

