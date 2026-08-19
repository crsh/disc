#' Create plot
#'
#' @param data a data frame
#' @param x_var variable mapped to the x-axis
#' @param y_var variable mapped to the y-axis
#' @param upper_fill color of points above the diagonal
#' @param lower_fill color of points below the diagonal
#'
#' @returns draws a fixed-size scatter plot with marginal density plots
#' @export
#'
#' @examples
#'
#' test_data <- data.frame(foo = rnorm(100), bar = rnorm(100), group = factor(rep(c("A", "B"), 50)))
#'
#' # Minimales Beispiel
#' create_plot(test_data, foo, bar)
#'
#' # Gruppierung nach Form und Farben
#' create_plot(test_data, foo, bar, shape = group, upper_fill = "blue", lower_fill = "orange")
#'
#' # Achsenbeschriftung und Legendenposition
#' create_plot(test_data, foo, bar, shape = group, upper_fill = "blue", lower_fill = "orange", xlab = "Foo", ylab = "Bar", legend_position = c(0.1, 0.16))
#'


create_plot <- function(data,
                        x_var,
                        y_var,
                        shape = NULL,
                        upper_fill = "firebrick",
                        lower_fill = "black",
                        xlab = NULL,
                        ylab = NULL,
                        legend_position = c(0.1, 0.16)
                        ) {

  range_x <- range(dplyr::pull(data, {{ x_var}}), na.rm = TRUE)
  range_y <- range(dplyr::pull(data, {{ y_var}}), na.rm = TRUE)
  plot_range <- range(c(range_x, range_y), na.rm = TRUE)

if (is.null(substitute(shape))) {
    point_layer <- ggplot2::geom_point(
      ggplot2::aes(
        fill = ifelse({{ y_var }} > {{ x_var }}, "upper", "lower")
      ),
      shape = 21,
      color = "white",
      size = 3
    )
    shape_scale <- NULL
   } else {
     point_layer <- ggplot2::geom_point(
      ggplot2::aes(
        fill = ifelse({{ y_var }} > {{ x_var }}, "upper", "lower"),
        shape = {{ shape }}
      ),
      color = "white",
      size = 3
    )
    shape_scale <- ggplot2::scale_shape_manual(
      values = c(21, 22, 23, 24, 25)
    )
  }
  scatterplot_uncorrelated <- ggplot2::ggplot(data) +
    ggplot2::aes(
      x = {{ x_var }},
      y = {{ y_var }}
    ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "22",
      color = grey(0.7)
    ) +
    point_layer +
    ggplot2::scale_fill_manual(
      values = c(
        upper = upper_fill,
        lower = lower_fill
      )
    ) +
    shape_scale +
    ggplot2::coord_equal(
      xlim = plot_range,
      ylim = plot_range
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab
    ) +
    ggplot2::guides(
      fill = "none"
    ) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.background = ggplot2::element_rect(
        fill = "white",
        colour = "black"
      )
    )


#  scatterplot_uncorrelated <- ggplot2::ggplot(data) +
#   ggplot2::aes (x = {{ x_var }}, y = {{ y_var }}) +
#   ggplot2::geom_abline(
#     intercept = 0
#   , slope = 1
#   , linetype = "22"
#   , color = grey(0.7)
#  ) +
#   ggplot2::geom_point(
#       ggplot2::aes(fill = ifelse({{ y_var}} > {{ x_var }}, "upper", "lower"))
#     , shape = {{ shape }}
#     , color = "white"
#     , size = 3
#   ) +
#   ggplot2::scale_fill_manual(values = c(upper = upper_fill, lower = lower_fill)) +
#   ggplot2::scale_shape_manuel(values = c(21, 22, 23, 24, 25)) +
#   ggplot2::coord_equal(xlim = plot_range, ylim = plot_range) +
#   ggplot2::labs(x = xlab, y = ylab) +
#   ggplot2::guides(fill = "none") +
#   ggplot2::theme(legend.position = legend_position,
#                 legend.background = ggplot2::element_rect(
#                    fill = "white",
#                    colour = "black"))

  x_den <- ggplot2::ggplot(data) +
    ggplot2::aes(x = {{ x_var }}) +
    ggplot2::stat_density(color = "black", fill = NA) +
    ggplot2::lims(x = plot_range) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2))

  y_den <- ggplot2::ggplot(data) +
    ggplot2::aes(y = {{ y_var }}) +
    ggplot2::stat_density(color = "black", fill = NA) +
    ggplot2::lims(y = plot_range) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2))

  diag_den <- ggplot2::ggplot(data) +
    ggplot2::geom_path(ggplot2::aes(x = x, y = y), data = tibble::tibble(x = c(0, dnorm(0, mean =
                                                                0.7, sd = 1) * 5), y = x), linetype = "22", color = grey(0.7)) +
    ggplot2::geom_path(ggplot2::aes(x = x, y = y), data = tibble::tibble(x = c(-2.85, 2.75), y =
                                                 c(2.85, -2.75)), size = 0.5) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2))

  final_plot <- (x_den +
                   patchwork::plot_spacer() +
                   scatterplot_uncorrelated +
                   y_den +
                   patchwork::plot_layout(width = c(1, 0.3), height = c(0.3, 1))
  ) |>
    cowplot::ggdraw() +
    patchwork::inset_element(
      diag_den
      , left = 0.61 -0.025
      , bottom = 0.61 -0.025
      , right = 1 -0.025
      , top = 1 -0.025
      , align_to = "plot"
    )
  plot_fixed(final_plot)

}

