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
#' test_data$foo.lower <- test_data$foo - 0.5
#' test_data$foo.upper <- test_data$foo + 0.5
#' test_data$bar.lower <- test_data$bar - 0.5
#' test_data$bar.upper <- test_data$bar + 0.5
#'
#' # Minimales Beispiel
#' create_plot(test_data, foo, bar)
#'
#' # Gruppierung nach Form und Farben
#' create_plot(test_data, foo, bar, shape = group, upper_fill = "blue", lower_fill = "orange")
#'
#' # Achsenbeschriftung und Legendenposition
#' c reate_plot(test_data, foo, bar, shape = group, upper_fill = "blue", lower_fill = "orange", xlab = "Foo", ylab = "Bar", legend_position = c(0.18, 0.02))
#'
#' # Vertikale Fehlerbalken
#' create_plot(test_data, foo, bar, ymin = bar.lower, ymax = bar.upper, shape = group, upper_fill = "blue", lower_fill = "orange", xlab = "Foo", ylab = "Bar", legend_position = c(0.18, 0.02))
#'
#' # horizontale und vertikale Fehlerbalken
#' create_plot(test_data, foo, bar, xmin = foo.lower, xmax = foo.upper, ymin = bar.lower, ymax = bar.upper, shape = group, upper_fill = "blue", lower_fill = "orange", xlab = "Foo", ylab = "Bar", legend_position = c(0.18, 0.02))



create_plot <- function(data,
                        x_var,
                        y_var,
                        xmin = NULL,
                        xmax = NULL,
                        ymin = NULL,
                        ymax = NULL,
                        shape = NULL,
                        upper_fill = "firebrick",
                        lower_fill = "black",
                        xlab = NULL,
                        ylab = NULL,
                        legend_position = c(0.95, 0.05),
                        plot_range = NULL,
                        show_legend = TRUE
                        ) {

if(is.null(plot_range)) {
  range_x <- range(dplyr::pull(data, {{ x_var}}), na.rm = TRUE)
  range_y <- range(dplyr::pull(data, {{ y_var}}), na.rm = TRUE)
  plot_range <- range(c(range_x, range_y), na.rm = TRUE)
}

if(is.null(substitute(shape))) {
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
  scatterplot <- ggplot2::ggplot(data) +
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
    ggplot2::guides(
      fill = "none"
    ) +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = legend_position,
      legend.justification = c(1, 0),
      legend.background = ggplot2::element_rect(
        fill = "white",
        colour = "black"
      ),
      plot.margin = ggplot2::margin(0, 0, 5.5, 5.5)
    )

if(!is.null(substitute(xmin)) && !is.null(substitute(xmax))) {
  scatterplot <- scatterplot +
    ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = {{ xmin }},
      xmax = {{ xmax }}
  ),
  width = 0.1
  )
}

if(!is.null(substitute(ymin)) && !is.null(substitute(ymax))) {
  scatterplot <- scatterplot +
   ggplot2::geom_errorbar(
   ggplot2::aes(
     ymin = {{ ymin }},
     ymax = {{ ymax }}
 ),
 width = 0.1
 )
}

if(!is.null(xlab)) {
  scatterplot <- scatterplot +
    ggplot2::xlab(xlab)
}

if(!is.null(ylab)) {
  scatterplot <- scatterplot +
    ggplot2::ylab(ylab)
}

if (!show_legend) {
  scatterplot <- scatterplot +
    ggplot2::theme(legend.position = "none")
}

x_mean <- mean(dplyr::pull(data, {{ x_var }}), na.rm = TRUE)
x_sd <- sd(dplyr::pull(data,{{ x_var }}), na.rm = TRUE)

  x_den <- ggplot2::ggplot(data) +
    ggplot2::aes(x = {{ x_var }}) +
    ggplot2::stat_density(color = "black", fill = NA) +
    ggplot2::coord_cartesian(xlim = plot_range, clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::annotate("segment", x = x_mean - x_sd,xend = x_mean + x_sd, y = 0, yend = 0, linewidth = 1.5) +
    ggplot2::annotate("point", x = x_mean, y = 0, size = 3, color = "black") +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 0, 2))

y_mean <- mean(dplyr::pull(data, {{ y_var }}), na.rm = TRUE)
y_sd <- sd(dplyr::pull(data, {{ y_var }}), na.rm = TRUE)

  y_den <- ggplot2::ggplot(data) +
    ggplot2::aes(y = {{ y_var }}) +
    ggplot2::stat_density(color = "black", fill = NA) +
    ggplot2::coord_cartesian(ylim = plot_range, clip = "off") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::annotate("segment", y = y_mean - y_sd, yend = y_mean + y_sd, x = 0, xend = 0, linewidth = 1.5) +
    ggplot2::annotate("point", y = y_mean, x = 0, size = 3, color = "black") +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 0))

difference_scores <- (dplyr::pull(data, {{ y_var }}) - dplyr::pull(data, {{ x_var }})) / sqrt(2)
density_data <- stats::density(difference_scores, na.rm = TRUE)
density_df <- data.frame(difference = density_data$x, density = density_data$y)

half_axis <- max(abs(density_df$difference)) / sqrt(2)

density_df$x <- (-density_df$difference + half_axis * sqrt(2) * density_df$density) / sqrt(2)
density_df$y <- (density_df$difference + half_axis * sqrt(2) * density_df$density) / sqrt(2)

axis_df <- tibble::tibble(
  x = c( half_axis, -half_axis),
  y = c(-half_axis,  half_axis)
)

pos <- density_df[density_df$difference > 0, ]

pos_poly_df <- data.frame(
  x = c(0, pos$x, rev(-pos$difference / sqrt(2))),
  y = c(0, pos$y, rev( pos$difference / sqrt(2)))
)

neg <- density_df[density_df$difference < 0, ]

neg_poly_df <- data.frame(
  x = c(0, neg$x, rev(-neg$difference / sqrt(2))),
  y = c(0, neg$y, rev( neg$difference / sqrt(2)))
)

lim <- max(abs(c(density_df$x, density_df$y, axis_df$x, axis_df$y)))

diff_mean <- mean(difference_scores, na.rm = TRUE)
diff_sd <- sd(difference_scores, na.rm = TRUE)

 diag_den <- ggplot2::ggplot(
   density_df,
   ggplot2::aes(x = x, y = y)) +
   ggplot2::geom_polygon(
     fill = upper_fill,
     alpha = 0.4,
     data = pos_poly_df
   ) +
   ggplot2::geom_polygon(
     fill = lower_fill,
     alpha = 0.4,
     data = neg_poly_df
   ) +
   ggplot2::annotate(
     "segment",
     x = 0,
     y = 0,
     xend = half_axis / sqrt(2),
     yend = half_axis / sqrt(2),
     linetype = "22",
     color = grey(0.7)
   ) +
   ggplot2::geom_path() +
   ggplot2::geom_path(
     ggplot2::aes(x = x, y = y),
     data = axis_df,
     linewidth = 0.25) +
    ggplot2::annotate(
      "segment",
      x = -(diff_mean - diff_sd)/sqrt(2),
      xend = -(diff_mean + diff_sd)/sqrt(2),
      y = (diff_mean - diff_sd)/sqrt(2),
      yend = (diff_mean + diff_sd)/sqrt(2),
      linewidth = 1.5
    ) +
    ggplot2::annotate(
      "point",
      x = -diff_mean/sqrt(2),
      y = diff_mean/sqrt(2),
      size = 3,
      color = "black"
    ) +
   ggplot2::coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim),
                     expand = FALSE, clip = "off") +
   ggplot2::theme_void() +
   ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2))

  inset_half <- 0.25  # half-width of the inset box, as a fraction of the scatter panel; tune for size

  scatter_inset <- scatterplot +
    patchwork::inset_element(
      diag_den,
      left = 1 - inset_half, bottom = 1 - inset_half,
      right = 1 + inset_half, top = 1 + inset_half,
      align_to = "panel", clip = FALSE
    )

  final_plot <- ((scatter_inset +
                    x_den +
                    patchwork::plot_spacer() +
                    y_den +
                    patchwork::plot_layout(widths = c(1, inset_half), heights = c(inset_half, 1), design = "BC\nAD")
  ) & ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))) |>
    cowplot::ggdraw()

  plot_fixed(final_plot)
}
