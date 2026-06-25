create_plot <- function(data,
                        x_var,
                        y_var,
                        weird_color = "firebrick",
                        expected_color = "black") {
  scatterplot_uncorrelated <- ggplot2::ggplot(data) +
    ggplot2::aes (x = {{ x_var }}, y = {{ y_var }}) +
    ggplot2::geom_abline(
      intercept = 0
    , slope = 1
    , linetype = "22"
    , color = grey(0.7)
  ) +
    ggplot2::geom_point(
        ggplot2::aes(fill = ifelse({{ y_var}} > {{ x_var }}, "weird", "expected"))
      , pch = 21
      , color = "white"
      , size = 3
    ) +
    ggplot2::scale_fill_manual(values = c(weird = weird_color, expected = expected_color)) +
    ggplot2::coord_equal(xlim = c(2, 35), ylim = c(2, 35)) +
    ggplot2::guides(fill = "none")

  x_den <- ggplot2::ggplot(data) +
    ggplot2::aes(x = {{ x_var }}) +
    ggplot2::stat_density(color = "black", fill = NA) +
    ggplot2::lims(x = c(-3, 4)) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2))

  y_den <- ggplot2::ggplot(data) +
    ggplot2::aes(y = {{ y_var }}) +
    ggplot2::stat_density(color = "black", fill = NA) +
    ggplot2::lims(y = c(-3, 4)) +
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

