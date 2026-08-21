#' Fix plot
#'
#' @param p a plot object
#' @param size size of the viewport
#'
#' @returns draws the plot
#' @export
#'
#' @examples
#' p <- ggplot2::ggplot(mtcars,
#' ggplot2::aes(wt, mpg)) +
#'
#' ggplot2::geom_point()
#' plot_fixed(p)

plot_fixed <- function(p, size = 0.95) {
  grid::grid.newpage()

  grid::pushViewport(
    grid::viewport(
      x = 0.5,
      y = 0.5,
      width = grid::unit(size, "snpc"),
      height = grid::unit(size, "snpc"),
      just = c("center", "center")
    )
  )
  print(p, newpage = FALSE)
  grid::popViewport()
}
