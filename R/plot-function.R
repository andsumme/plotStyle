#' Custom Scatter Plot Function
#'
#' Creates a scatter plot with consistent and publication-ready styling.
#'
#' @param x A string specifying the column name for the x-axis.
#' @param y A string specifying the column name for the y-axis.
#' @param data A data frame containing the variables for the plot.
#' @param color A string specifying the color of the points. Default is "blue".
#' @param theme A string specifying the theme to apply. Options are "default" (with a white background) or "minimal" (with minimal grid lines). Default is "default".
#' @param title A string specifying the title of the plot. Default is "Custom Scatter Plot".
#' @return A ggplot object, which can be further customized or rendered.
#' @examples
#' # Create a scatter plot with default settings
#' custom_scatter("mpg", "wt", mtcars)
#'
#' # Create a scatter plot with a custom color, minimal theme, and custom title
#' custom_scatter("mpg", "wt", mtcars, color = "pink", theme = "minimal", title = "My Custom Plot")
#' @export
custom_scatter <- function(x, y, data, color = "purple", theme = "default", title = "Custom Scatter Plot") {
  library(ggplot2)

  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(color = color) +
    labs(title = title, x = x, y = y)

  if (theme == "default") {
    p <- p + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  } else if (theme == "minimal") {
    p <- p + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
  }

  return(p)
}

#' Academic Theme for Plots
#'
#' A clean and minimal theme designed for academic publications.
#'
#' This theme removes background elements and uses light grid lines, making it suitable for academic papers.
#'
#' @return A ggplot2 theme object that can be applied to a ggplot object with `+ theme_academic()`.
#' @examples
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_academic()
#' @export
theme_academic <- function() {
  library(ggplot2)
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
}

#' Vibrant Color Palette
#'
#' A custom color palette with vibrant and eye-catching colors for use in plots.
#'
#' This palette can be applied using `scale_color_manual()` for categorical variables
#' or `scale_color_gradient()` for continuous variables to add vibrant colors to ggplot visualizations.
#'
#' @return A vector of hex color codes.
#' @examples
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(color = factor(cyl))) +
#'   scale_color_manual(values = palette_vibrant())
#'
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(color = mpg)) +
#'   scale_color_gradientn(colors = palette_vibrant())
#' @export
palette_vibrant <- function() {
  c("#E64B95", "#4DBBE5", "#00A007", "#3C5493", "#F39B7F", "#8494B4", "#91D1C2")
}
