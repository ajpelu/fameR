#' Generate a Ternary Plot with Customizable Axis Labels
#'
#' This function generates a ternary plot using the ggtern package with customizable
#' axis labels. The function allows specifying the variable names for the x, y, 
#' and z axes. It also capitalizes the first letter of each variable name for axis labels.
#'
#' @param data A data frame containing the data to be plotted.
#' @param xvar The name of the variable to be plotted in the x-axis of the ternary plot.
#' @param yvar The name of the variable to be plotted in the y-axis of the ternary plot.
#' @param zvar The name of the variable to be plotted in the z-axis of the ternary plot.
#' @param bsize The base font size for the plot.
#' @param point_size The size of the points.
#'
#' @return A ggtern plot object displaying the ternary plot.
#'
#' @details This function creates a ternary plot, where data points are represented
#' by points in a triangular coordinate system. The `xvar`, `yvar`, and `zvar` 
#' arguments allow you to specify which variables from the `data` argument should 
#' be used for each axis. The function also capitalizes the first letter of each 
#' variable name for use in the axis labels. You can adjust the base font size 
#' (`bsize`) for the plot to control the text size.
#'
#' @seealso \code{\link{ggtern}} for more information on creating ternary plots using ggtern.
#'
#' @examples
#' \dontrun{
#' # Example usage of the custom function with customized variable names
#' data <- data.frame(
#'   arena = c(0.4, 0.3, 0.2, 0.1),
#'   arcilla = c(0.3, 0.4, 0.2, 0.1),
#'   limo = c(0.3, 0.3, 0.4, 0.1)
#' )
#' 
#' generateTernaryPlot(data, xvar = 'arena', yvar = 'arcilla', 
#' zvar = 'limo')
#' }
#'
#'
#' @import ggtern
#' @importFrom stringr str_to_title
#'
ternaryPlot <- function(data, xvar, yvar, zvar, bsize, point_size, ...) {
  # Capitalize the first letter of variable names for axis labels
  xvar_label <- str_to_title(xvar)
  yvar_label <- str_to_title(yvar)
  zvar_label <- str_to_title(zvar)
  
  # Create the ternary plot
  plot <- ggtern(data = data, aes_string(x = xvar, y = yvar, z = zvar)) +
    geom_crosshair_tern(colour = "gray") +
    geom_point(size = point_size) +
    labs(
      yarrow = paste(yvar_label, " (%)"),
      zarrow = paste(zvar_label, " (%)"),
      xarrow = paste(xvar_label, " (%)")
    ) +
    xlab("") + ylab("") + zlab("") +
    theme_showarrows() +
    theme_bvbw(base_size = bsize)
  
  return(plot)
}



