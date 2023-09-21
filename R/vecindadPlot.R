#' Generate Vecindad Plot
#'
#' This function generates a bar plot with error bars that visualizes the abundance
#' of neighboring species.
#'
#' @param x A data frame containing data for plotting.
#'   - `especie_vecina`: The neighboring species.
#'   - `ab_mean`: The mean abundance of the neighboring species.
#'   - `ab_se`: The standard error of the mean abundance.
#' @param ... others ggplot parameters
#' @param axis_text_size The size of the axis text. Default value=16
#' @param axis_title_size The size of the axis title. Default value=17
#' 
#'
#' @return A bar plot with error bars.
#'
#' @importFrom ggplot2 ggplot geom_bar geom_errorbar labs theme_minimal theme coord_flip element_text
#'
#' @export
vecindadPlot <- function(x, 
                         axis_text_size = 16, 
                         axis_title_size = 17, ...){
  g <- ggplot::ggplot(x, aes(x = especie_vecina, y = ab_mean)) +
    ggplot::geom_bar(stat = "identity", fill = "blue") +
    ggplot::geom_errorbar(aes(ymin = ab_mean - ab_se, 
                      ymax = ab_mean + ab_se),
                  width = 0.25, 
                  position = position_dodge(width = 0.9), 
                  colour = "blue") +
    ggplot::labs(x = "Especie Vecina",
         y = "Abundancia (n. ind)") +
    ggplot::theme_minimal() + 
    ggplot::coord_flip() +
    ggplot::theme(
      axis.text.y = element_text(face = "italic"),
      axis.text = element_text(size = axis_text_size), 
      axis.title = element_text(size = axis_title_size)
    ) 
  
  return(g)
  
}