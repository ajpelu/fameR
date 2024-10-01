#' Create Biometry Plot
#'
#' This function generates a biometry plot based on the provided data frame. 
#' It visualizes the measurements of height, major diameter, and minor diameter 
#' for a specific species.
#'
#' @param x A data frame containing biometric measurements for a species.
#' @param base_size The base_size for the plot 
#' @param axis_text_size The size of the axis text
#' @param ... others ggplot parameters 
#'
#' @return A biometry plot visualizing height, major diameter, and minor diameter.
#'
#' @details
#' The function takes a data frame `x` containing the following columns:
#'
#' - `especie_code`: The species code.
#' - `id_individuo`: The individual identifier.
#' - `altura_cm`: Height in centimeters.
#' - `dmayor_cm`: Major diameter in centimeters.
#' - `dmenor_cm`: Minor diameter in centimeters.
#'
#' The function creates a boxplot for each measurement type and uses custom colors. 
#' It also includes half-eye plots and half-point plots.
#'
#' @importFrom ggplot2 ggplot scale_color_manual scale_fill_manual geom_boxplot theme coord_flip
#' @importFrom ggdist stat_halfeye
#' @importFrom gghalves geom_half_point
#' @importFrom rcartocolor carto_pal
#'
#' @export
biometryPlot <- function(x, base_size, axis_text_size = 24, ...){
  
  nombre_variables <- c(
    altura_cm = "Altura", 
    dmayor_cm = "Di\u00e1metro mayor",
    dmenor_cm = "Di\u00e1metro menor")
  
  biometry <- x$especie_focal |> 
    dplyr::select(especie_code:id_individuo, altura_cm, dmayor_cm, dmenor_cm) |> 
    pivot_longer(cols = c(altura_cm, dmayor_cm, dmenor_cm)) |> 
    mutate(name = recode(name, !!!nombre_variables))
  
  my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]
  
  ggplot2::ggplot(biometry, aes(x = as.factor(name), y = value, color = name, fill = name)) +
    ggplot2::scale_color_manual(values = my_pal, guide = "none") +
    ggplot2::scale_fill_manual(values = my_pal, guide = "none") + 
    ggplot2::geom_boxplot( width = .2, fill = "white", size = 1.5, outlier.shape = NA) +
    ggplot2::theme(
      axis.text = element_text(size = axis_text_size)) + 
    ggdist::stat_halfeye(
      adjust = .33, ## bandwidth
      width = .67, 
      color = NA, ## remove slab interval
      position = position_nudge(x = .15)
    ) +
    gghalves::geom_half_point(
      side = "l", 
      range_scale = .3, 
      alpha = .5, size = 3
    ) + coord_flip() +
    ggplot2::theme_minimal(base_size = base_size) +
    xlab("") + ylab("")
  
}