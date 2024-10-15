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
#'
#' @export
biometryPlot <- function(x, base_size, axis_text_size = 24, ...){
  
  if (!inherits(x, "list")) {
    stop("Error: 'x' debe ser una lista que contenga el elemento 'especie_focal'.")
  }
  
  if (!"especie_focal" %in% names(x)) {
    stop("Error: La lista 'x' debe contener el elemento 'especie_focal'.")
  }
  
  # Verificar que 'x$especie_focal' es un data frame
  if (!inherits(x$especie_focal, "data.frame")) {
    stop("Error: 'x$especie_focal' debe ser un data frame.")
  }
  
  # Columnas requeridas
  required_columns <- c("especie_code", "id_individuo", "altura_cm", "dmayor_cm", "dmenor_cm")
  
  # Verificar que las columnas requeridas están presentes
  missing_columns <- setdiff(required_columns, names(x$especie_focal))
  if (length(missing_columns) > 0) {
    stop(paste("Error: Las siguientes columnas faltan en 'x$especie_focal':", paste(missing_columns, collapse = ", ")))
  }
  
  # Verificar que 'base_size' es numérico y positivo
  if (!is.numeric(base_size) || length(base_size) != 1 || base_size <= 0) {
    stop("Error: 'base_size' debe ser un número positivo.")
  }
  
  # Verificar que 'axis_text_size' es numérico y positivo
  if (!is.numeric(axis_text_size) || length(axis_text_size) != 1 || axis_text_size <= 0) {
    stop("Error: 'axis_text_size' debe ser un número positivo.")
  }
  
  nombre_variables <- c(
    altura_cm = "Altura", 
    dmayor_cm = "Di\u00e1metro mayor",
    dmenor_cm = "Di\u00e1metro menor")
  
  biometry <- x$especie_focal |> 
    dplyr::select(especie_code:id_individuo, altura_cm, dmayor_cm, dmenor_cm) |> 
    pivot_longer(cols = c(altura_cm, dmayor_cm, dmenor_cm)) |> 
    mutate(name = recode(name, !!!nombre_variables))
  
  my_pal <- c("#7F3C8D","#3969AC","#E68310","#11A579")
  
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