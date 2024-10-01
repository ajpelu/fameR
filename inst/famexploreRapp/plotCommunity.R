#' Plot Plant Community Data
#'
#' This function generates a bar plot to visualize plant community data based on 
#' the specified method.
#'
#' @param x A list containing data frames for different aspects of the study.
#' @param ... others ggplot parameters 
#' @param axis_text_size The size of the axis text. Default value=16
#' @param axis_title_size The size of the axis title. Default value=17
#'
#' @return A bar plot visualizing plant community data.
#'
#' @details The function first extracts the method used for data collection from 
#' the 'datos_generales' data frame. Based on the method, it selects the 
#' corresponding plant community data frame and creates a bar plot to visualize
#' the coverage of plant species.
#'
#' @export

plotCommunity <- function(x, 
                          axis_text_size = 16, 
                          axis_title_size = 17, ...) { 
  
  # Extract the method from general data
  metodo <- x$datos_generales |> 
    filter(campo == "comunidad_vegetal")  |>
    pull(valor)
  
  # Select plan community dataset according to method
  comunidad <- switch(
    metodo,
    "m\u00e9todo cobertura" = x$com_veg_cobertura,
    "m\u00e9todo contacto" = x$com_veg_contactos,
    "There is not data about plant community" = NULL
  )
  
  if (is.null(comunidad)) {
    cat("There is not data about plant community.\n")
    return(NULL)
  }
  
  # Prepare data
  m <- comunidad  |>
    select(referencia, especie_acomp, cobertura) |> 
    na.omit()
  
  ggplot2::ggplot(m, aes(x = forcats::fct_rev(especie_acomp), y = cobertura)) +
    ggplot2::geom_bar(stat = "identity", fill = "blue") +
    ggplot2::labs(x = "Especie",
         y = "Cobertura (%)") +
    ggplot2::theme_minimal() + 
    ggplot2::theme(axis.text.y = element_text(face = "italic")) + 
    ggplot2::coord_flip() +
    ggplot2::theme(
      axis.text = element_text(size = axis_text_size), 
      axis.title = element_text(size = axis_title_size)
    ) 
} 
