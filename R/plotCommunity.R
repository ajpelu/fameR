#' Plot Plant Community Data
#'
#' This function generates a bar plot to visualize plant community data based on 
#' the specified method.
#'
#' @param x A list containing data frames for different aspects of the study.
#' @param ... others ggplot parameters 
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
    "método cobertura" = x$com_veg_cobertura,
    "método contacto" = x$com_veg_contactos,
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
  
  ggplot(m, aes(x = forcats::fct_rev(especie_acomp), y = cobertura)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(x = "Especie",
         y = "Cobertura (%)") +
    theme_minimal() + 
    theme(axis.text.y = element_text(face = "italic")) + 
    coord_flip() +
    theme(
      axis.text = element_text(size = axis_text_size), 
      axis.title = element_text(size = axis_title_size)
    ) 
} 
