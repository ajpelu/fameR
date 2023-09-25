#' preparaPopup function
#'
#' This function takes a tibble `x` and a vector of field names `mdfields` as 
#' input and prepares an HTML popup content based on the specified fields. 
#'
#' @param x A tibble containing data with columns 'campo' and 'valor'.
#' @param mdfields A vector of field names specifying which fields to include in 
#' the popup content.
#'
#' @return A character string containing HTML-formatted popup content.
#' @export
#' 
preparePopup <- function(x, mdfields = c("especie focal", "localidad", "site", "reference",
                                  "poblacion", "tratamiento", "elevacion", "fecha")) { 
  # Convert the fecha field to the desired format
  x <- x |> 
    mutate(
      valor = case_when(
        campo == "fecha" ~ format(lubridate::ymd(valor), "%Y-%d-%m"), 
        TRUE ~ valor
      )
      # ifelse(campo == "fecha", format(lubridate::ymd(valor), "%Y-%d-%m"), valor)
    )
  
  # Filtered the data 
  filtered_x <- x |> filter(campo %in% mdfields)
  if (nrow(filtered_x) == 0) {return("No matching fields found.")}
  
  # Mappin list of field names to rename (if they exist)
  rename_mapping <- list("especie focal" = "Taxon", 
                         "site" = "Código sitio",
                         "poblacion" = "Código población",
                         "tratamiento" = "Tratamiento",
                         "localidad" = "Localidad", 
                         "elevacion" = "Elevación",
                         "reference" = "Código plot",
                         "fecha" = "Fecha")
  
  # Rename fields based on the mapping (if they exist)
  filtered_x <- filtered_x |> 
    mutate(campo = ifelse(campo %in% names(rename_mapping), rename_mapping[campo], campo))
  
  result <- glue::glue(
    "<strong>{filtered_x$campo}:</strong> {filtered_x$valor}<br>"
  ) %>%
    paste(collapse = "")
  
  return(result)
}
