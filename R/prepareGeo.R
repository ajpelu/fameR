#' Prepare Geo Spatial Data
#'
#' This function takes a data frame containing information about geographic 
#' coordinates and prepares it for spatial analysis by converting it into a 
#' Simple Features (sf) object.
#'
#' @param x A data frame containing at least the columns 'campo' and 'valor', 
#' where 'campo' specifies the type of information (e.g., 'crs', 'coord_x', 
#' 'coord_y', 'elevacion'), and 'valor' contains the corresponding values.
#' @return A Simple Features (sf) object with spatial coordinates and attributes.
#' @details This function creates an sf object with spatial coordinates and 
#' attributes, from an input data frame thata contain specific columns 
#' for 'campo' and 'valor'. 
#' 
#' @import dplyr
#' @import tidyr
#' @import sf
prepareGeo <- function(x) {
  # Check if input is a valid data frame
  if (!is.data.frame(x) || is.null(x)) {
    stop("Input must be a non-null data frame.")
  }
  
  
  # Check if input data frame contains the required columns
  if (!all(c("campo", "valor") %in% colnames(x))) {
    stop("Input data frame must have columns named 'campo' and 'valor'.")
    } 

  # Extract, filter, and transform data
  d_geo <- x |> 
    filter(campo %in% c("crs", "coord_x", "coord_y", "elevacion")) |> 
    pivot_wider(names_from = campo, values_from = valor) |> 
    separate(crs, into = c("epsg", "crs_desc"), sep = " \\| ") |> 
    mutate(across(-crs_desc, as.numeric)) 
  
  # Create sf object with spatial coordinates
  geo <- st_as_sf(d_geo, coords = c("coord_x", "coord_y"), crs = d_geo$epsg)
  
  # Combine sf object with its coordinates
  result <- cbind(geo, st_coordinates(geo))
  
  return(result)  
}
