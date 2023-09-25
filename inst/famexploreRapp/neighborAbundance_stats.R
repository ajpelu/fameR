#' Calculate Neighbor Abundance Statistics
#'
#' This function calculates various neighbor abundance statistics based on input data.
#'
#' @param data A data frame containing relevant columns, including "individuo" 
#' (individual ID), "diam_muestreo_vecindad_cm" (neighborhood sampling diameter 
#' in centimeters), "especie_vecina" (neighboring species), and "n_vecino" 
#' (number of neighbors).
#' @param units A character vector specifying the units for area calculation. 
#' Options are "m2" (square meters), "dm2" (square decimeters), and "cm2" (square 
#' centimeters).
#' @param focal_sp A character string specifying the focal species.
#'
#' @return A list containing two data frames:
#' \describe{
#'   \item{output}{A data frame with individual-level neighbor abundance statistics.}
#'   \item{output_summary}{A data frame with summarized neighbor abundance statistics.}
#' }
#'
#' @export

neighborAbundance_stats <- function(data, 
                                    units = c("m2", "dm2", "cm2"), 
                                    focal_sp) {
  # Select relevant columns from the data
  x <- data |> 
    dplyr::select(one_of("individuo", "diam_muestreo_vecindad_cm", "especie_vecina", "n_vecino"))
  
  # Calculate the area based on selected units
  area_cm2 <- (unique(x$diam_muestreo_vecindad_cm)/2)^2*pi
  if (units == "cm2") { 
    area <- area_cm2
  } else if (units == "dm2") {
    area <- area_cm2 / (10 * 10)
  } else if (units == "m2") {
    area <- area_cm2 / (100 * 100)
  }
  
  # Calculate the total number of neighboring species and distinct neighboring species
  nsps_tot <- x |> group_by(individuo) |> 
    dplyr::distinct(especie_vecina) |> 
    summarise(n_sps_vecinas = n())
  
  nsps_tot_dist <- x |> group_by(individuo) |> 
    filter(especie_vecina != focal_sp) |> 
    dplyr::distinct(especie_vecina) |>
    summarise(n_sps_vecinas_dist = n())
  
  # Calculate the total number of neighbors and neighbor density
  nvec_tot <- x |> group_by(individuo) |> 
    summarise(n_total_vecinos = sum(n_vecino, na.rm = FALSE), 
              den_vecinos = n_total_vecinos / area)
  
  output <- inner_join(nvec_tot, nsps_tot) |> 
    inner_join(nsps_tot_dist)
  
  # Summarize the statistics and pivot them into a tidy format
  output_summary <- output |> 
    summarise(
      across(
        c(n_total_vecinos, den_vecinos, n_sps_vecinas, n_sps_vecinas_dist),
        list(
          avg = mean,
          se = ~sd(.)/sqrt(length(.)),
          min = min,
          max = max
        ),
        .names = "{.col}_{.fn}"
      )
    ) |> 
    pivot_longer(everything()) |>
    mutate(stats = str_extract(name, "(?<=_)[^_]+$"),
           variable = str_remove(name, "_[^_]+$")) |> 
    dplyr::select(-name) |> 
    pivot_wider(names_from = stats, values_from = value)
  
  results <- list(output, output_summary)
  return(results)
}
