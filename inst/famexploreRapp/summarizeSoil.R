#' Summarize Soil Data
#'
#' This function takes a tibble containing soil data, performs data summarization, 
#' and returns a formatted and summarized tibble.
#'
#' @param x A tibble containing soil data.
#'
#' @return A tibble with summarized soil data.
#'
#' @details This function performs the following steps:
#'   1. Removes rows with missing values.
#'   2. Excludes certain columns from the analysis.
#'   3. Calculates mean, standard deviation, and standard error for numeric columns.
#'   4. Pivots the data into a long format.
#'   5. Renames columns for clarity.
#'   6. Pivots the data back to a wider format.
#'   7. Rounds numeric columns to three decimal places.
#'
#' @import dplyr
#' @import tidyr
#' @export
#' 
summarizeSoil <- function(x){
  results <- x |> 
    na.omit() |> 
    dplyr::select(-code_especie, -tratamiento, -referencia,
                  -limo_g, -limo_f, -arcilla, -arena, 
                  -referencia_suelo) |>
    summarize(across(everything(), 
                     list(mean = ~mean(., na.rm = TRUE), 
                          sd = ~sd(., na.rm = TRUE), 
                          se = ~{sd(., na.rm = TRUE)/sqrt(length(.))} ))) |> 
    pivot_longer(cols = everything(), 
                 names_to = c(".value", "parameter"), 
                 names_pattern = "(.*)_(mean|sd|se)") |> 
    rename("Carbono Org\u00e1nico (%)" = CO,
           "F\u00f3sforo (ppm)" = P,
           "Materia Org\u00e1nica (%)" = MO,
           "Nitr\u00f3geno Total (%)" = N_total,
           "Carbono Total (%)" = C_total,
           "Conductividad el\u00e9ctrica (\u00b5S/cm)" = CE,
           "Saturaci\u00f3n (%)" = sat,
           "Fluoruros (mg/L)" = fluoruro,
           "Cloruros (mg/L)" = cloruro,
           "Nitratos (mg/L)" = nitrato,
           "Nitritos (mg/L)" = nitrito,
           "Sulfatos (mg/L)" = sulfato) |>
    pivot_longer(cols = -parameter, names_to = "Variable") |> 
    pivot_wider(names_from = parameter, values_from = value) |> 
    dplyr::rename(`Media` = mean) |> 
    mutate(across(where(is.numeric), ~ round(., 3)))
  return(results)
}
