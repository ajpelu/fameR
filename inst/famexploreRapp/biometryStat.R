#' Calculate Biometry Statistics
#'
#' This function calculates various statistics (mean, standard deviation, standard error, 
#' minimum, and maximum) for specified biometric variables in a data frame.
#'
#' @param x A data frame containing biometric data.
#' @param variables A character vector specifying the names of the biometric variables
#'   for which statistics will be calculated. Default is c('altura_cm', 'dmayor_cm', 'dmenor_cm').
#'
#' @return A data frame with columns for each statistic (mean, sd, se, min, max) and a
#'   corresponding variable column.
#'
#' @import dplyr tidyr purrr tibble
#' @export
#' 
#' 
biometryStat <- function(x, 
                         variables = c("altura_cm", "dmayor_cm", "dmenor_cm")) {
  result <- x |>
    select(all_of(variables)) |>
    map_dfr(~ tibble(
      mean = mean(.x, na.rm = TRUE),
      sd = sd(.x, na.rm = TRUE),
      se = sd(.x, na.rm = TRUE) / sqrt(length(.x)),
      min = min(.x, na.rm = TRUE),
      max = max(.x, na.rm = TRUE)
    ), .id = "variable") |>
    mutate(variable = case_when(
      variable == "altura_cm" ~ "Altura (cm)",
      variable == "dmayor_cm" ~ "Di\u00e1metro mayor (cm)",
      variable == "dmenor_cm" ~ "Di\u00e1metro menor (cm)"
    ))
  return(result)
}
 




