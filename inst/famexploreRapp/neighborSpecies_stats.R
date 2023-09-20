#' Summarizes neighbor species data 
#' 
#' This function takes a data frame or tibble and groups it by neighbor species 
#' (`especie_vecina`). For each neighbor species, the function calculates the 
#' number of plots where each species has been recorded, as well as several 
#' statistics for the neighbor species: the mean, standard error, minimum, 
#' and maximum abundance of the neighbor species in the plots where it is present.
#'
#' @param data A data frame or tibble containing the data to be summarized.
#'
#' @return A tibble with the following columns:
#'   - especie_vecina: The grouping variable (neighbor species).
#'   - ab_mean: The mean abundance of the neighbor species.
#'   - ab_se: The standard error of the mean abundance.
#'   - ab_min: The minimum abundance of the neighbor species.
#'   - ab_max: The maximum abundance of the neighbor species.
#'   - present_at: The count of plots where each neighbor species is recorded.
#'   - present_at_per: The percentage of the sampled plots (`individuo`) where each 
#'   neighbor species is recorded.
#'
#' @export
neighborSpecies_stats <- function(data) {
  # Check if 'data' is a data frame or tibble
  if (!is.data.frame(data) && !is.data.frame(as.data.frame(data))) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  
  # Check if 'especie_vecina' and 'n_vecino' columns exist 
  required_cols <- c("especie_vecina", "n_vecino", "individuo")
  missing_cols <- setdiff(required_cols, colnames(data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Required columns missing:", paste(missing_cols, collapse = ", ")))
  }
  
  nplots <- length(unique(data$vecindad$individuo))
  
  result <- data %>%
    group_by(especie_vecina) %>%
    summarise(ab_mean = mean(n_vecino), 
              ab_se = sd(n_vecino) / sqrt(length(n_vecino)),
              ab_min = min(n_vecino), 
              ab_max = max(n_vecino),
              present_at = length(n_vecino), 
              present_at_per = round((present_at / nplots)*100, 2))
  
  return(result)
}
