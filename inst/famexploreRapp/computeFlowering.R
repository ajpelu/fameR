#' Compute Summary Statistics for Flowering Variables
#'
#' This function computes summary statistics for specified flowering variables
#' in a given data frame.
#'
#' @param x A data frame containing the variables of interest.
#' @param var_interest A character vector of variable names to be summarized.
#'
#' @return A data frame containing summary statistics for the specified variables.
#'
#' @details
#' This function calculates the following summary statistics for each specified
#' variable:
#'
#' - **variable**: The name of the variable.
#' - **n_ind**: The number of individuals with non-zero values.
#' - **pct_ind**: The percentage of individuals with non-zero values.
#' - **mean_count**: The mean value of the variable for individuals with non-zero values.
#' - **sd_count**: The standard deviation of the variable for individuals with non-zero values.
#' - **se_count**: The standard error of the mean for the variable.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   n_flores = c(0, 2, 3, 0, 5),
#'   n_frutos = c(0, 0, 4, 0, 6)
#' )
#' count_columns <- c("n_flores", "n_frutos")
#' result_summary <- computeFlowering(data, count_columns)
#' }
#'
#' @import dplyr
#' @import purrr
#'
#' @export
#' 
computeFlowering <- function(x, var_interest) {
  summary_list <- purrr::map(var_interest, ~ {
    column_name <- rlang::ensym(.x)
    x |> 
      dplyr::filter(!!column_name > 0) |> 
      dplyr::summarize(
        variable = as.character(.x),
        n_ind = n(),
        pct_ind = (n() / nrow(x)) * 100,
        mean_count = mean(!!column_name),
        sd_count = stats::sd(!!column_name),
        se_count = sd_count / sqrt(n())
      )
  })
  
  result_summary <- dplyr::bind_rows(summary_list)
  return(result_summary)
}