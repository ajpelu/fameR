#' Create a bar plot with error bars for flowering/fructification data.
#'
#' This function generates a bar plot with error bars for flowering data, allowing
#' you to specify whether to use standard error ("se") or standard deviation ("sd")
#' error bars.
#'
#' @param x A data frame containing the flowering data.
#' @param error The type of error bars to use. Should be "se" (standard error) or
#'   "sd" (standard deviation). Default is "se".
#' @param bar_color The color for the bars in the plot. Default is "blue".
#' @param ... others ggplot parameters 
#'
#' @return A ggplot2 object representing the bar plot with error bars.
#'
#' @seealso \code{\link{ggplot2}}, \code{\link{geom_bar}}, \code{\link{geom_errorbar}}
#'
#' @import dplyr tidyr ggplot2
#' @export
plotFlowering <- function(x, error = "se", bar_color = "blue", ...) {
  if (error == "se") {
    plot_data <- x |>
      select(-starts_with("sd")) |>
      pivot_longer(c("mean_count", "pct_ind")) |>
      rename(deviation = starts_with("se")) |>
      mutate(
        nameF = case_when(
          name == "mean_count" ~ "Valores medios",
          name == "pct_ind" ~ "Individuos (%)"
        ),
        variable = case_when(
          variable == "n_flores" ~ "Flores",
          variable == "n_frutos" ~ "Frutos"
        )
      )
    
    error_data <- plot_data |>
      filter(name == "mean_count")
  } else if (error == "sd") {
    plot_data <- x |>
      select(-starts_with("se")) |>
      pivot_longer(c("mean_count", "pct_ind")) |>
      rename(deviation = starts_with("sd")) |>
      mutate(
        nameF = case_when(
          name == "mean_count" ~ "Valores medios",
          name == "pct_ind" ~ "Individuos (%)"
        ),
        variable = case_when(
          variable == "n_flores" ~ "Flores",
          variable == "n_frutos" ~ "Frutos"
        )
      )
    
    error_data <- plot_data |>
      filter(name == "mean_count")
  } else {
    stop("Invalid 'error' argument. Use 'se' or 'sd'.")
  }
  
  p <- ggplot(plot_data, aes(x = variable, y = value)) +
    geom_bar(stat = "identity", fill = bar_color, width = 0.4) +
    geom_errorbar(
      data = error_data,
      aes(ymin = value - deviation, ymax = value + deviation),
      width = 0.2, colour = bar_color, position = "identity", show.legend = FALSE
    ) +
    facet_wrap(~nameF, ncol = 1, scales = "free_y") +
    theme_minimal() +
    ylab("") +
    xlab("")
  
  return(p)
}
