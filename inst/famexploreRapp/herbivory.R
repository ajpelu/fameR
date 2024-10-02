#' Calculate Herbivory Metrics and Create a Plot
#'
#' This function calculates herbivory metrics from a given dataset and creates a
#' corresponding ggplot.
#'
#' @param data A data frame containing the herbivory data with columns including
#'   'comido_pct', 'id_individuo', and other relevant variables.
#' @param bar_color The color for the bar in the ggplot. Default is blue.
#' @param point_fill The fill color for points in the ggplot. Default is green.
#' @param point_color The outline color for points in the ggplot. Default is black.
#' @param point_alpha The alpha (transparency) for points in the ggplot. Default is 0.9.
#'
#' @return A list containing:
#'   - 'damage': A tibble with herbivory metrics.
#'   - 'plot_damage': A ggplot object displaying herbivory metrics.
#'
#' @export

herbivory <- function(data, bar_color = "blue", point_fill = "green", point_color = "black", point_alpha = 0.9) {
  # Filter and calculate damage metrics
  damage <- data |>
    dplyr::filter(comido_pct > 0) |>
    dplyr::group_by(id_individuo) |>
    dplyr::summarize(
      leaf_damages_pct = (n() / 5) * 100,
      mean_damage = mean(comido_pct),
      sd_damage = stats::sd(comido_pct),
      se_damage = sd_damage / sqrt(length(comido_pct))
    ) |>
    tidyr::complete(id_individuo = unique(data$id_individuo), 
                    fill = list(leaf_damages_pct = 0, mean_damage = NA, sd_damage = NA, se_damage = NA)) |>
    dplyr::mutate(id_individuo = as.factor(id_individuo))
  
  # Create the ggplot
  plot_damage <- ggplot2::ggplot(damage, aes(x = id_individuo)) +
    ggplot2::geom_bar(stat = "identity", aes(y = mean_damage), fill = bar_color) +
    ggplot2::geom_errorbar(aes(ymin = mean_damage - sd_damage, ymax = mean_damage + sd_damage), width = 0.2, colour = bar_color) +
    ggplot2::geom_point(aes(y = -1, size = leaf_damages_pct), shape = 21, color = point_color, fill = point_fill, alpha = point_alpha) +
    ggplot2::labs(x = "Individuo id", y = "Da\u00f1o medio de las hojas comidas (%) ") +
    ggplot2::scale_size_continuous(range = c(2, 10), name = "% hojas comidas") +
    ggplot2::theme_minimal()
  
  results <- list(
    "damage" = damage,
    "plot_damage" = plot_damage
  )
  
  return(results)
}