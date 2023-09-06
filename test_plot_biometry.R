library(tidyverse)

source("R/readAllsheets.R")

hojas_validas <- "data/hojas_oficiales.csv" |> read.csv() |> pull()

f <- readAllsheets(upload_path = "exdata/ficha_campo.ods", 
                   valid_sheets = hojas_validas)

b <- f$especie_focal

b |> ggplot(aes(dmeno_cm)) +
  geom_histogram()
names(b)

feno <- b |> dplyr::select(especie:id_individuo, n_flores, n_frutos) |> 
  pivot_longer(cols = c(n_flores, n_frutos))

nombre_variables <- c(
  altura_cm = "Altura", 
  dmayor_cm = "Diámetro mayor",
  dmeno_cm = "Diámetro menor"
)


biometry <- b |> 
  dplyr::select(especie:id_individuo, altura_cm, dmayor_cm, dmeno_cm) |> 
  pivot_longer(cols = c(altura_cm, dmayor_cm, dmeno_cm)) |> 
  mutate(name = recode(name, !!!nombre_variables))
  
my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]


library(ggdist)


g <- ggplot(biometry, aes(x = as.factor(name), y = value, color = name, fill = name)) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")

g + ggdist::stat_halfeye(
  aes(fill = name, fill = after_scale(colorspace::lighten(fill, .7)))
)
  

g + 
  geom_boxplot(
    width = .2, fill = "white",
    size = 1.5, outlier.shape = NA
  ) +
  ggdist::stat_halfeye(
    adjust = .33, ## bandwidth
    width = .67, 
    color = NA, ## remove slab interval
    position = position_nudge(x = .15)
  ) +
  gghalves::geom_half_point(
    side = "l", 
    range_scale = .3, 
    alpha = .5, size = 3
  ) + coord_flip()

g + ggridges::geom_density_ridges(
  alpha = .7, size = 1.5
)

biometry |> 
  ggplot(aes(y=value, x = name)) + 
  geom_boxplot() + coord_flip() +
  xlab('') + 
  ylab('Valor (cm)') + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 30))




b |> 