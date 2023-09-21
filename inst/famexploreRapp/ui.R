#### famexploreR ui 

# load pkg
library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(purrr)
library(rcartocolor)
library(ggdist)
library(gghalves)
library(sf)
library(mapSpain)
library(leaflet)
library(lubridate)
library(ggtern)
library(stringr)
library(plotly)
library(vegan)
library(formattable)
library(kableExtra)


hojas_validas <- load("../data_shiny/hojas_validas.rda")

cards <- list(
  metadatos = card(full_screen = TRUE, htmlOutput("metadataText")), 
  soil_parameters = card(card_header("Parámetros"), 
                 tableOutput("suelos_tabla")), 
  biometria = card(full_screen = TRUE, plotOutput("biometria")),
  biometria_stats = card(full_screen = TRUE, tableOutput("biometria_stats")),
  floracion = card(full_screen = TRUE, plotlyOutput("plotfloracion")),
  mapa = card(full_screen = TRUE, leaflet::leafletOutput("map")),
  suelos = card(full_screen = FALSE, plotOutput("suelos")), 
  vecindad = card(full_screen = TRUE, card_header("Vecindad"), 
                  plotOutput("vecindad")), 
  comunidad = card(full_screen = TRUE, 
                   card_header("Composición de la comunidad"), 
                   plotlyOutput("plotcomunidad")), 
  herbivoria_plot = card(full_screen = TRUE, plotlyOutput("plotherbivoria")), 
  herbivoria_tabla = card(full_screen = FALSE, formattableOutput("tablaherbivoria"))
)

### Value box 
vb <- list(
  temp_media = value_box(
    title = "Temperatura media del Suelo",
    showcase = bsicons::bs_icon("thermometer"),
    value = htmlOutput("meanTemp"),
    theme_color = "secondary"
  ),
  humedad_media = value_box(
    title = "Humedad media del Suelo",
    showcase = bsicons::bs_icon("moisture"),
    value = htmlOutput("meanHumedad"),
    theme_color = "secondary"
  ),
  vecinos_abundancia = value_box(
    title = shiny::h3("Abundancia vecinos", style = "text-align: center;"),
    showcase = bsicons::bs_icon("align-center"),
    value = htmlOutput("mean_vecinos_ab"),
    theme_color = "dark"
  ),
  vecinos_sps = value_box(
    title = shiny::h3("N especies vecinas", style = "text-align: center;"),
    showcase = icon("pagelines", class = "fa-3x"),
    value = htmlOutput("mean_vecinos_sp"),
    # p(htmlOutput("lu_vecinos_sp")),
    theme_color = "dark"
  ),
  comunidad_richness = value_box(
    title = "Riqueza de Especies",
    showcase = icon("pagelines", class = "fa-3x"),
    value = textOutput("richness"),
    theme_color = "dark"
  ), 
  comunidad_shannon = value_box(
    title = "Diversidad de Shannon",
    showcase = icon("seedling", class = "fa-3x"),
    value = textOutput("diversity_shannon"),
    theme_color = "dark"
  ), 
  comunidad_simpson = value_box(
    title = "Diversidad de Simpson",
    showcase = icon("seedling", class = "fa-3x"),
    value = textOutput("diversity_simpson"),
    theme_color = "dark"
  ), 
  comunidad_evenness = value_box(
    title = "Índice de Equitatividad (Pileou's)",
    showcase = icon("leaf", class = "fa-3x"),
    value = textOutput("evenness_pielou"),
    theme_color = "dark"
  )
)

link_github <- tags$a(shiny::icon("github"), "Source code", href = "https://github.com/ajpelu/famexploreR/", target = "_blank")

ui <- page_navbar(
  title = "famExploreR v 1.0.0",
  sidebar = sidebar(
    shiny::h4("Estadillo de campo"), 
    fileInput("upload", label = "", 
              accept = c(".ods", ".xlsx"),
              placeholder = "Seleccione el archivo a subir"),
    shiny::br(), 
    shiny::br(),
    shiny::h4("Información espacial"),
    fileInput(inputId = "upload_spat",
              label = "Cargar shapefile ('.shp','.dbf','.sbn','.sbx','.shx','.prj')",
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    shiny::br(), 
    shiny::br(),
    shiny::br(), 
    shiny::br(),
    downloadButton("generateReport", "Generar Informe")
    
  ),
  navset_card_tab(
    nav_panel("Datos generales", cards[["metadatos"]]),
    nav_panel("Localización", cards[["mapa"]]),
    nav_menu("Suelos",
             nav_panel("Parámetros",
                       layout_columns(fill = TRUE,
                                      col_widths = c(-2, 4, 4, -2),
                                      vb[["temp_media"]],
                                      vb[["humedad_media"]]),
                       layout_columns(fill = TRUE,
                                      col_widths = c(-2, 8, -2),
                                      cards[["soil_parameters"]])),
             nav_panel("Diagrama Ternario", 
                       layout_columns(fill = TRUE,
                                      col_widths = c(-2, 8, -2),
                                      cards[["suelos"]]))),
    nav_menu("Especie Focal", 
             nav_panel("Biometria", 
                       layout_columns(
                         fill = TRUE,
                         col_widths = c(-2, 8, -2),
                         cards[["biometria"]]),
                       layout_columns(
                         fill = TRUE,
                         col_widths = c(-3, 6, -3),
                         cards[["biometria_stats"]])),
             nav_panel("Floración / Fructificación", 
                       layout_columns(
                         fill = TRUE,
                         col_widths = c(-2, 8, -2),
                         cards[["floracion"]]))),
    nav_menu("Herbivoría",
             nav_panel("Gráfico", cards[["herbivoria_plot"]]), 
             nav_panel("Tabla",
                       fill = FALSE,
                       layout_columns(
                         col_widths = c(-4, 4, -4),
                         cards[["herbivoria_tabla"]])
                       )),
    nav_panel(
      "Vecindad",
      layout_columns(
        fill = FALSE,
        col_widths = c(-3, 3, 3, -3),
        vb[["vecinos_abundancia"]],
        vb[["vecinos_sps"]]
      ),
      layout_columns(
        fill = TRUE, 
        col_widths = c(-3, 6, -3),
        cards[["vecindad"]]),
      layout_columns(
        fill = FALSE, 
        col_widths = c(-3, 6, -3),
        downloadButton("downloadVecindad", "Download Plot"))
    ), 
    nav_panel(
      "Comunidad",
      layout_columns(
        fill = FALSE,
        col_widths = c(-1,3,3,3,-2),
        vb[["comunidad_shannon"]], 
        vb[["comunidad_richness"]],
        vb[["comunidad_evenness"]]
      ),
      layout_columns(
        fill = FALSE, 
        col_widths = c(-2, 8, -2),
        cards[["comunidad"]])
      # downloadButton("downloadVecindad", "Download Plot")
    ) 
  ),
  nav_spacer(),
  nav_menu(
    title = "About",
    align = "right",
    nav_item(link_github)
  )
)