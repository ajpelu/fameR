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


famExmploreR_app <- function(...) { 
  
  
# hojas_validas <- 'data/hojas_oficiales.csv' |> read.csv() |> pull()
# usethis::use_data(hojas_validas)

hojas_validas <- hojas_validas

cards <- list(
  metadatos = card(full_screen = TRUE, htmlOutput("metadataText")), 
  humedad = card(card_header("Datos de Humedad y Temperatura del suelo"), 
                 tableOutput("humedad")), 
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
  herbivoria_tabla = card(full_screen = FALSE, tableOutput("tablaherbivoria"))
)

### Value box 
vb <- list(
  temp_media = value_box(
    title = "Temperatura media del Suelo",
    showcase = bsicons::bs_icon("thermometer"),
    value = textOutput("meanTemp"),
    theme_color = "secondary"
  ),
  humedad_media = value_box(
    title = "Humedad media del Suelo",
    showcase = bsicons::bs_icon("moisture"),
    value = textOutput("meanHumedad"),
    theme_color = "secondary"
  ),
  vecinos_abundancia = value_box(
    title = "Abundancia vecinos",
    showcase = bsicons::bs_icon("align-center"),
    value = htmlOutput("mean_vecinos_ab"),
    theme_color = "dark"
  ),
  vecinos_sps = value_box(
    title = "N especies vecinas",
    showcase = icon("pagelines", class = "fa-3x"),
    value = textOutput("mean_vecinos_sp"),
    p(htmlOutput("lu_vecinos_sp")),
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


ui <- page_navbar(
  title = "famExploreR",
  sidebar = sidebar(
      shiny::h4("Estadillo de campo"), 
      fileInput("upload", label = "", 
                accept = c(".ods", ".xlsx"),
                placeholder = "Seleccione el archivo a subir"),
      shiny::br(), 
      shiny::br(),
      shiny::h4("Información espacial"),
      fileInput(inputId = "upload_spat",
                label = "Upload map. Choose shapefile",
                multiple = TRUE,
                accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj'))
      ),
  navset_card_tab(
    nav_panel("Datos generales", cards[["metadatos"]]),
    nav_panel("Localización", cards[["mapa"]]),
    nav_menu("Suelos",
             nav_panel("Humedad",
                       layout_columns(fill = TRUE,
                                      vb[["temp_media"]],
                                      vb[["humedad_media"]]),
                       cards[["humedad"]]),
             nav_panel("Diagrama Ternario", cards[["suelos"]])),
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
             nav_panel("Tabla", cards[["herbivoria_tabla"]])),
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
  )
)


server <- function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    readAllsheets(upload_path = input$upload$datapath, valid_sheets = hojas_validas)
  })
  
  

  output$metadata <- renderTable({
    data()$datos_generales
  })
  
  output$metadataText <- renderUI({
    x <- data()$datos_generales |> pivot_wider(names_from = campo, values_from = valor)
    
    shiny::tagList(
      list(
        shiny::h1(x$`especie focal`),
        shiny::br(),
        shiny::h4(paste0("Localidad: ", x$localidad)),
        shiny::br(),
        shiny::h5(paste0("Fecha: ", format(lubridate::ymd(x$fecha), "%Y-%d-%m"))),
        shiny::p(HTML(paste0("<strong>Excrementos (n/m", tags$sup(2), "):</strong> ", data()$excrementos$excrementos_m2, " (", 
                             data()$excrementos$excrementos_n, " en ", data()$excrementos$superficie_m2, ")")))
      )
    )
    
    
  })
  output$humedad <- renderTable({
    data()$humedad_temp
  })
  
  output$meanTemp <- renderText({
    mean(data()$humedad_temp$temperatura, na.rm=FALSE)
  })
  
  output$meanHumedad <- renderText({
    mean(data()$humedad_temp$humedad, na.rm=FALSE)
  })
  
  
  # Biometry
  generateBiometriaPlot <- function(x){
    
    nombre_variables <- c(
      altura_cm = "Altura", 
      dmayor_cm = "Diámetro mayor",
      dmenor_cm = "Diámetro menor")
    
    biometry <- x$especie_focal |> 
      dplyr::select(especie:id_individuo, altura_cm, dmayor_cm, dmenor_cm) |> 
      pivot_longer(cols = c(altura_cm, dmayor_cm, dmenor_cm)) |> 
      mutate(name = recode(name, !!!nombre_variables))
    
    my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]
    
    ggplot(biometry, aes(x = as.factor(name), y = value, color = name, fill = name)) +
      scale_color_manual(values = my_pal, guide = "none") +
      scale_fill_manual(values = my_pal, guide = "none") + 
      geom_boxplot( width = .2, fill = "white", size = 1.5, outlier.shape = NA) +
      theme(
        axis.text = element_text(size = 24)) + 
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
      ) + coord_flip() +
      theme_minimal(base_size = 20) +
      xlab("") + ylab("")
    
  }
  
  output$biometria <- renderPlot({
    generateBiometriaPlot(data())
  })
  
  
  output$biometria_stats <- renderTable({
    biometryStat(data()$especie_focal) |> 
      mutate(across(c(mean, sd, se), ~ round(.x, digits = 2))) |> 
      formattable()
  })
  
  # Flowering 
  output$plotfloracion <- renderPlotly({
    
    s <- computeFlowering(data()$especie_focal, 
                          var_interest = c("n_flores", "n_frutos"))
    
    g <- plotFlowering(s) 
    ggplotly(g)
  })
  
  
  
  # Plot Soil ternary 
  ternary_data <- reactive({
    data()$suelo |> 
      dplyr::select(limo_g, limo_f, arcilla, arena) |> 
      mutate(limo = sum(c_across(starts_with("limo"))))
  })
  
  output$suelos <- renderPlot({
    print(
      ternaryPlot(ternary_data(), bsize =20,
                  xvar = "arena", yvar = "arcilla",  zvar = "limo") 
    ) # Note that the ggtern need to be plotted in a print environment 
  })
  
  
  # Map 
  initial_map <- reactive({
    req(input$upload)
    coord_data <- st_transform(prepareGeo(data()$datos_generales), 4326)
    custom_popup <- suppressWarnings(preparePopup(data()$datos_generales))
    
    leaflet::leaflet(data = coord_data) |> 
      addProviderEspTiles("IGNBase.Gris", group = "Base") |> 
      addProviderEspTiles("MTN", group = "MTN") |> 
      addProviderEspTiles("LiDAR", group = "LIDAR") |> 
      addProviderEspTiles('MDT.CurvasNivel', group = "Curvas de Nivel") |>
      addProviderTiles('Esri.WorldImagery', group = "Ortofoto") |> 
      leaflet::addMarkers(popup = custom_popup) |> 
      addLayersControl(
        baseGroups = c("Base", "LIDAR", "Curvas de Nivel", "MTN", "Ortofoto"),
        options = layersControlOptions(collapsed = FALSE))
    })

  
  # Define an eventReactive to handle shapefile upload
  updated_map <- eventReactive(input$upload_spat, {
  
      shpdf <- input$upload_spat
      tempdirname <- dirname(shpdf$datapath[1])
      
      # Rename files
      for (i in 1:nrow(shpdf)) {
        file.rename(
          shpdf$datapath[i],
          paste0(tempdirname, "/", shpdf$name[i])
        )
      }
      
      # Read the shapefile
      geo <- sf::st_read(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))
      geo <- sf::st_transform(geo, 4326)
      
      # Add the shapefile to the existing Leaflet map
      m <- initial_map() |> 
        addPolygons(data = geo,
                    fillColor = "blue",  # Customize fill color and other options
                    color = "black",
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 0.2)
    })
  
  
  # Use the shapefileUpload eventReactive result to update the Leaflet map
  output$map <- leaflet::renderLeaflet({
    if (!is.null(updated_map())) {updated_map() } else {initial_map()}
  })
  

  # Vecindad 
  stat_vecinos_sps <- reactive({
    neighborSpecies_stats(data()$vecindad)
  })
  
  generateVecindadPlot <- function(x){
    ggplot(x, aes(x = especie_vecina, y = ab_mean)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_errorbar(aes(ymin = ab_mean - ab_se, 
                        ymax = ab_mean + ab_se),
                    width = 0.25, 
                    position = position_dodge(width = 0.9), 
                    colour = "blue") +
      labs(x = "Especie Vecina",
           y = "Abundancia (n. ind)") +
      theme_minimal() + 
      theme(axis.text.y = element_text(face = "italic")) + 
      coord_flip() +
      theme(
        axis.text = element_text(size = 16), 
        axis.title = element_text(size = 17)
      ) 
    
    
  }
  
  output$vecindad <- renderPlot({
    generateVecindadPlot(stat_vecinos_sps())
  })
  
  output$downloadVecindad <- downloadHandler(
    filename = function() {
      "vecindad_plot.png"  # Set the filename for the downloaded plot
    },
    content = function(file) {
      g <- generateVecindadPlot(stat_vecinos_sps())
      ggsave(file, g)
    }
  )
  
  especie_focal <- reactive({
    data()$datos_generales |> 
      filter(campo == "especie focal") |> 
      dplyr::select(valor) |> 
      pull()
  })
  
  
  
  ### Stats Vecindad
  stats_vecinos_ab_summ <- reactive({
    y <- neighborAbundance_stats(data = data()$vecindad,
                                 units = "dm2", focal_sp = especie_focal())
    y[[2]]
  })
  
  
  # output$mean_vecinos_ab <- renderText({
  #   x <- subset(stats_vecinos_ab_summ(), variable == "n_total_vecinos")
  #   paste0(round(x$avg,2), ' ± ', round(x$se,2))
  #  })
  
  output$mean_vecinos_ab <- renderUI({
    x <- subset(stats_vecinos_ab_summ(), variable == "n_total_vecinos")
    shiny::tagList(
      list(
        shiny::p(paste0(round(x$avg,2), ' ± ', round(x$se,2)), style = "font-size: 70%; text-align: center;"),
        shiny::p(paste0(round(x$min,2), ' - ', round(x$max,2)), style = "font-size: 50%; text-align: center;")
      )
    )
  })
  
  
  
  
  output$mean_vecinos_sp <- renderText({
    x <- subset(stats_vecinos_ab_summ(), variable == "n_sps_vecinas")
    paste0(round(x$avg,2), ' ± ', round(x$se,2))
  })
  
  output$lu_vecinos_sp <- renderUI({
    x <- subset(stats_vecinos_ab_summ(), variable == "n_sps_vecinas")
    shiny::p(paste0(round(x$min,2), ' - ', round(x$max,2)), 
             style = "text-align: center;")
  })
  
  
  ### Comunidad
  
  comunidad <- reactive({
    diversityCommunity(data())
  })
  
  output$richness <- renderText({
    comunidad()$richness
  })
  
  output$diversity_shannon <- renderText({
    round(comunidad()$diversity_shannon, 2)
  })
  
  output$evenness_pielou <- renderText({
    round(comunidad()$evenness_pielou, 3)
  })
  
  output$plotcomunidad <- renderPlotly({
    g <- plotCommunity(data())
    g$data$especie_acomp <- paste0("<i>", g$data$especie_acomp, "</i>")
    ggplotly(g, tooltip = "y")
  })
  
  herbivoria_calculos <- reactive({
    herbivory(data()$herbivoria)
  }) 
  
  # Herbivoria 
  output$plotherbivoria <- renderPlotly({
    ggplotly(herbivoria_calculos()$plot_damage)
  })
  
  output$tablaherbivoria <- renderTable({
    herbivoria_calculos()$damage
  })
  
}

shinyApp(ui, server, ...)
}

