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


source("biometryStat.R")
source("biometryPlot.R")
source("computeFlowering.R")
source("diversityCommunity.R")
source("herbivory.R")
source("neighborAbundance_stats.R")
source("neighborSpecies_stats.R")
source("plotCommunity.R")
source("plotFlowering.R")
source("prepareGeo.R")
source("preparePopup.R")
source("readAllsheets.R")
source("summarizeSoil.R")
source("ternaryPlot.R")
source("vecindadPlot.R")


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
        shiny::h4("Localidad"),
        shiny::h5(paste0("Localidad: ", x$localidad)),
        shiny::h5(paste0("Elevación: ", x$elevacion, " (m.a.s.l)")), 
        shiny::br(), 
        shiny::h5(paste0("Código de Población: ", x$site)), 
        shiny::h5(paste0("Tratamiento (dentro/fuera): ", x$tratamiento)),
        shiny::h5(paste0("Referencia: ", x$reference)),
        shiny::h5(paste0("Fecha: ", format(lubridate::ymd(x$fecha), "%Y-%d-%m"))),
        shiny::br(),
        shiny::h4("Vallado"),
        shiny::h5(paste0("Tipo: ", x$vallado_tipo)),
        shiny::h5(paste0("Año de instalación: ", x$vallado_year)), 
        shiny::h5(paste0("Dimensiones (perímetro): ", x$vallado_perimetro)), 
        shiny::h5(paste0("Estado del vallado): ", x$vallado_perimetro)), 
        shiny::br(),
        shiny::h4("Excrementos"),
        shiny::h5(HTML(paste0("<strong>Densidad excrementos (n/m", tags$sup(2), "):</strong> ", data()$excrementos$excrementos_m2, " (", 
                             data()$excrementos$excrementos_n, " en ", data()$excrementos$superficie_m2, ")"))),
        shiny::br()
      )
    )
    
    
  })
  output$humedad <- renderTable({
    data()$humedad_temp
  })
  
  temp_humedad <- reactive({ 
    data()$humedad_temp |> 
      na.omit() |> 
      dplyr::select(referencia, temperatura, humedad) |> 
      pivot_longer(-referencia) |> 
      group_by(name) |> 
      summarise(mean = mean(value, na.rm = FALSE), 
                sd = sd(value, na.rm = FALSE),
                se = sd/sqrt(length(value)), 
                min = min(value, na.rm = FALSE),
                max = max(value, na.rm = FALSE))
    })
  
  
  
  output$meanTemp <- renderUI({
    x <- subset(temp_humedad(), name == "temperatura")
    shiny::tagList(
      list(
        shiny::h5(paste0(round(x$mean,2), ' ± ', round(x$se,2)), style = "font-size: 70%; text-align: center;"),
        shiny::h5(paste0(round(x$min,2), ' - ', round(x$max,2)), style = "font-size: 60%; text-align: center;")
      )
    )
  })
  
  output$meanHumedad <- renderUI({
    x <- subset(temp_humedad(), name == "humedad")
    shiny::tagList(
      list(
        shiny::h5(paste0(round(x$mean,2), ' ± ', round(x$se,2)), style = "font-size: 70%; text-align: center;"),
        shiny::h5(paste0(round(x$min,2), ' - ', round(x$max,2)), style = "font-size: 60%; text-align: center;")
      )
    )
  })
  
  
  
  
  
  # output$meanTemp <- renderText({
  #   mean(data()$humedad_temp$temperatura, na.rm=FALSE)
  # })
  # 
  # output$meanHumedad <- renderText({
  #   mean(data()$humedad_temp$humedad, na.rm=FALSE)
  # })
  # 
  
  # Biometry
  
  output$biometria <- renderPlot({
    biometryPlot(data(), base_size = 20)
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
      na.omit() |> 
      dplyr::select(limo_g, limo_f, arcilla, arena, referencia_suelo) |> 
      mutate(limo = sum(c_across(starts_with("limo"))))
  })
  
  output$suelos <- renderPlot({
    print(
      ternaryPlot(ternary_data(), bsize = 20, point_size = 5,
                  xvar = "arena", yvar = "arcilla",  zvar = "limo") 
    ) # Note that the ggtern need to be plotted in a print environment 
  })
  
  output$suelos_tabla <- renderTable({
    
    summarizeSoil(data()$suelo) |> 
      dplyr::filter(if_any(everything(), ~ !all(is.na(.)))) |> 
      dplyr::mutate_all(.funs = ~ tidyr::replace_na(as.character(.x), "")) |>
      formattable()
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
    x <- data()$vecindad |> na.omit()
    neighborSpecies_stats(x)
  })
  
  
  output$vecindad <- renderPlot({
    vecindadPlot(stat_vecinos_sps())
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
    ef <- data()$datos_generales |> 
      filter(campo == "especie focal") |> 
      dplyr::select(valor) |> 
      pull()
    
    data()$dicc_taxon |> 
      filter(scientificname == ef) |> 
      dplyr::select(withoutautorship) |> 
      pull()
    
  })
  
  
  
  ### Stats Vecindad
  stats_vecinos_ab_summ <- reactive({
    d <- data()$vecindad |> na.omit()
    
    y <- neighborAbundance_stats(data = d,
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
        shiny::h5(paste0(round(x$avg,2), ' ± ', round(x$se,2)), style = "font-size: 70%; text-align: center;"),
        shiny::h5(paste0(round(x$min,2), ' - ', round(x$max,2)), style = "font-size: 50%; text-align: center;")
      )
    )
  })
  

  output$mean_vecinos_sp <- renderUI({
    x <- subset(stats_vecinos_ab_summ(), variable == "n_sps_vecinas")
    
    shiny::tagList(
      list(
        shiny::h5(paste0(round(x$avg,2), ' ± ', round(x$se,2)), style = "font-size: 70%; text-align: center;"),
        shiny::h5(paste0(round(x$min,2), ' - ', round(x$max,2)), style = "font-size: 50%; text-align: center;")
      )
    )
#     paste0(round(x$avg,2), ' ± ', round(x$se,2))
  })
  
  # output$lu_vecinos_sp <- renderUI({
  #   x <- subset(stats_vecinos_ab_summ(), variable == "n_sps_vecinas")
  #   shiny::p(paste0(round(x$min,2), ' - ', round(x$max,2)), 
  #            style = "text-align: center;")
  # })
  
  
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
  
  output$tablaherbivoria <- renderFormattable({
    herbivoria_calculos()$damage |> 
      dplyr::mutate(across(c(mean_damage, sd_damage, se_damage), ~ round(.x, 2))) |> 
      dplyr::mutate_all(.funs = ~ tidyr::replace_na(as.character(.x), "")) |> 
      rename(
        `Individuo` = id_individuo, 
        `% Hojas Dañadas` = leaf_damages_pct, 
        `% Daño (media)` = mean_damage,
        `% Daño (sd)` = sd_damage,
        `% Daño (se)` = se_damage) |> 
      formattable(
        list(`% Hojas Dañadas` = color_bar("lightgreen"),
             `% Daño (media)` = color_bar("lightblue"))) 
    
  })
  

  output$generateReport <- downloadHandler(
    filename = function() {
      "famexploreR_report.docx"  # Set the filename for the downloadedreport
    },
    content = function(file) {
      # Define parameters to be passed to the Rmd file
      params <- list(data = data())  
      
      rmarkdown::render("famexploreR_report.Rmd", 
                        output_file = file, 
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
}


