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


source("R/readAllsheets.R")
source("R/prepareGeo.R")
source("R/preparePopup.R")
source("R/ternaryPlot.R")
source("R/neighborSpecies_stats.R")

hojas_validas <- "data/hojas_oficiales.csv" |> read.csv() |> pull()


# Sidebar upload 
upload <- fileInput("upload", 
                    label = "Subir la ficha de campo", 
                    accept = c(".ods", ".xlsx"),
                    placeholder = "Seleccione el archivo a subir")

cards <- list(
  card(full_screen = TRUE, card_header("Datos generales"), 
       tableOutput("metadata")), 
  card(full_screen = TRUE, card_header("Datos de Humedad y Temperatura del suelo"), 
       tableOutput("humedad")), 
  card(full_screen = TRUE, card_header("Biometría"), 
    plotOutput("biometria")), 
  card(full_screen = TRUE, card_header("Mapa"), 
    leaflet::leafletOutput("map")),
  card(full_screen = TRUE, card_header("Suelos"), 
    plotOutput("suelos")), 
  card(full_screen = TRUE, card_header("Vecindad"), 
    plotOutput("vecindad"))
  )

ui <- page_navbar(
  title = "Explorador fameR", 
  sidebar = upload,
  nav_panel("Datos generales", 
            cards[[1]]), 
  nav_panel("Localización", 
            cards[[4]]),
  nav_panel("Humedad", 
            layout_columns(
              fill = FALSE, 
              value_box(
                title = "Temperatura media del Suelo", 
                showcase = bsicons::bs_icon("thermometer"),
                value = textOutput("meanTemp"),
                theme_color = "secondary"
              ),
              value_box(
                title = "Humedad media del Suelo", 
                showcase = bsicons::bs_icon("moisture"),
                value = textOutput("meanHumedad"),
                theme_color = "secondary"
              )),
            cards[[2]]), 
  nav_panel("Suelos", 
            cards[[5]]), 
  nav_panel("Biometria", 
            cards[[3]]), 
  nav_panel("Vecindad", 
            cards[[6]], 
            downloadButton("downloadVecindad", "Download Plot"))
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    readAllsheets(upload_path = input$upload$datapath, valid_sheets = hojas_validas)
  })
  
  output$metadata <- renderTable({
    data()$datos_generales
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
  
  
  
  leaflet_map <- reactive({
    
    coord_data <- st_transform(prepareGeo(data()$datos_generales), 4326)
    custom_popup <- suppressWarnings(preparePopup(data()$datos_generales)) 
  
    leaflet_map <- leaflet::leaflet(data = coord_data) |> 
      addProviderEspTiles("IGNBase.Gris", group = "Base") |> 
      addProviderEspTiles("MTN", group = "MTN") |> 
      addProviderEspTiles("LiDAR", group = "LIDAR") |> 
      addProviderEspTiles('MDT.CurvasNivel', group = "Curvas de Nivel") |>
      addProviderTiles('Esri.WorldImagery', group = "Ortofoto") |> 
      leaflet::addMarkers(
        popup = custom_popup
      ) |> 
      addLayersControl(
        baseGroups = c("Base", "LIDAR", "Curvas de Nivel", "MTN", "Ortofoto"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    })
  
  output$map <- leaflet::renderLeaflet({
    req(leaflet_map())
    map <- leaflet_map()
  })
  
  
  # Vecindad 
  stat_vecinos <- reactive({
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
    generateVecindadPlot(stat_vecinos())
  })
  
  output$downloadVecindad <- downloadHandler(
    filename = function() {
      "vecindad_plot.png"  # Set the filename for the downloaded plot
    },
    content = function(file) {
      g <- generateVecindadPlot(stat_vecinos())
      ggsave(file, g)
    }
  )
  

}

shinyApp(ui, server)