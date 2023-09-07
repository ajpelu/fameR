library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(purrr)
library(rcartocolor)
library(ggdist)
library(gghalves)


source("R/readAllsheets.R")
hojas_validas <- "data/hojas_oficiales.csv" |> read.csv() |> pull()



upload <- fileInput("upload", 
                    label = "Subir la ficha de campo", 
                    accept = c(".ods", ".xlsx"),
                    placeholder = "Seleccione el archivo a subir")

cards <- list(
  card(
    full_screen = TRUE,
    card_header("Datos generales"),
    tableOutput("metadata")
    ), 
  card(
    full_screen = TRUE, 
    card_header("Datos de Humedad y Temperatura del suelo"), 
    tableOutput("humedad")
  ), 
  card(
    full_screen = TRUE, 
    card_header("Biometría"), 
    plotOutput("biometria")
  ), 
  card(
    full_screen = TRUE, 
    card_header("Mapa"), 
    leaflet::leafletOutput("map")
  )
)

ui <- page_navbar(
  title = "Explorador fameR", 
  sidebar = upload,
  nav_panel("Datos generales", cards[[1]]), 
  nav_panel("Localización", cards[[4]]),
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
  nav_panel("Biometria", cards[[3]])
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$upload)
    readAllsheets(upload_path = input$upload$datapath, 
                  valid_sheets = hojas_validas)
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


  output$biometria <- renderPlot({
    
    nombre_variables <- c(
      altura_cm = "Altura", 
      dmayor_cm = "Diámetro mayor",
      dmeno_cm = "Diámetro menor")
    
    biometry <- data()$especie_focal |> 
      dplyr::select(especie:id_individuo, altura_cm, dmayor_cm, dmeno_cm) |> 
      pivot_longer(cols = c(altura_cm, dmayor_cm, dmeno_cm)) |> 
      mutate(name = recode(name, !!!nombre_variables))
    
    my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]
    
    ggplot(biometry, aes(x = as.factor(name), y = value, color = name, fill = name)) +
      scale_color_manual(values = my_pal, guide = "none") +
      scale_fill_manual(values = my_pal, guide = "none") + 
      geom_boxplot(
        width = .2, fill = "white",
        size = 1.5, outlier.shape = NA
      ) +
      theme(
        axis.text = element_text(size = 24)
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
      ) + coord_flip() +
      theme_minimal(base_size = 20) +
      xlab("") + ylab("")
  })
  
  leaflet_map <- reactive({
    
    coord_data <- st_transform(prepareGeo(data()$datos_generales), 4326)
  
    leaflet_map <- leaflet::leaflet(data = coord_data) |> 
      leaflet::addTiles() |> 
      leaflet::addMarkers() 
    })
  
  output$map <- leaflet::renderLeaflet({
    req(leaflet_map())
    map <- leaflet_map()
  })
  

}

shinyApp(ui, server)