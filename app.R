library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(purrr)

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
  )
)

ui <- page_navbar(
  title = "Explorador fameR", 
  sidebar = upload,
  nav_panel("Datos generales", cards[[1]]), 
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
            cards[[2]])
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
  
  output$meanTemp <- renderText(
    mean(data()$humedad_temp$temperatura, na.rm=FALSE)
  )
  
  output$meanHumedad <- renderText(
    mean(data()$humedad_temp$humedad, na.rm=FALSE)
  )
}

shinyApp(ui, server)