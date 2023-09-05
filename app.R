library(shiny)
library(bslib)
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
    card_header("Humedad"), 
    tableOutput("humedad")
  )
)

ui <- page_navbar(
  title = "Explorador fameR", 
  sidebar = upload,
  nav_panel("Datos generales", cards[[1]]), 
  nav_panel("Humedad", cards[[2]])
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
}

shinyApp(ui, server)