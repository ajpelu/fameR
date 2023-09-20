
library(shiny)
library(ggplot2)



plot_UI <- function(id, dataset) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          ns('select_var_1'),
          'Variable 1',
          choices = names(dataset)
        ),
        selectInput(
          ns('select_var_2'),
          'Variable 2',
          choices = names(dataset)
        ),
        actionButton(
          ns('draw_scatterplot'),
          'Draw scatterplot',
        )
      ),
      mainPanel(
        plotOutput(ns('scatterplot'))
      )
    )
  )
}

plot_Server <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$scatterplot <- renderPlot({
        ggplot(dataset) +
          geom_point(
            aes_string(input$select_var_1, input$select_var_2), 
            size = 3,
            alpha = 0.5,
            col = 'dodgerblue4'
          )
      }) |> bindEvent(input$draw_scatterplot)
    }
  )
}


ui <- fluidPage(
  theme =  bslib::bs_theme(bootswatch = 'flatly'),
  tabsetPanel(
    tabPanel("Penguins", 
             plot_UI('Penguin', palmerpenguins::penguins)),
    tabPanel("Iris", 
             plot_UI('Iris', iris)), 
    tabPanel("Diamonds", 
             plot_UI('Diamonds', diamonds)), 
    tabPanel("mpg",
             plot_UI('mpg', mpg))
  )
)

server <- function(input, output, session) {
  plot_Server('Penguin', palmerpenguins::penguins)
  plot_Server('Iris', iris)
  plot_Server('Diamonds', diamonds)
  plot_Server('mpg', mpg)
  

  
  
}

shinyApp(ui, server)
