library(shiny)
library(tidyverse)
library(leaflet)

fake_data <- read.csv("https://raw.githubusercontent.com/gabrielburcea/stackoverflow_fake_data/master/gather_divided.csv")

ui <- fluidPage(
  
  # Application title
  h1("Symptoms accross the world"),
  
  # Inputs for country and symptom 
  selectInput("country", "Select Country", c("Bangladesh", "India", "Nigeria", "Pakistan", "United Kingdom"), multiple = TRUE), 
  selectInput("symptom", "Symptom", c("Chills", "Cough", "Muscle Ache"), multiple = TRUE),
  
  # Output with map
  h2("Map"),
  leafletOutput("map")
  
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    fake_data %>%
      filter(Country %in% input$country,
             Symptom %in% input$symptom)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = filtered_data())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)