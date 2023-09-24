library(shiny)
library(tidyverse)
library(leaflet)
library(fmsb)

# Library
library(fmsb)


library(ggplot2)
library(ggiraph)

# Create example data
data <- data.frame(
  variable = c("Math", "Science", "English", "History"),
  value = c(90, 85, 77, 92)
)

# Add an id for ggiraph
data$id <- seq_len(nrow(data))

# Create the ggplot2 radar chart
p <- ggplot(data, aes(x = variable, y = value, group = 1, tooltip = id)) +
  geom_polygon(fill = "blue", alpha = 0.4) +
  geom_line(color = "darkblue") +
  coord_polar()

# Make it interactive with ggiraph
interactive <- girafe(ggobj = p)

# Print or save the interactive plot
print(interactive)



# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(30,10) , rep(0,10) , data)

# Check your data, it has to look like this!
head(data)

# The default radar chart 
radarchart(data)
# ===============================================================================

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