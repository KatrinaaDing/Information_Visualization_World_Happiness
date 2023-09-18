library('shiny')
library('ggplot2')
library('leaflet')
library('ggiraph')
library(dplyr)
library(bslib)

worldHappiness <- read.csv("/Users/katrinading/Desktop/Uni/postgrad/GEOM90007 iv/asst2/data/World Happiness Reports 2013-2023/WorldHappinessIndex2013-2023.csv")
# worldHappiness <- worldHappiness[complete.cases(worldHappiness$Life.Ladder, worldHappiness$Year), ]
# worldHappiness$Year <- as.numeric(as.character(worldHappiness$Year))
# worldHappiness$Rank <- ave(worldHappiness$Life.Ladder, worldHappiness$Year, FUN = function(x) rank(-x))


##################
# USER INTERFACE #
##################


rank_tab <- tabPanel(
  title='Ranks',
  h2('World Happiness Ranks'),
  sidebarLayout(
    sidebarPanel(
      textInput("country_search", "Search for a country:", ""),
      div(style = "height: 400px; overflow-y: scroll;",
          checkboxGroupInput("country_select", "Choose a country:",
            choices = sort(unique(worldHappiness$Country)),
            selected = "Australia"
          ))
    ),
    mainPanel(
      girafeOutput("linePlot")
    )
  )
)

ui <- navbarPage(
  title = "World Happiness Report",
  theme = bslib::bs_theme(bootswatch = "lumen"),
  rank_tab
)

##################
# EVENT OBSERVER #
##################




################
# SHINY SERVER #
################

server <- function(input, output, session) {
  # get input value from "country_search" text input
  # observeEvent(input$country_search, {
  #   filtered_data <- sort(unique(worldHappiness$Country))

  #   if (input$country_search != "") {
  #     filtered_data <- filtered_data[grepl(input$country_search, filtered_data, ignore.case = TRUE)]
  #   }
  #   selected_countries <- intersect(input$country, filtered_data)
  #   updateCheckboxGroupInput(session, 'country', choices = filtered_data, selected = selected_countries)
  # })

  output$linePlot <- renderGirafe({
    if (is.null(input$country_select)) {
      # If no data, display a warning
      return(ggplot() + geom_text(aes(0, 0, label = "No data available for this country")))
    }
    filtered_data <- worldHappiness %>% filter(Country %in% input$country_select)

    p <- ggplot(filtered_data, aes(x=Year, y=Rank, group=Country, color=Country)) +
      theme_minimal() +
      scale_x_continuous(breaks=2013:2023) +
      geom_line_interactive(aes(group=Country)) +
      geom_point_interactive(size = 4, aes(tooltip = paste("Country:", Country, "<br>", "Year:", Year, "<br>", "Rank:", Rank))) +
      scale_y_continuous(limits = c(0, max(filtered_data$Rank))) +
      ggtitle(paste("World Happiness Rank of Countries Over Time")) +
      xlab("Year") +
      ylab("Rank")

    girafe(ggobj=p)

  })

}

# Run the application 
shinyApp(ui = ui, server = server)
