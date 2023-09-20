library("shiny")
library("ggplot2")
library("leaflet")
library("ggiraph")
library(maptools)
library(rgdal)
library(dplyr)
library(bslib)
library(maps)
library(rnaturalearth)
library(sf)

# worldMap <- readOGR("/private/var/folders/rb/k_cp4hm14m30kkkpg55d22g00000gn/T/MicrosoftEdgeDownloads/3209d76e-b490-4d19-a1e6-b117132665b7/ref-countries-2020-60m.shp/CNTR_BN_60M_2020_3035.shp", "CNTR_BN_60M_2020_3035")
worldMap <- ne_countries(scale = "medium", returnclass = "sf")
worldHappiness <- read.csv("../data/World Happiness Reports 2013-2023/WorldHappinessIndex2013-2023.csv")

# worldHappiness <- worldHappiness[complete.cases(worldHappiness$Life.Ladder, worldHappiness$Year), ]
# worldHappiness$Year <- as.numeric(as.character(worldHappiness$Year))
# worldHappiness$Rank <- ave(worldHappiness$Life.Ladder, worldHappiness$Year, FUN = function(x) rank(-x))
worldMap <- st_transform(worldMap, 4326)


colors <- colorNumeric("Blue", na.color = NA, worldHappiness$Index)

##################
# USER INTERFACE #
##################

map_tab <- tabPanel(
  title = "Map",
  h2("World Happiness Map"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "timeline",
        "Select Year",
        min = 2013,
        max = 2023,
        value = 2023,
        step = 1,
        sep = ""
      )
    ),
    mainPanel(
      girafeOutput("map_happiness")
    )
  )
)

rank_tab <- tabPanel(
  title = "Ranks",
  h2("World Happiness Ranks"),
  sidebarLayout(
    sidebarPanel(
      # textInput("country_search", "Search for a country:", ""),
      div(
        style = "height: 400px; overflow-y: scroll;",
        checkboxGroupInput("country_select", "Choose a country:",
          choices = sort(unique(worldHappiness$Country)),
          selected = "Australia"
        )
      )
    ),
    mainPanel(
      girafeOutput("linePlot")
    )
  )
)

ui <- navbarPage(
  title = "World Happiness Report",
  theme = bslib::bs_theme(bootswatch = "lumen"),
  rank_tab,
  map_tab
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

    p <- ggplot(filtered_data, aes(x = Year, y = Rank, group = Country, color = Country)) +
      theme_minimal() +
      scale_x_continuous(breaks = 2013:2023) +
      geom_line_interactive(aes(group = Country)) +
      geom_point_interactive(size = 4, aes(tooltip = paste("Country:", Country, "<br>", "Year:", Year, "<br>", "Rank:", Rank))) +
      scale_y_continuous(limits = c(0, max(filtered_data$Rank))) +
      ggtitle(paste("World Happiness Rank of Countries Over Time")) +
      xlab("Year") +
      ylab("Rank")

    girafe(ggobj = p)
  })

  output$map_happiness <- renderLeaflet({
    joined_data <- left_join(worldMap, worldHappiness, by = c("name_long" = "Country"))
    joined_data <- joined_data %>% filter(Year==2023)
    leaflet(joined_data) %>%
      addTiles() %>%
      #addProviderTiles(providers$CartoDB) %>%
      # setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolygons(fillColor = ~colors(joined_data$Index),
                  fillOpacity = 0.7,
                  stroke = FALSE,
                  weight = 1,
                  popup = ~paste("Country:", joined_data$Country, "<br>", "Happiness Score:", joined_data$Index))

  })
}

# Run the application
shinyApp(ui = ui, server = server
# options = list(
#   width = 1920,
#   height = 1080
# )
)
