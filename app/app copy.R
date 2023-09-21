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
library(htmlwidgets)


#########
# ICONS #
#########

faceIcons <- iconList(
  happy = makeIcon("icons/face-smile-regular.svg", "icons/face-smile-regular.svg", 24, 24),
  natural = makeIcon("icons/face-meh-regular.svg", "icons/face-meh-regular.svg", 24, 24),
  sad = makeIcon("icons/face-frown-regular.svg", "icons/face-frown-regular.svg", 24, 24)
)

#########
#  DATA #
#########


# country_coords <- world.cities %>%
#   group_by(country.etc) %>%
#   summarise(longitude = mean(long), latitude = mean(lat))


#worldMap <- ne_countries(scale = "medium", returnclass = "sf")
#worldMap <- st_transform(worldMap, 4326)

### Import data
worldMap <- st_read("../data/countries.geo.json")
worldHappiness <- read.csv("../data/World Happiness Reports 2013-2023/WorldHappinessIndex2013-2023.csv")

### Filter data
worldHappiness <- worldHappiness %>% filter(!is.na(worldHappiness$Index))

# worldHappiness <- worldHappiness[complete.cases(worldHappiness$Life.Ladder, worldHappiness$Year), ]
# worldHappiness$Year <- as.numeric(as.character(worldHappiness$Year))
# worldHappiness$Rank <- ave(worldHappiness$Life.Ladder, worldHappiness$Year, FUN = function(x) rank(-x))

###### Map data
# get country centroid coordinates
dataWithSpatial <- left_join(worldMap, worldHappiness, by = c("name" = "Country"))
dataWithSpatial$centroid <- st_centroid(dataWithSpatial$geometry)
dataWithSpatial$lng <- st_coordinates(dataWithSpatial$centroid)[, "X"]
dataWithSpatial$lat <- st_coordinates(dataWithSpatial$centroid)[, "Y"]

##### Icon data
# add icon type
dataWithSpatial$happiness <- ifelse(dataWithSpatial$Index > 5, "happy", ifelse(dataWithSpatial$Index > 3, "natural", "sad"))



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
      leafletOutput("map_happiness", height = 600)
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
    dataWithSpatial <- dataWithSpatial %>% filter(Year == input$timeline & !is.na(dataWithSpatial$Index))
    colors <- colorNumeric("Blues", dataWithSpatial$Index)
    leaflet_map <- leaflet(dataWithSpatial) %>%
      addProviderTiles(providers$CartoDB) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),
        lng = dataWithSpatial$lng,
        lat = dataWithSpatial$lat,
        icon = ~faceIcons[happiness],
        options = leaflet::markerOptions(happinessIndex = dataWithSpatial$Index), # Setting happinessIndex
        
        popup = ~paste("Country:", dataWithSpatial$name, "<br>", "Happiness Score:", dataWithSpatial$Index)
    
      ) %>%
      addPolygons(
                  fillColor = ~colors(Index),
                  color = 'white',
                  smoothFactor = 0.2,
                  fillOpacity = 0.7,
                  stroke = F,
                  weight = 2,
                  #popup = ~paste("Country:", dataWithSpatial$name, "<br>", "Happiness Score:", dataWithSpatial$Index)
                  ) %>%
      addLegend(
        position = "bottomright",
        pal = colors,
        values = ~dataWithSpatial$Index,
        bins = 14,

        title = "Index of Happiness"
      )
    leaflet_map %>% onRender("
      function(el, x) {
        let happyIcon = L.icon({
            iconUrl: 'icons/face-smile-regular.svg',
            iconSize: [24, 24]
        });
        console.log(happyIcon)
        let naturalIcon = L.icon({
            iconUrl: 'icons/face-meh-regular.svg',
            iconSize: [24, 24]
        });

        let sadIcon = L.icon({
            iconUrl: 'icons/face-frown-regular.svg',
            iconSize: [24, 24]
        });

        let map = this;
        map.eachLayer(function(layer) {
          if (layer instanceof L.MarkerClusterGroup) {
            layer.options.iconCreateFunction = function(cluster) {
              let markers = cluster.getAllChildMarkers();
              let averageHappiness = 0;
              for (let i = 0; i < markers.length; i++) {
                averageHappiness += parseFloat(markers[i].options.happinessIndex);
              }
              averageHappiness = (averageHappiness / markers.length).toFixed(2);
              if (averageHappiness > 5) {
                return happyIcon;
              } else if (averageHappiness > 3) {
                return naturalIcon;
              } else {
                return sadIcon;
              }
            };
          }
        });
      }
    ")
  })
}

# Run the application
shinyApp(ui = ui, server = server
# options = list(
#   width = 1920,
#   height = 1080
# )
)
