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
  happy = makeIcon("icons/face-smile-regular.svg", "icons/face-smile-regular.svg", 18, 18),
  natural = makeIcon("icons/face-meh-regular.svg", "icons/face-meh-regular.svg", 18, 18),
  sad = makeIcon("icons/face-frown-regular.svg", "icons/face-frown-regular.svg", 18, 18)
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

MIN_INDEX <- min(dataWithSpatial$Index, na.rm = TRUE)
MAX_INDEX <- max(dataWithSpatial$Index, na.rm = TRUE)
CLASS <- 3
STEP <- (MAX_INDEX - MIN_INDEX) / CLASS
SAD_THREAHSOLD <- MIN_INDEX + STEP
HAPPY_THRESHOLD <- MAX_INDEX - STEP

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
dataWithSpatial$happiness <- ifelse(dataWithSpatial$Index > HAPPY_THRESHOLD, "happy", ifelse(dataWithSpatial$Index > SAD_THREAHSOLD, "natural", "sad"))



##################
# USER INTERFACE #
##################

map_tab <- tabPanel(
  title = "Map",
  tags$head(
    tags$style(HTML("
      #controls-container{
        position: absolute;
        bottom: 5px;
        left: 50%;
        transform: scale(1.1) translate(-50%, -50%);
      }
      #controls-container .shiny-input-container {
        background-color: rgba(255,255,255,0.5);
        padding: 10px;
        text-align: center;
        margin-bottom: 0;

      }
      #controls-container #timeline-label {
        width: 100%;
        text-align: center;
      }
      #map_happiness {
        height: calc(100vh - 100px) !important;
      }
    ")),
  ),
  leafletOutput("map_happiness", height = "100vh"),
  # control panel
  absolutePanel(
    id = "controls-container",
    sliderInput(
      "timeline",
      "Select Year<br>sdf",
      min = 2013, max = 2023, value = 2023,
      step = 1,
      sep = "",
      animate = TRUE,
    ),
    checkboxInput("clustering", "Enable clustering", value = TRUE)
  ),
  
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
  observeEvent(input$timeline,{
    if (input$timeline == 2014) {
      updateSliderInput(session, "timeline", value = 2015)
    }
  })
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
  
  getFilteredData <- reactive({
    filter(dataWithSpatial, 
      Year == input$timeline & !is.na(dataWithSpatial$Index))
  })
  # observe({
  #   dataWithSpatial <- getFilteredData()
  #   proxy <- leafletProxy("map_happiness")
  #   proxy %>% clearMarkers() 
  #   if (input$clustering) {
  #       # Enable clustering
  #       proxy %>% 
  #         addMarkers(
  #           clusterOptions = markerClusterOptions(),
  #           lng = dataWithSpatial$lng,
  #           lat = dataWithSpatial$lat,
  #           icon = faceIcons[dataWithSpatial$happiness],
  #           options = leaflet::markerOptions(happinessIndex = dataWithSpatial$Index), # Setting happinessIndex
  #           popup = paste("Country:", dataWithSpatial$name, "<br>", 
  #                     "Happiness Score:", dataWithSpatial$Index , "<br>",
  #                     "Rank:", dataWithSpatial$Rank),
  #           label = paste(dataWithSpatial$name),
  #           labelOptions = labelOptions(direction = "top")
  #         ) 
  #   } else {
  #     proxy %>%
  #       addMarkers(
  #         lng = dataWithSpatial$lng,
  #         lat = dataWithSpatial$lat,
  #         icon = faceIcons[dataWithSpatial$happiness],
  #         options = leaflet::markerOptions(happinessIndex = dataWithSpatial$Index), # Setting happinessIndex
  #         popup = paste("Country:", dataWithSpatial$name, "<br>", 
  #                   "Happiness Score:", dataWithSpatial$Index , "<br>",
  #                   "Rank:", dataWithSpatial$Rank),
  #         label = paste(dataWithSpatial$name),
  #         labelOptions = labelOptions(direction = "top")
  #       ) 
  #   }
  # })
  output$map_happiness <- renderLeaflet({
    dataWithSpatial <- getFilteredData()
    nrow <- dataWithSpatial %>% nrow()
    if (nrow == 0) {
      return(NULL)  # or return a leaflet() object with a message
    }
    colors <- colorNumeric("Blues", dataWithSpatial$Index)
    leaflet_map <- leaflet(dataWithSpatial) %>%
      addProviderTiles(providers$CartoDB) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 190, lat2 = 90) %>%
      addPolygons(
        fillColor = 'lightblue',#~colors(Index),
        color = 'white',
        smoothFactor = 0.2,
        fillOpacity = 0.7,
        stroke = F,
        weight = 2,
        # popup = ~paste("Country:", dataWithSpatial$name, "<br>", "Happiness Score:", dataWithSpatial$Index)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colors,
        values = ~dataWithSpatial$Index,
        bins = 14,
        title = "Index of Happiness"
      ) %>%addMarkers(
        # clusterOptions = markerClusterOptions(),
        lng = dataWithSpatial$lng,
        lat = dataWithSpatial$lat,
        icon = ~faceIcons[happiness],
        options = leaflet::markerOptions(happinessIndex = dataWithSpatial$Index), # Setting happinessIndex
        popup = ~paste("Country:", dataWithSpatial$name, "<br>", 
                  "Happiness Score:", dataWithSpatial$Index , "<br>",
                  "Rank:", dataWithSpatial$Rank),
        label = ~paste(dataWithSpatial$name),
        labelOptions = labelOptions(direction = "top")
      ) 
      leaflet_map %>% onRender("
        function(el, x) {
          // icons
          let happyIcon = L.icon({
              iconUrl: 'www/icons/face-smile-regular.svg',
              iconSize: [24, 24]
          });
          console.log(happyIcon)
          let naturalIcon = L.icon({
              iconUrl: 'www/icons/face-meh-regular.svg',
              iconSize: [24, 24]
          });
          let sadIcon = L.icon({
              iconUrl: 'www/icons/face-frown-regular.svg',
              iconSize: [24, 24]
          });
          const getAvgHappiness = (markers) => 
            (markers.reduce((a, b) => a + parseFloat(b.options.happinessIndex), 0) / markers.length).toFixed(2)
          let map = this;
          map.eachLayer(function(layer) {
            if (layer instanceof L.MarkerClusterGroup) {
              // create cluster icon
              layer.options.iconCreateFunction = function(cluster) {
                const averageHappiness = getAvgHappiness(cluster.getAllChildMarkers());

                // cluster icon background style
                iconHtml = '<div style=\"background: radial-gradient(circle at center, lightskyblue, transparent); width: 40px; height: 40px; border-radius: 50%;\"></div>';
                // icon style
                iconStyle = 'style=\"width:24px; height: 24px; position: relative; top: -32px; left: 8px;\"';
                if (averageHappiness > 5.8476) {
                  iconHtml += '<img src=\"icons/face-smile-regular.svg\" '+ iconStyle + ' />';
                } else if (averageHappiness > 3.8533) {
                  iconHtml += '<img src=\"icons/face-meh-regular.svg\" '+ iconStyle + ' />';
                } else {
                  iconHtml += '<img src=\"icons/face-frown-regular.svg\" '+ iconStyle + ' />';
                }
                // cluster label (num of childern markers)
                iconHtml += '<div style=\"position: relative; top: -35px; font-size: 12px; text-align: center; font-weight: 700;\">' + cluster.getAllChildMarkers().length + '</div>';

                return L.divIcon({ html: iconHtml, className: 'my-cluster-icon', iconSize: L.point(40, 40) });
              };
              // create hover popup
              layer.on('clustermouseover', function(a) {
                let cluster = a.layer;
                const averageHappiness = getAvgHappiness(cluster.getAllChildMarkers());
                let popup = L.popup()
                    .setLatLng(cluster.getLatLng())
                    .setContent(`Average Happiness: ${averageHappiness} for ${cluster.getChildCount()} countries`)
                    .openOn(map);
              });
              layer.on('clustermouseout', function(a) {
                map.closePopup();
              });
            }
          });
          map.on('zoomend', function(layer) {
            var zoomLevel = map.getZoom();
            if (layer instanceof L.MarkerClusterGroup) {
              if (zoomLevel > 2.5) {
                // Enable clustering
                layer.addTo(map); // Assuming `layer` is your L.MarkerClusterGroup
              } else {
                // Disable clustering
                map.removeLayer(layer);
              }
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
