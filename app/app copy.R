library("shiny")
library("ggplot2")
library("leaflet")
library("ggiraph")
library("maptools")
library("rgdal")
library("dplyr")
library("bslib")
library("maps")
library("rnaturalearth")
library("sf")
library("htmlwidgets")
library("shinydashboard")
library("shinyWidgets")

#########
#  DATA #
#########

# worldMap <- ne_countries(scale = "medium", returnclass = "sf")
# worldMap <- st_transform(worldMap, 4326)

### Import data
worldMap <- st_read("../data/countries.geo.json")
worldHappiness <- read.csv("../data/World Happiness Reports 2013-2023/WorldHappinessIndex2013-2023.csv")

### Filter data
worldHappiness <- worldHappiness %>% filter(!is.na(worldHappiness$Index))


###### Map data
# get country centroid coordinates
dataWithSpatial <- left_join(worldMap, worldHappiness, by = c("name" = "Country"))
dataWithSpatial$centroid <- st_centroid(dataWithSpatial$geometry)
dataWithSpatial$lng <- st_coordinates(dataWithSpatial$centroid)[, "X"]
dataWithSpatial$lat <- st_coordinates(dataWithSpatial$centroid)[, "Y"]
N_COUNTRIES <- length(unique(worldHappiness$Country))

# calculate happiness class
MIN_INDEX <- min(dataWithSpatial$Index, na.rm = TRUE)
MAX_INDEX <- max(dataWithSpatial$Index, na.rm = TRUE)
CLASS <- 5
STEP <- (MAX_INDEX - MIN_INDEX) / CLASS
SAD_THRESHOLD <- MIN_INDEX + STEP
SAD_THRESHOLD_2 <- MIN_INDEX + 2 * STEP
NEUTRAL_THRESHOLD <- MIN_INDEX + 3 * STEP
HAPPY_THRESHOLD <- MIN_INDEX + 4 * STEP

#########
# ICONS #
#########

ICON_SIZE <- 18
faceIcons <- iconList(
  very_happy = makeIcon("icons/face-grin-beam-solid.svg", "icons/face-grin-beam-solid.svg", ICON_SIZE, ICON_SIZE),
  happy = makeIcon("icons/face-smile-solid.svg", "icons/face-smile-solid.svg", ICON_SIZE, ICON_SIZE),
  natural = makeIcon("icons/face-meh-solid.svg", "icons/face-meh-solid.svg", ICON_SIZE, ICON_SIZE),
  sad = makeIcon("icons/face-frown-solid.svg", "icons/face-frown-solid.svg", ICON_SIZE, ICON_SIZE),
  very_sad = makeIcon("icons/face-sad-tear-solid.svg", "icons/face-sad-tear-solid.svg", ICON_SIZE, ICON_SIZE)
)
# add icon type
dataWithSpatial$happiness <- cut(
  dataWithSpatial$Index,
  breaks = c(MIN_INDEX, SAD_THRESHOLD, SAD_THRESHOLD_2, NEUTRAL_THRESHOLD, HAPPY_THRESHOLD, MAX_INDEX),
  labels = c("very_sad", "sad", "natural", "happy", "very_happy"),
  include.lowest = TRUE
)
##################
# USER INTERFACE #
##################

home_tab <- tabPanel(
  title = "Home",
  mainPanel(
    style = "padding-left: 20px; padding-right: 20px",
    width = 12,
    h2("Introduction"),
    br(),
    HTML("<p><a href=\"https://www.google.com\">The World Happiness Report</a> is a survey that
    evaluates global happiness levels.
    It's gaining more attention as governments and organizations increasingly use its happiness
    metrics for policy decisions. Experts from various fields like economics, psychology, and public
    policy contribute to the report, explaining how well-being measures can effectively gauge a
    nation's progress. The report reviews current happiness states globally and delves into the
    science behind happiness variations.</p>"),
    HTML("<p>This project aims to explore the World Happiness Report data from <strong>2013 to 2023</strong>
    (except for 2014 since no report released in this year) with <strong>167</strong> countries. Gathered
    data will be visualized in a line chart and a map. The line chart will show the happiness rank
    of selected countries over time. The map will focus on the happiness score of each country in
    the world. </p>"),
    br(),
    fluidRow(
      actionButton("start_explore", "Start Explore", class = "btn btn-primary btn-block")
    )
  )
)

rank_tab <- tabPanel(
  title = "Trends",
  h2("World Happiness Ranks"),
  p("Compare the happiness ranks of countries over time."),
  sidebarLayout(
    sidebarPanel(
      # textInput("country_search", "Search for a country:", ""),
      actionButton("clear_all", "Clear All"),
      div(
        style = "height: 500px; overflow-y: scroll;",
        checkboxGroupInput(
          "country_select",
          HTML("<h5>Choose a country:</h5>"),
          choices = sort(unique(worldHappiness$Country)),
          selected = "Australia"
        )
      ),
    ),
    mainPanel(
      girafeOutput("linePlot", height = "600px")
    )
  )
)

map_tab <- tabPanel(
  title = "Map",
  tags$head(
    tags$style(HTML("
      #controls-container {
        position: absolute;
        bottom: 30px;
        right: 40px;
        width: 300px;
        padding: 10px;
        height: calc(100vh - 250px);
        background:rgba(255, 255, 255, 0.9);
        border-radius: 5px;
        text-align: left;
        box-shadow: 0 0 10px rgba(0,0,0,0.3);
      }
      #controls-container .shiny-input-container {
        padding-top: 10px;
        margin-bottom: 0;
        text-align: left;
      }
      #controls-container #timeline-label {
        text-align: left;
      }
      #world_average_index {
        text-align: center;
      }
      #world_average_index_change {
        text-align: center;
      }
    ")),
  ),
  fluidRow(
    h2("World Happiness Score"),
    valueBoxOutput("world_average_index", width = 6),
    valueBoxOutput("world_average_index_change", width = 6)
  ),
  leafletOutput("map_happiness", height = "calc(100vh - 220px)"),
  # control panel
  absolutePanel(
    draggable = F,
    id = "controls-container",
    sliderInput(
      "timeline",
      HTML("<span style='font-size: 16px;'>Select Year</span>
        <span style='font-size: 12px;'>(No data available for 2014)</span>"),
      min = 2013, max = 2023, value = 2013,
      step = 1,
      sep = "",
      animate = animationOptions(interval = 2000)
    ),
    checkboxInput("clustering", "Enable clustering", value = TRUE),
    checkboxInput("country_name", "Hide country name", value = FALSE),
    pickerInput(
      "happiness_select", "Select Happiness Level:",
      choices = c("very_happy", "happy", "natural", "sad", "very_sad"),
      selected = c("very_happy", "happy", "natural", "sad", "very_sad"),
      multiple = TRUE
    ),
    br(),
    girafeOutput("plot_happiness", height = '400px')
  ),
)


ui <- navbarPage(
  id = "navbar",
  title = "World Happiness Report (2013-2023)",
  theme = bslib::bs_theme(bootswatch = "lumen"),
  home_tab,
  rank_tab,
  map_tab
)

################
# SHINY SERVER #
################

server <- function(input, output, session) {
  # filter selected-year data based on input$timeline
  getFilteredData <- reactive({
    filter(
      dataWithSpatial,
      Year == input$timeline &
        happiness %in% input$happiness_select &
        !is.na(dataWithSpatial$Index)
    )
  })
  # get previous year of the selected year
  getPrevYear <- reactive({
    # 2014 has no data so the previous year of 2015 is 2013
    prev_year <- ifelse(input$timeline == 2015, 2013, input$timeline - 1)
    prev_year
  })
  # filter previous-year data based on input$timeline
  getFilteredDataPrevYear <- reactive({
    filter(
      dataWithSpatial,
      Year == getPrevYear() & !is.na(dataWithSpatial$Index)
    )
  })

  # get input value from "country_search" text input
  # observeEvent(input$country_search, {
  #   filtered_data <- sort(unique(worldHappiness$Country))

  #   if (input$country_search != "") {
  #     filtered_data <- filtered_data[grepl(input$country_search, filtered_data, ignore.case = TRUE)]
  #   }
  #   selected_countries <- intersect(input$country, filtered_data)
  #   updateCheckboxGroupInput(session, 'country', choices = filtered_data, selected = selected_countries)
  # })

  # since 2014 has no data, if the user selects 2014, we will update the slider to 2015
  observeEvent(input$timeline, {
    if (input$timeline == 2014) {
      updateSliderInput(session, "timeline", value = 2015)
    }
  })

  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "country_select", selected = character(0))
  })

  observeEvent(input$start_explore, {
    updateNavbarPage(session, "navbar", "Trends")
  })

 

  

  output$linePlot <- renderGirafe({
    if (is.null(input$country_select)) {
      p <- ggplot() +
        geom_text(aes(x = 0, y = 0, label = "No country selected. Please select a country."), size = 6) +
        theme_void()
      return(girafe(ggobj = p))
    }
    filtered_data <- worldHappiness %>% filter(Country %in% input$country_select)
    worldRankMedium <- worldHappiness %>%
      group_by(Year) %>%
      summarise(Med_rank = median(Rank, na.rm = TRUE))
    y_max <- ifelse(max(worldRankMedium$Med_rank) > max(filtered_data$Rank), max(worldRankMedium$Med_rank) + 2, max(filtered_data$Rank, na.rm = TRUE))
    p <- ggplot() +
      geom_line_interactive(data = filtered_data, aes(x = Year, y = Rank, group = Country, color = Country)) +
      geom_point_interactive(data = filtered_data, aes(x = Year, y = Rank, color = Country, tooltip = paste("Country:", Country, "<br>", "Year:", Year, "<br>", "Rank:", Rank)), size = 4) +
      geom_line_interactive(data = worldRankMedium, aes(x = Year, y = Med_rank, color = "World Medium"), size = 1) +
      geom_point_interactive(data = worldRankMedium, aes(x = Year, y = Med_rank, color = "World Medium", tooltip = paste("World Medium Rank in ", Year, ":", Med_rank)), size = 4) +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks = 2013:2023) +
      scale_y_continuous(breaks = seq(0, y_max, by = 10), limits = c(0, y_max)) +
      scale_color_manual(values = c("World Medium" = "gray", setNames(rainbow(length(unique(filtered_data$Country))), unique(filtered_data$Country)))) +
      ggtitle("World Happiness Rank of Countries Over Time") +
      xlab("Year") +
      ylab("Rank")
    girafe(ggobj = p)
  })

  output$map_happiness <- renderLeaflet({
    dataWithSpatial <- getFilteredData()
    nrow <- dataWithSpatial %>% nrow()
    if (nrow == 0) {
      return(NULL)
    }
    # colors <- colorNumeric("Blues", dataWithSpatial$Index)
    leaflet_map <- leaflet(dataWithSpatial) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 80, lat = 0, zoom = 2) %>%
      # setMaxBounds(lng1 = -160, lat1 = -90, lng2 = 300, lat2 = 90) %>%
      # addPolygons(
      #   fillColor = "lightblue", # ~colors(Index),
      #   color = "white",
      #   smoothFactor = 0.2,
      #   fillOpacity = 0.7,
      #   stroke = F,
      #   weight = 2,
      # ) %>%
      addMarkers(
        clusterOptions = NULL, # markerClusterOptions(maxClusterRadius = 30),
        lng = dataWithSpatial$lng,
        lat = dataWithSpatial$lat,
        icon = ~ faceIcons[happiness],
        options = leaflet::markerOptions(happinessIndex = dataWithSpatial$Index),
        popup = ~ paste(
          "Country: <strong>", dataWithSpatial$name, "</strong><br>",
          "Happiness Score:  <strong>", dataWithSpatial$Index, "</strong><br>",
          "Rank:  <strong>", dataWithSpatial$Rank, "</strong>"
        ),
        label = ~ paste(dataWithSpatial$name),
        labelOptions = labelOptions(direction = "top")
      ) %>%
      addControl(
        html = paste0(
          '<div style="padding: 10px; background-color: white;">
              <h5>Happiness Level</h5>
              <div style="padding: 5px;"><img src="icons/face-grin-beam-solid.svg" width="20" height="20"> <strong>Very High</strong> (',
          format(round(HAPPY_THRESHOLD, 3), nsmall = 3), "-", format(round(MAX_INDEX, 3), nsmall = 3), ')</div>
              <div style="padding: 5px;"><img src="icons/face-smile-solid.svg" width="20" height="20"> <strong>High</strong> (',
          format(round(NEUTRAL_THRESHOLD, 3), nsmall = 3), "-", format(round(HAPPY_THRESHOLD, 3), nsmall = 3), ')</div>
              <div style="padding: 5px;"><img src="icons/face-meh-solid.svg" width="20" height="20"> <strong>Medium</strong> (',
          format(round(SAD_THRESHOLD, 3), nsmall = 3), "-", format(round(NEUTRAL_THRESHOLD, 3), nsmall = 3), ')</div>
              <div style="padding: 5px;"><img src="icons/face-frown-solid.svg" width="20" height="20"> <strong>Low</strong> (',
          format(round(SAD_THRESHOLD_2, 3), nsmall = 3), "-", format(round(SAD_THRESHOLD, 3), nsmall = 3), ')</div>
              <div style="padding: 5px;"><img src="icons/face-sad-tear-solid.svg" width="20" height="20"> <strong>Very Low</strong> (',
          format(round(MIN_INDEX, 3), nsmall = 3), "-", format(round(SAD_THRESHOLD_2, 3), nsmall = 3), ")</div>
            </div>"
        ),
        position = "bottomleft"
      )

    # render custom clustered icons
    leaflet_map %>% onRender("
        function(el, x) {
          // icons
          let happyIcon = L.icon({
              iconUrl: 'www/icons/face-smile-solid.svg',
              iconSize: [24, 24]
          });
          console.log(happyIcon)
          let naturalIcon = L.icon({
              iconUrl: 'www/icons/face-meh-solid.svg',
              iconSize: [24, 24]
          });
          let sadIcon = L.icon({
              iconUrl: 'www/icons/face-frown-solid.svg',
              iconSize: [24, 24]
          });
          const getAvgHappiness = (markers) =>
            (markers.reduce((a, b) => a + parseFloat(b.options.happinessIndex), 0) / markers.length).toFixed(3)
          let map = this;
          map.eachLayer(function(layer) {
            if (layer instanceof L.MarkerClusterGroup) {
              // create cluster icon
              layer.options.iconCreateFunction = function(cluster) {
                const averageHappiness = getAvgHappiness(cluster.getAllChildMarkers());

                // cluster icon background style
                iconHtml = '<div style=\"background: radial-gradient(circle at center, transparent, transparent); width: 40px; height: 40px; border-radius: 50%;\"></div>';
                // icon style
                iconStyle = 'style=\"width: 24px; height: 24px; position: relative; top: -32px; left: 8px;\"';
                if (averageHappiness > 5.8476) {
                  iconHtml += '<img src=\"icons/face-smile-solid.svg\" '+ iconStyle + ' />';
                } else if (averageHappiness > 3.8533) {
                  iconHtml += '<img src=\"icons/face-meh-solid.svg\" '+ iconStyle + ' />';
                } else {
                  iconHtml += '<img src=\"icons/face-frown-solid.svg\" '+ iconStyle + ' />';
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
                    .setContent(`${cluster.getChildCount()} countries have average Happiness score of ${averageHappiness}`)
                    .openOn(map);
              });
              layer.on('clustermouseout', function(a) {
                map.closePopup();
              });
            }
          });
        }
      ")
  })
  observeEvent(input$country_name, {
    if (input$country_name) {
      leafletProxy("map_happiness") %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels)
    } else {
      leafletProxy("map_happiness") %>%
        addProviderTiles(providers$CartoDB.Positron)
    }
  })
  # observe({
  #   dataWithSpatial <- getFilteredData()
  #   proxy <- leafletProxy("map_happiness")
  #   proxy %>%
  #     clearMarkerClusters() %>%
  #     clearMarkers()
  #   if (input$clustering) {
  #     # Enable clustering
  #     proxy %>%
  #       addMarkers(
  #         clusterOptions = markerClusterOptions(maxClusterRadius = 30),
  #         lng = dataWithSpatial$lng,
  #         lat = dataWithSpatial$lat,
  #         icon = faceIcons[dataWithSpatial$happiness],
  #         options = leaflet::markerOptions(happinessIndex = dataWithSpatial$Index),
  #         popup = paste(
  #           "Country:", dataWithSpatial$name, "<br>",
  #           "Happiness Score:", dataWithSpatial$Index, "<br>",
  #           "Rank:", dataWithSpatial$Rank
  #         ),
  #         label = paste(dataWithSpatial$name),
  #         labelOptions = labelOptions(direction = "top")
  #       )
  #   } else {
  #     proxy %>%
  #       addMarkers(
  #         lng = dataWithSpatial$lng,
  #         lat = dataWithSpatial$lat,
  #         icon = faceIcons[dataWithSpatial$happiness],
  #         options = leaflet::markerOptions(happinessIndex = dataWithSpatial$Index),
  #         popup = paste(
  #           "Country:", dataWithSpatial$name, "<br>",
  #           "Happiness Score:", dataWithSpatial$Index, "<br>",
  #           "Rank:", dataWithSpatial$Rank
  #         ),
  #         label = paste(dataWithSpatial$name),
  #         labelOptions = labelOptions(direction = "top")
  #       )
  #   }
  # })

  output$world_average_index <- renderValueBox({
    average_happiness <- mean(getFilteredData()$Index, na.rm = TRUE)
    valueBox(
      ifelse(input$timeline == 2014, "Data Not Available", format(round(average_happiness, 3), nsmall = 3)),
      paste("World Average Happiness Score in ", input$timeline)
    )
  })

  output$world_average_index_change <- renderValueBox({
    average_happiness <- mean(getFilteredData()$Index, na.rm = TRUE)
    avaerage_happiness_last_year <- mean(getFilteredDataPrevYear()$Index, na.rm = TRUE)
    change <- ((average_happiness - avaerage_happiness_last_year) / avaerage_happiness_last_year) * 100
    sign <- ifelse(change > 0, "+", "")
    formated_change <- paste(sign, format(round(change, 3), nsmall = 3), "%")
    valueBox(
      ifelse(input$timeline == 2013, "Data Not Available", formated_change),
      paste("Change compare to", getPrevYear()),
    )
  })

  output$plot_happiness <- renderGirafe({
    data <- getFilteredData()

    # Count the number of unique countries for each happiness level
    count_data <- data %>%
      group_by(happiness = data$happiness) %>%
      summarise(n = n_distinct(name, na.rm = TRUE))
    
    p <- ggplot(count_data, aes(x = happiness, y = n)) +
      geom_bar_interactive(stat = "identity", width = 0.8, fill = "#8f00b6") +
      labs(x = "Happiness Level", y = "Number of Countries") +
      theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "#e2e2e2"),
        axis.ticks = element_blank()
      ) +
      ggtitle("Number of Countries by Happiness Level Each Year")

    girafe(ggobj = p)
  })
}

# Run the application
shinyApp(
  ui = ui, server = server
  # options = list(
  #   width = 1920,
  #   height = 1080
  # )
)
