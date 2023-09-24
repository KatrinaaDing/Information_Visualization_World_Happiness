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
library(fmsb)

#########
#  DATA #
#########

worldMap <- ne_countries(scale = "medium", returnclass = "sf")
worldMap <- st_make_valid(worldMap, 4326)

### Import data
# worldMap <- st_read("../data/countries.geo.json")
worldHappiness <- read.csv("../data/World Happiness Report 2005-Present.csv")

### Filter data
worldHappiness <- worldHappiness %>% filter(!is.na(worldHappiness$"Life.Ladder"))


###### Map data
# get country centroid coordinates
dataWithSpatial <- left_join(worldMap, worldHappiness, by = c("name" = "Country.Name"))
dataWithSpatial$centroid <- st_centroid(dataWithSpatial$geometry)
dataWithSpatial$lng <- st_coordinates(dataWithSpatial$centroid)[, "X"]
dataWithSpatial$lat <- st_coordinates(dataWithSpatial$centroid)[, "Y"]
N_COUNTRIES <- length(unique(worldHappiness$"Country.Name"))

# calculate happiness class
MIN_INDEX <- min(dataWithSpatial$"Life.Ladder", na.rm = TRUE)
MAX_INDEX <- max(dataWithSpatial$"Life.Ladder", na.rm = TRUE)
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
  neutral = makeIcon("icons/face-meh-solid.svg", "icons/face-meh-solid.svg", ICON_SIZE, ICON_SIZE),
  unhappy = makeIcon("icons/face-frown-solid.svg", "icons/face-frown-solid.svg", ICON_SIZE, ICON_SIZE),
  very_unhappy = makeIcon("icons/face-sad-tear-solid.svg", "icons/face-sad-tear-solid.svg", ICON_SIZE, ICON_SIZE)
)
# add icon type
dataWithSpatial$happiness <- cut(
  dataWithSpatial$"Life.Ladder",
  breaks = c(MIN_INDEX, SAD_THRESHOLD, SAD_THRESHOLD_2, NEUTRAL_THRESHOLD, HAPPY_THRESHOLD, MAX_INDEX),
  labels = c("very_unhappy", "unhappy", "neutral", "happy", "very_happy"),
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
    HTML("<p><a href=\"https://worldhappiness.report/\">The World Happiness Report</a> is a survey that
    evaluates global happiness levels.
    It's gaining more attention as governments and organizations increasingly use its happiness
    metrics for policy decisions. Experts from various fields like economics, psychology, and public
    policy contribute to the report, explaining how well-being measures can effectively gauge a
    nation's progress. The report reviews current happiness states globally and delves into the
    science behind happiness variations.</p>"),
    HTML("<p>This project aims to explore the World Happiness Report data from <strong>2005 to 2022</strong>
    with <strong>165</strong> countries. </p>"),
    br(),
    fluidRow(
      actionButton("start_explore", "Start Explore", class = "btn btn-primary btn-block")
    )
  )
)

rank_tab <- tabPanel(
  title = "Trends",
  tags$head(
    tags$style(HTML("
      .row .col-sm-4 {
        width: 320px;
      }
      #line_plot {
        width: 100% !important;
      }
      #statistic {
        padding-top: 20px;
      }
      #statistic_title {
        margin-bottom: 0px
      }
    ")),
  ),
  h2("World Happiness Score Trends"),
  p("Compare the happiness scores of countries over time."),
  sidebarLayout(
    sidebarPanel(
      div(
        style = "height: 700px; width: 270px; overflow-y: scroll;",
        checkboxGroupInput(
          "country_select",
          HTML("<h5>Choose a country:</h5>"),
          choices = sort(unique(worldHappiness$"Country.Name")),
          selected = "Australia"
        )
      ),
      actionButton("clear_all", "Clear All"),
    ),
    mainPanel(
      fluidRow(
        column(12, girafeOutput("line_plot", height = "400px", width = "100%"))
      ),
      uiOutput("statistic_title"),
      fluidRow(
        column(6, plotOutput("radar_plot", height = "450px", width = "100%")),
        column(6, uiOutput("statistic"))
      )
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
      #country_count {
        text-align: center;
      }
    ")),
  ),
  fluidRow(
    h2("World Happiness Score Map"),
    valueBoxOutput("country_count", width = 4),
    valueBoxOutput("world_average_index", width = 4),
    valueBoxOutput("world_average_index_change", width = 4)
  ),
  leafletOutput("map_happiness", height = "calc(100vh - 220px)"),
  # control panel
  absolutePanel(
    draggable = F,
    id = "controls-container",
    sliderInput(
      "timeline",
      "Select Year",
      min = 2005, max = 2022, value = 2005,
      step = 1,
      sep = "",
      animate = animationOptions(interval = 3000)
    ),
    checkboxInput("clustering", "Enable clustering", value = TRUE),
    checkboxInput("country_name", "Hide country name", value = FALSE),
    pickerInput(
      "happiness_select", "Select Happiness Level:",
      choices = c("very_happy", "happy", "neutral", "unhappy", "very_unhappy"),
      selected = c("very_happy", "happy", "neutral", "unhappy", "very_unhappy"),
      multiple = TRUE
    ),
    br(),
    girafeOutput("plot_happiness", height = "400px"),
  ),
)

data_tab <- tabPanel(
  title = "Data",
  h2("Data Source"),
  HTML("<p>The data used in this project are from <a href='https://www.kaggle.com/datasets/usamabuttar/world-happiness-report-2005-present'>World Happiness Report, 2005-Present</a>.</p>"),
  br(),
  
)
ui <- navbarPage(
  id = "navbar",
  title = "World Happiness Report (2005-2022)",
  # reference: https://bootswatch.com/lumen/
  theme = bslib::bs_theme(bootswatch = "lumen"),
  home_tab,
  rank_tab,
  map_tab,
  data_tab
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
        !is.na(dataWithSpatial$"Life.Ladder")
    )
  })
  # get previous year of the selected year
  getPrevYear <- reactive({
    prev_year <- input$timeline - 1
    prev_year
  })
  # filter previous-year data based on input$timeline
  getFilteredDataPrevYear <- reactive({
    filter(
      dataWithSpatial,
      Year == getPrevYear() & !is.na(dataWithSpatial$"Life.Ladder")
    )
  })

  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "country_select", selected = character(0))
    selected_point$year <- NULL
    selected_point$country <- NULL
  })

  observeEvent(input$start_explore, {
    updateNavbarPage(session, "navbar", "Trends")
  })


  output$line_plot <- renderGirafe({
    if (is.null(input$country_select)) {
      p <- ggplot() +
        geom_text(aes(x = 0, y = 0, label = "No country selected. Please select a country."), size = 6) +
        theme_void()
      return(girafe(ggobj = p))
    }
    filtered_data <- worldHappiness %>% filter(Country.Name %in% input$country_select)
    worldScoreAverage <- worldHappiness %>%
      group_by(Year) %>%
      summarise(Avg = mean(Life.Ladder, na.rm = TRUE))
    y_max <- ifelse(max(worldScoreAverage$Avg) > max(filtered_data$Life.Ladder), max(worldScoreAverage$Avg) + 2, max(filtered_data$Life.Ladder, na.rm = TRUE))
    p <- ggplot() +
      geom_line_interactive(data = filtered_data, aes(x = Year, y = Life.Ladder, group = Country.Name, color = Country.Name)) +
      geom_point_interactive(data = filtered_data,
        aes(x = Year, y = Life.Ladder, color = Country.Name,
          tooltip = paste("Country:", Country.Name, "<br>", "Year:", Year, "<br>", "Score:", Life.Ladder),
          data_id = paste(Country.Name, Year)),
        size = 3) +
      geom_line_interactive(data = worldScoreAverage, aes(x = Year, y = Avg, color = "World Average"), size = 1) +
      geom_point_interactive(data = worldScoreAverage, aes(x = Year, y = Avg, color = "World Average", tooltip = paste("World Average Score in ", Year, ":", Avg)), size = 3) +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = 2005:2022) +
      scale_y_continuous(breaks = seq(0, y_max, by = 1), limits = c(0, y_max)) +
      scale_color_manual(values = c("World Average" = "gray", setNames(rainbow(length(unique(filtered_data$"Country.Name"))), unique(filtered_data$"Country.Name")))) +
      ggtitle("World Happiness Scores of Countries Over Time") +
      xlab("Year") +
      ylab("Happiness Score")
    girafe(ggobj = p, height_svg = 4, width_svg = 10, )
  })

  # reference: https://stackoverflow.com/questions/39436713/r-shiny-reactivevalues-vs-reactive
  selected_point <- reactiveValues()

  observeEvent(input$line_plot_selected, {
    # select only one point at a time
    # reference: lab 7 solution
    session$sendCustomMessage(type = "line_plot_set", message = character(0))
    # reference: https://www.geeksforgeeks.org/strsplit-function-in-r/
    point_data <- unlist(strsplit(input$line_plot_selected, " "))
    year <- tail(point_data, 1)
    country <- paste(head(point_data, -1), collapse = " ")
    # Extract the year (assuming it's the second element in the vector)
    selected_point$country <- country
    selected_point$year <- year
  })

  output$radar_plot <- renderPlot({
    if (is.null(selected_point$country)) {
      return(NULL)
    }
    filtered_data <- worldHappiness %>%
      filter(Year == selected_point$year & Country.Name == selected_point$country)
    filtered_data <- filtered_data %>%
      select(c(
        Social.Support, Freedom.To.Make.Life.Choices, Generosity,
        Perceptions.Of.Corruption, Positive.Affect, Negative.Affect, Confidence.In.National.Government
      ))
    filtered_data_formatted <- filtered_data
    # wrap column name
    colnames(filtered_data_formatted) <- gsub("\\.", "\n", colnames(filtered_data))
    # Add max and min rows for radarchart
    max_vals <- 1
    min_vals <- 0
    plot_color <- rgb(0.2, 0.5, 0.5, 0.9)
    filtered_data_formatted <- rbind(max_vals, min_vals, filtered_data_formatted)
    radarchart(
      filtered_data_formatted,
      axistype = 1,
      pcol = plot_color,
      pfcol = rgb(0.2, 0.5, 0.5, 0.4),
      plwd = 4,
      plty = 1,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1),
      cglwd = 0.8,
    )
  })
  # reference: https://rstudio.github.io/shiny/reference/renderUI.html
  output$statistic_title <- renderUI({
    if (is.null(selected_point$country)) {
      return(HTML("<h5 style='text-align: center; padding-top: 5px; margin-right: 100px;'>Select a point to view statistic</h5>"))
    }
    print(selected_point)
    HTML(paste0(
      "<h5 style='text-align: center; padding-top: 10px; margin-right: 100px;'>Statistic for ",
      selected_point$country, " in ", selected_point$year, "</h5>",
      "<p style='text-align: center; font-size: 12px; margin-right: 100px;'>
      Except for \"Negative Affect\", where a lower score is better, higher scores are better in all other dimensions.</p>"
    ))
  })

  output$statistic <- renderUI({
    if (is.null(selected_point$country)) {
      return(NULL)
    }
    filtered_data <- worldHappiness %>%
      filter(Year == selected_point$year & Country.Name == selected_point$country)
    HTML(
      paste0("
        <lu>
          <li> Log GDP Per Capita: <strong>", filtered_data$Log.GDP.Per.Capita, "</strong></li>
          <li> Healthy Life Expectancy At Birth: <strong>", filtered_data$Healthy.Life.Expectancy.At.Birth, "</strong></li>
          <br>
          <li> Confidence In National Government: <strong>", filtered_data$Confidence.In.National.Government, "</strong></li>
          <li> Freedom To Make Life Choices: <strong>", filtered_data$Freedom.To.Make.Life.Choices, "</strong></li>
          <li> Generosity: <strong>", filtered_data$Generosity, "</strong></li>
          <li> Negative Affect: <strong>", filtered_data$Negative.Affect, "</strong></li>
          <li> Perceptions Of Corruption: <strong>", filtered_data$Perceptions.Of.Corruption, "</strong></li>
          <li> Positive Affect: <strong>", filtered_data$Positive.Affect, "</strong></li>
          <li> Social Support: <strong>", filtered_data$Social.Support, "</strong></li>
        </lu>
      ")
    )
  })

  output$map_happiness <- renderLeaflet({
    dataWithSpatial <- getFilteredData()
    nrow <- dataWithSpatial %>% nrow()
    if (nrow == 0) {
      return(NULL)
    }
    leaflet_map <- leaflet(dataWithSpatial) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 80, lat = 0, zoom = 2) %>%
      setMaxBounds(lng1 = -160, lat1 = -90, lng2 = 300, lat2 = 90) %>%
      addMarkers(
        clusterOptions = markerClusterOptions(maxClusterRadius = 20),
        lng = dataWithSpatial$lng,
        lat = dataWithSpatial$lat,
        icon = ~ faceIcons[happiness],
        options = leaflet::markerOptions(happinessIndex = dataWithSpatial$"Life.Ladder"),
        popup = ~ paste(
          "Country: <strong>", dataWithSpatial$name, "</strong><br>",
          "Happiness Score:  <strong>", dataWithSpatial$"Life.Ladder", "</strong><br>",
          "Score:  <strong>", dataWithSpatial$Life.Ladder, "</strong>"
        ),
        label = ~ paste(dataWithSpatial$name),
        labelOptions = labelOptions(direction = "top")
      ) %>%
      addControl(
        html = paste0(
          '<div style="padding: 10px; background-color: white;">
              <h5>Happiness Level</h5>
              <div style="padding: 5px;"><img src="icons/face-grin-beam-solid.svg" width="20" height="20"> <strong>Very Happy</strong> (Scored ',
          format(round(HAPPY_THRESHOLD, 3), nsmall = 3), "-", format(round(MAX_INDEX, 3), nsmall = 3), ')</div>
              <div style="padding: 5px;"><img src="icons/face-smile-solid.svg" width="20" height="20"> <strong>Happy</strong> (Scored ',
          format(round(NEUTRAL_THRESHOLD, 3), nsmall = 3), "-", format(round(HAPPY_THRESHOLD, 3), nsmall = 3), ')</div>
              <div style="padding: 5px;"><img src="icons/face-meh-solid.svg" width="20" height="20"> <strong>Neutral</strong> (Scored ',
          format(round(SAD_THRESHOLD_2, 3), nsmall = 3), "-", format(round(NEUTRAL_THRESHOLD, 3), nsmall = 3), ')</div>
              <div style="padding: 5px;"><img src="icons/face-frown-solid.svg" width="20" height="20"> <strong>Unhappy</strong> (Scored ',
          format(round(SAD_THRESHOLD, 3), nsmall = 3), "-", format(round(SAD_THRESHOLD_2, 3), nsmall = 3), ')</div>
              <div style="padding: 5px;"><img src="icons/face-sad-tear-solid.svg" width="20" height="20"> <strong>Very Unhappy</strong> (Scored ',
          format(round(MIN_INDEX, 3), nsmall = 3), "-", format(round(SAD_THRESHOLD, 3), nsmall = 3), ")</div>
            </div>"
        ),
        position = "bottomleft"
      )

    # render custom clustered icons
    leaflet_map %>% onRender("
        function(el, x) {
          const HAPPY_THRESHOLD = 6.67140162;
          const NEUTRAL_THRESHOLD = 5.32386899;
          const SAD_THRESHOLD_2 = 3.97633636;
          const SAD_THRESHOLD = 2.62880373;
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
                if (averageHappiness > HAPPY_THRESHOLD) {
                  iconHtml += '<img src=\"icons/face-grin-beam-solid.svg\" '+ iconStyle + ' />';
                } else if (averageHappiness > NEUTRAL_THRESHOLD) {
                  iconHtml += '<img src=\"icons/face-smile-solid.svg\" '+ iconStyle + ' />';
                } else if (averageHappiness > SAD_THRESHOLD_2) {
                  iconHtml += '<img src=\"icons/face-meh-solid.svg\" '+ iconStyle + ' />';
                } else if (averageHappiness > SAD_THRESHOLD) {
                  iconHtml += '<img src=\"icons/face-frown-solid.svg\" '+ iconStyle + ' />';
                } else {
                  iconHtml += '<img src=\"icons/face-sad-tear-solid.svg\" '+ iconStyle + ' />';
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
                    .setContent(`${cluster.getChildCount()} countries have average Happiness Score of ${averageHappiness}`)
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
  #         options = leaflet::markerOptions(happinessIndex = dataWithSpatial$'Life.Ladder'),
  #         popup = paste(
  #           "Country:", dataWithSpatial$name, "<br>",
  #           "Happiness Score:", dataWithSpatial$'Life.Ladder', "<br>",
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
  #         options = leaflet::markerOptions(happinessIndex = dataWithSpatial$'Life.Ladder'),
  #         popup = paste(
  #           "Country:", dataWithSpatial$name, "<br>",
  #           "Happiness Score:", dataWithSpatial$'Life.Ladder', "<br>",
  #           "Rank:", dataWithSpatial$Rank
  #         ),
  #         label = paste(dataWithSpatial$name),
  #         labelOptions = labelOptions(direction = "top")
  #       )
  #   }
  # })

  output$world_average_index <- renderValueBox({
    average_happiness <- mean(getFilteredData()$"Life.Ladder", na.rm = TRUE)
    valueBox(
      ifelse(is.na(average_happiness), "Data Not Available", format(round(average_happiness, 3))),
      paste("World Average Happiness Score in ", input$timeline)
    )
  })

  output$world_average_index_change <- renderValueBox({
    average_happiness <- mean(getFilteredData()$"Life.Ladder", na.rm = TRUE)
    avaerage_happiness_last_year <- mean(getFilteredDataPrevYear()$"Life.Ladder", na.rm = TRUE)
    change <- ((average_happiness - avaerage_happiness_last_year) / avaerage_happiness_last_year) * 100
    sign <- ifelse(change > 0, "+", "")
    formated_change <- paste(sign, format(round(change, 3), nsmall = 3), "%")
    valueBox(
      ifelse(input$timeline == 2005 | is.na(change), "Data Not Available", formated_change),
      paste("Change compare to", getPrevYear()),
    )
  })

  output$country_count <- renderValueBox({
    valueBox(
      nrow(getFilteredData()),
      paste("Number of Recorded Countries in", input$timeline),
    )
  })

  output$plot_happiness <- renderGirafe({
    data <- getFilteredData()
    # Count the number of unique countries for each happiness level
    count_data <- data %>%
      group_by(happiness = data$happiness) %>%
      summarise(n = n_distinct(name, na.rm = TRUE))
    p <- ggplot(count_data, aes(x = happiness, y = n)) +
      geom_bar_interactive(aes(fill = happiness, tooltip = paste("Happiness Level:", happiness, "<br>Number of Countries:", n)), stat = "identity", width = 0.8) +
      geom_text(aes(label = n), vjust = -0.5, size = 6) +
      scale_fill_manual(values = c(
        "very_happy" = "darkgreen",
        "happy" = "#5b9c4f",
        "neutral" = "#e6c105",
        "unhappy" = "#e67905",
        "very_unhappy" = "red"
      )) +
      labs(x = "Happiness Level", y = "Number of Countries") +
      theme(
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "#e2e2e2"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 20),
      ) +
      ggtitle("Number of Countries by Happiness Level")

    girafe(ggobj = p, height_svg = 7)
  })
}

# Run the application
shinyApp(
  ui = ui, server = server
)
