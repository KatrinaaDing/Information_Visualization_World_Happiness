library('shiny')
library('ggplot2')
library('leaflet')

worldHappiness <- read.csv("/Users/katrinading/Desktop/Uni/postgrad/GEOM90007 iv/asst2/data/World Happiness Reports 2013-2023/WorldHappinessIndex2013-2023.csv")


##################
# USER INTERFACE #
##################


rank_tab <- tabPanel(
  title='Ranks',
  h2('World Happiness Ranks'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId='year',
        label='Year',
        choices=c('2012'='X2017',
                  '2018'='X2018',
                  '2019'='X2019',
                  '2020'='X2020'),
        selected='X2020'
      )
    ),
    mainPanel(
      girafeOutput('plot_births')
    )
  )
)

# Define UI for application that draws a histogram
ui <- navbarPage(
  title='World Happiness Report',
  rank_tab
)

################
# SHINY SERVER #
################

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$distPlot <- renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
