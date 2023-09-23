# Box with descriptionBlock
if (interactive()) {
  library(shiny)
  library(bs4Dash)
  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(
        title="sdf"
      ),
      dashboardSidebar(),
      bs4DashNavbar(
        title="navbar1"
      ),
      dashboardBody(
        box(
          solidHeader = FALSE,
          title = "Status summary",
          background = NULL,
          width = 12,
          status = "danger",
          footer = fluidRow(
            column(
              width = 6,
              descriptionBlock(
                number = "17%", 
                numberColor = "pink", 
                numberIcon = icon("caret-up"),
                header = "$35,210.43", 
                text = "TOTAL REVENUE", 
                rightBorder = TRUE,
                marginBottom = FALSE
              )
            ),
            column(
              width = 6,
              descriptionBlock(
                number = "18%", 
                numberColor = "secondary", 
                numberIcon = icon("caret-down"),
                header = "1200", 
                text = "GOAL COMPLETION", 
                rightBorder = FALSE,
                marginBottom = FALSE
              )
            )
          )
        )
      ),
      title = "Description Blocks"
    ),
    server = function(input, output) { }
  )
}

if (interactive()) {
  library(shiny)
  library(bs4Dash)
  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(
        box(
          title = "Box with right pad",
          status = "warning",
          fluidRow(
            column(width = 6),
            column(
              width = 6,
              boxPad(
                color = "purple",
                descriptionBlock(
                  header = "8390", 
                  text = "VISITS", 
                  rightBorder = FALSE,
                  marginBottom = TRUE
                ),
                descriptionBlock(
                  header = "30%", 
                  text = "REFERRALS", 
                  rightBorder = FALSE,
                  marginBottom = TRUE
                ),
                descriptionBlock(
                  header = "70%", 
                  text = "ORGANIC", 
                  rightBorder = FALSE,
                  marginBottom = FALSE
                )
              )
            )
          )
        )
      ),
      title = "boxPad"
    ),
    server = function(input, output) { }
  )
}

# A box with label, sidebar, dropdown menu
if (interactive()) {
  library(shiny)
  library(bs4Dash)
  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(
        box(
          title = "Closable Box with dropdown",
          closable = TRUE,
          width = 12,
          status = "warning",
          solidHeader = FALSE,
          collapsible = TRUE,
          label = boxLabel(
            text = 1,
            status = "danger"
          ),
          dropdownMenu = boxDropdown(
            boxDropdownItem("Link to google", href = "https://www.google.com"),
            boxDropdownItem("item 2", href = "#"),
            dropdownDivider(),
            boxDropdownItem("item 3", href = "#", icon = icon("table-cells"))
          ),
          sidebar = boxSidebar(
            startOpen = TRUE,
            id = "mycardsidebar",
            sliderInput(
              "obs",
              "Number of observations:",
              min = 0,
              max = 1000,
              value = 500
            )
          ),
          plotOutput("distPlot")
        )
      )
    ),
    server = function(input, output) {
      output$distPlot <- renderPlot({
        hist(rnorm(input$obs))
      })
    }
  )
}
# Toggle a box on the client
if (interactive()) {
  library(shiny)
  library(bs4Dash)
  
  ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
      tags$style("body { background-color: ghostwhite}"),
      fluidRow(
        actionButton("toggle_box", "Toggle Box"),
        actionButton("remove_box", "Remove Box", class = "bg-danger"),
        actionButton("restore_box", "Restore Box", class = "bg-success")
      ),
      actionButton("update_box", "Update Box", class = "bg-info"),
      actionButton("update_box2", "Update Box 2", class = "bg-info"),
      br(),
      br(),
      box(
        title = textOutput("box_state"),
        id = "mybox",
        status = "danger",
        background = "maroon",
        solidHeader = TRUE,
        gradient = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        plotOutput("plot")
      )
    )
  )
  
  server <- function(input, output, session) {
    output$plot <- renderPlot({
      req(!input$mybox$collapsed)
      plot(rnorm(200))
    })
    
    output$box_state <- renderText({
      state <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
      paste("My box is", state)
    })
    
    observeEvent(input$toggle_box, {
      updateBox("mybox", action = "toggle")
    })
    
    observeEvent(input$remove_box, {
      updateBox("mybox", action = "remove")
    })
    
    observeEvent(input$restore_box, {
      updateBox("mybox", action = "restore")
    })
    
    observeEvent(input$mybox$visible, {
      collapsed <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
      visible <- if (input$mybox$visible) "visible" else "hidden"
      message <- paste("My box is", collapsed, "and", visible)
      showNotification(message, type = "warning", duration = 1)
    })
    
    observeEvent(input$update_box, {
      updateBox(
        "mybox",
        action = "update",
        options = list(
          title = h2("hello", dashboardBadge(1, color = "primary")),
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          background = NULL,
          height = "900px",
          closable = FALSE
        )
      )
    })
    
    observeEvent(input$update_box2, {
      updateBox(
        "mybox",
        action = "update",
        options = list(
          status = NULL,
          solidHeader = FALSE,
          width = 4,
          background = "green",
          height = "500px",
          closable = TRUE
        )
      )
    })
  }
  
  shinyApp(ui, server)
}
