#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a map of overdose deaths in Ohio by county
ui <- fluidPage(

    # Application title
    titlePanel("Ohio Overdose Deaths by County"),

    # Sidebar with a slider input for year 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = 2005,
                        max = 2017,
                        value = 2017)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("OhioCountyOverdoseMap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
