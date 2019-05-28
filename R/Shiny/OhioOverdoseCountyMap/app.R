#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("leaflet")
library("tidyverse")

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

# Load data needed for map
OhioCounties <- raster::getData("GADM", country = "usa", level = 2)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$OhioCountyOverdoseMap <- renderLeaflet({
        map <- leaflet() %>%
            addTiles() %>%
            setView(lng = -80, lat = 41, zoom = 8)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
