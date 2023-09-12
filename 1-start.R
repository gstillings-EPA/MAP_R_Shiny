# Load required libraries
library(shiny)
library(tidyverse)
library(RSQLite)
library(leaflet)

# Define the work year
workyear <- "2024"

# Connect to SQLite database and read table into 'calls'
mydb <- dbConnect(RSQLite::SQLite(), "ShinyExample.sqlite")
calls <- dbReadTable(mydb, "NatCallLong", row.names = FALSE)

# Create a copy of 'calls' in 'allcalls'
allcalls <- calls

# Filter 'calls' for the specified 'workyear'
calls <- calls %>% filter(Year == workyear)

# Get unique stations and parameters
stations <- calls %>% select(STATION) %>% distinct
stations <- sort(as.vector(stations[,"STATION"]))
parms <- calls %>% select(Parm) %>% distinct %>% as.list
parms <- as.vector(parms$Parm)

# Define the user interface (UI)
ui <- fluidPage(
  mainPanel(
    selectInput("Station", "Station", stations),  # Dropdown for selecting a station
    radioButtons("Parm", "Parms", parms, inline = TRUE),  # Radio buttons for selecting a parameter
    tableOutput("CallTable")  # Display table output

  )
)

# Define the server logic
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    dbDisconnect(mydb)
    stopApp()
  })
  v <- reactiveValues(
    allcalls = allcalls  # Store 'allcalls' in a reactive value
  )

  # Define a reactive expression for filtered calls based on user input
  callsin <- reactive({
    v$allcalls %>%
      filter(STATION == input$Station) %>%
      filter(Parm == input$Parm) %>%
      arrange(desc(Year))
  })

  # Render the table output
  output$CallTable <- renderTable({
    callsin()
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
