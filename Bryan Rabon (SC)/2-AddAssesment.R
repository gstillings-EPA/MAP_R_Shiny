# Load required libraries
library(shiny)
library(tidyverse)
library(RSQLite)
library(leaflet)

# Define the work year
workyear <- "2024"

# Connect to SQLite database and read tables into 'calls' and 'assess'
mydb <- dbConnect(RSQLite::SQLite(), "ShinyExample.sqlite")
calls <- dbReadTable(mydb, "NatCallLong", row.names = FALSE)
assess <- dbReadTable(mydb, "DOpHAssessments", row.names = FALSE)

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
    tableOutput("CallTable"),  # Display table output for calls
    tableOutput("AssessTable")  # Display table output for assessments
  )
)

# Define the server logic
server <- function(input, output, session) {

  # Define a function to run on session end to disconnect the database and stop the app
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

  # Define a reactive expression for filtered assessments based on user input
  statasses <- reactive({
    assess %>%
      filter(STATION == input$Station) %>%
      select(STATION, CLASS, Year, contains(input$Parm)) %>%
      arrange(desc(Year))
  })

  # Render the table output for calls
  output$CallTable <- renderTable({
    callsin()
  })

  # Render the table output for assessments
  output$AssessTable <- renderTable({
    statasses()
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
