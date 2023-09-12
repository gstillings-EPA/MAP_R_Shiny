# Load required libraries
library(shiny)       # For building web applications
library(tidyverse)   # For data manipulation and visualization
library(RSQLite)     # For SQLite database operations
library(leaflet)     # For interactive maps


# Define the work year
workyear <- "2024"

# Connect to SQLite database and read tables into 'calls', 'assess', and 'dataset'
mydb <- dbConnect(RSQLite::SQLite(), "ShinyExample.sqlite")
calls <- dbReadTable(mydb, "NatCallLong", row.names = FALSE)
attri <- dbReadTable(mydb,"ALL_ATTRIBUTES", as.is = TRUE)
assess <- dbReadTable(mydb, "DOpHAssessments", row.names = FALSE)
dataset <- dbReadTable(mydb, "DATASET", row.names = FALSE)

# Data preprocessing for 'dataset'
dataset <- dataset %>%
  select(STAT, DateTime, PARM, Result) %>%
  mutate(DateTime = as.Date(DateTime)) %>%
  arrange(STAT, DateTime)  # Arrange data by station and datetime for better visualization and analysis

attri <- attri %>%
  mutate(LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE))

# Create a copy of 'calls' in 'allcalls'
allcalls <- calls

# Filter 'calls' for the specified 'workyear'
calls <- calls %>% filter(Year == workyear)

# Get unique stations and parameters
stations <- calls %>% select(STATION) %>% distinct
stations <- sort(as.vector(stations[,"STATION"]))
parms <- calls %>% select(Parm) %>% distinct %>% as.list
parms <- as.vector(parms$Parm)


getColor <- function(calls) {
  sapply(calls$Call, function(call) {

    if(is.na(call) ) {
      "white"
    } else if(call == "MEETING") {
      "green"
    } else if(call == "LIST") {
      "red"
    } else if(call == "NAT") {
      "blue"
    } else {
      "white"
    } }, USE.NAMES = FALSE)
}




# Define the user interface (UI)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("Station", "Station", stations),  # Dropdown for selecting a station
      radioButtons("Parm", "Parms", parms, inline = TRUE),  # Radio buttons for selecting a parameter
      leafletOutput("Map")
    ),
    mainPanel(
      plotOutput("ploted"),  # Display plot output
      tableOutput("CallTable"),  # Display table output for calls
      tableOutput("AssessTable")  # Display table output for assessments
    )
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

  # Render the plot output
  output$ploted <- renderPlot({
    dataplot <-
      dataset %>% filter(STAT == input$Station) %>%
      filter(PARM == input$Parm) %>%
      mutate(Year = year(DateTime))

    plotout <-
      ggplot(data = dataplot, aes(x = DateTime, y = Result))
    plotout <-
      plotout + geom_point() + theme(axis.text = element_text(size = 14))
    plotout
  })

  output$Map <- renderLeaflet({
    allcallsworkyear <- v$allcalls %>%
      filter(Parm == input$Parm) %>%
      filter(Year == workyear)

    mapattri <-
      attri %>% select(STATION, LONGITUDE, LATITUDE) %>%
      left_join(allcallsworkyear)

    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(mapattri)
    )

    mapattristat <- attri %>%
      select(STATION, LONGITUDE, LATITUDE) %>%
      filter(STATION == input$Station)

    long <- mapattristat[1,]$LONGITUDE
    lat <- mapattristat[1,]$LATITUDE

    m <- leaflet(mapattri) %>%
      setView(lng = long,
              lat = lat,
              zoom = 12) %>%
      addAwesomeMarkers(popup = mapattri$STATION, icon = icons) %>%
      addWMSTiles(
        "https://basemap.nationalmap.gov/arcgis/services/USGSTopo/MapServer/WmsServer",
        group = "USGS TOPO",
        layers = "0",
        options = WMSTileOptions(format = "image/png", transparent = TRUE)
      )

    m


  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
