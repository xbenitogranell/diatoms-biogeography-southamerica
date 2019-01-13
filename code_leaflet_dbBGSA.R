setwd("~/Documents/ubuntu-sf/dbBGSA/leaflet")

library(leaflet)
library(tidyverse)
library(shiny)

diatom_data <- read.csv("Diatom_Biogeography_South_America_DB-def.csv", stringsAsFactors = FALSE)

#filter only unique sitenames
diatom_subset <- diatom_data %>% filter(!region=="Tierra del Fuego" & !Habitat=="channel") #not working

#plot 
map <- leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addMarkers(lat = diatom_subset$lat, 
             lng = diatom_subset$long,
             clusterOptions = markerClusterOptions(),
             popup = paste("<strong>", "Site: ", "</strong>", as.character(diatom_subset$SiteName), "<br>",
                           "<strong>", "Habitat: ", "</strong>", as.character(diatom_subset$Habitat), "<br>",
                           "<strong>", "Sample type: ", "</strong>", as.character(diatom_subset$SampleType), "<br>",
                           "<strong>", "Collection: ", "</strong>", as.character(diatom_subset$Collection)))
                           

map

#shinyApp

shinyApp(
  ui = fluidPage(
     tags$div(title = "This input has a tool tip",
             selectInput(inputId = "habitat", 
                         label = "Habitat type", 
                         choices = sort(unique(diatom_subset$Habitat)))),
    leafletOutput("MapPlot1")
  ),
  
  server = function(input, output) {
    
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("Esri.WorldImagery") %>% 
        setView(lng = -68, lat = -15, zoom = 3)
    })
    
    observe({
      
      habitat <- input$habitat
      
      sites <- diatom_subset %>% 
        filter(diatom_subset$Habitat %in% habitat)
      
      leafletProxy("MapPlot1") %>% clearMarkers() %>% 
        addCircleMarkers(lng = sites$long,
                        lat = sites$lat,
        popup = paste("<strong>", "Site: ", "</strong>", as.character(sites$SiteName), "<br>",
                      "<strong>", "Habitat: ", "</strong>", as.character(sites$Habitat), "<br>",
                      "<strong>", "Sample type: ", "</strong>", as.character(sites$SampleType), "<br>",
                      "<strong>", "Collection: ", "</strong>", as.character(sites$Collection)))

    })
  },
  options = list(height = 600)
)

