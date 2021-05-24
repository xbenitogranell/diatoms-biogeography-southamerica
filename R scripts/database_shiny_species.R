## Tropical South American Diatom Database

##########################################################
##  ShinyApp for visualization of species occurrence   ###
##########################################################

library(leaflet)
library(shiny)
library(tidyverse)
library(shinyWidgets)


shinyApp(
  ui = fluidPage(headerPanel('Distribution of diatom taxa in South America'),
                 sidebarPanel(
                   selectizeInput(selected = NULL,
                                  inputId = "search",
                                  'species',
                                  multiple = TRUE,
                                  label="Search species",
                                  #options = list(create=TRUE),
                                  options = list(plugins= list('remove_button')),
                                  choices=as.character(unique(sort(diatoms_list$taxa)))),
                   
                   # wellPanel(span(h4(strong("Taxa:")), h4(textOutput("taxa"))),
                   #           br()),
                   # 
                 ),
                 
                 mainPanel(
                   leafletOutput("MapPlot1",
                                 width = '100%',
                                 height = 600)
                   
                 )
  ),
  
  server = function(input, output) {
    
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("Esri.WorldImagery") %>% 
        setView(lng = -68, lat = -15, zoom = 3) %>%
        addCircles(lng=sitesDB$Long.DD.W,
                   lat=sitesDB$Lat.DD.S,
                   color = "lightblue",
                   #fillColor = "black",
                   opacity = 0.4)
      

    })
    
    observe({
      
      species <- input$search
      all <- diatoms_list %>% filter(diatoms_list$abund>0.1 & diatoms_list$taxa %in% species)
      
      leafletProxy("MapPlot1") %>% clearMarkers() %>% 
        addCircleMarkers(lng = all$Long.DD.W,
                         lat = all$Lat.DD.S,
                         radius = sqrt(all$abund*5),
                         stroke = TRUE, 
                         color = abundColour(all$abund_lvl),
                         fillOpacity = 1,
                         popup = paste("<strong>", "Region: ", "</strong>", as.character(all$region.x),
                                       "<br>",
                                       "<strong>Site Name:</strong>",
                                       as.character(all$SiteName),"<br>","<strong>Year:</strong>",as.character(all$Year))) %>%
        
        setView(lng = mean(all$Long.DD.W), lat = mean(all$Lat.DD.S), zoom = 05) %>%
        addScaleBar(.,
                    position = 'topright') %>%
        addLegend('bottomright', pal = abundColour, values = all$abund_lvl,
                  title = 'Relative abundance (%)', layerId = "species",
                  opacity = 1)
      
      
    })
    
    #output$taxa <- renderText({ unique(as.character(all()$taxa))})
    
  }
)
