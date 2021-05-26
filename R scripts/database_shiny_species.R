## Tropical South American Diatom Database

##########################################################
##  ShinyApp for visualization of species occurrence   ###
##########################################################

library(leaflet)
library(shiny)
library(tidyverse)
library(shinyWidgets)

#Read in assembled diatom datasets and Regions
combined <- read.csv("data/assembledspp.csv", row.names=1)
lake_regions <- read.csv("data/regions.csv", row.names = 1)

##Merge diatom datasets and regions datasets
modern_lakes <- merge(combined, lake_regions, by="row.names")

## Sites
sitesDB <- read.csv("data/biogeographySites.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(CollectionName, Country, Collector.Analyst, Year, SiteName, SampleType, Habitat, Substrate,
                code, region, Lat.DD.S, Long.DD.W) %>%
  mutate(region=str_replace(region, "Colombia-Andes-Central", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Andes-Eastern", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-North", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Eastern", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Western", "Colombia-Lowlands")) %>%
  rename(Row.names=code) 

#transform dataframe to tidy format
df_thin <- modern_lakes %>%
  gather(key = taxa, value = count, -Row.names, -region)#don't gather region


## Make the same without filtering spp for obtaining a list of diatom spp with coordinates
diatoms_list <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_1)) %>%
  group_by(region, Row.names, taxa) %>%
  summarise(count = sum(count)) %>%
  spread(key = taxa, value = count) %>%
  as.data.frame() %>%
  left_join(sitesDB, by="Row.names") %>% 
  select(-c(CollectionName, Country, Collector.Analyst, region.y, Row.names)) %>%
  gather(key = taxa, value = abund, -Habitat, -Lat.DD.S, -Long.DD.W, -region.x, -Substrate, -Year, -SampleType, -SiteName) %>%
  filter(!taxa=="Auxospores") %>%
  mutate(taxa=factor(taxa)) %>%
  #assign presence/absence column
  mutate(pres_abs=ifelse(abund>0.5, 1,0)) %>% #the shiny collapses if abund<0.5
  #create cut levels of abundance
  mutate(abund_lvl=cut(abund, 
                       c(0,.5,1,2,3,5,100), include.lowest = T,
                       labels = c('<0.5%' ,'0.5-1%', '1-2%', '2-3%', '3-5%','>5%')))

# then assign a palette to this using colorFactor
abundColour <- colorFactor(palette = 'RdYlGn', diatoms_list$abund_lvl)

# Run the Shiny App
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
