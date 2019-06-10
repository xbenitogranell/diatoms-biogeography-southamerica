Tropical South American Diatom Database
================
Xavier Benito and multiple collaborators

<!-- README.md is generated from README.Rmd. Please edit that file -->

This repository contains a georeferenced inventory of diatom sites of tropical South America.
The [database](http://www.xavierbenito.com/bgsa/bgsa.html) includes diatom (unicelular siliceous algae) data and associated environmental variables of waterbody sites (streams, wetlands, lakes), covering an altitudinal gradient from 220 to 5,070 m a.s.l. between 8°N–30°S and 58–79°W.

Datasets are mostly available on [Dryad](https://datadryad.org/resource/doi:10.5061/dryad.ck7pt) while here the aim is to provide a [leaflet](https://rstudio.github.io/leaflet/) map and [shiny](http://shiny.rstudio.com) app to interactively explore the database in R for promoting biodiversity, biogeographic and palaeoecological studies in tropical South America using diatoms as sentinel organisms.

``` {r}
library(leaflet)
library(tidyverse)
library(shiny)
```

Load inventory of sites

``` {r}
diatom_data <- read.csv("data/Diatom_Biogeography_South_America_DB-def.csv", stringsAsFactors = FALSE)

head(diatom_data)
```

Filter out non-tropical sites and artifical habitats

``` {r}
diatom_subset <- diatom_data %>% filter(!region=="Tierra del Fuego" & !Habitat=="channel")
```

Create a leaflet map. Click on the markers to see site's basic information

``` {r}
map <- leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addMarkers(lat = diatom_subset$lat, 
             lng = diatom_subset$long,
             clusterOptions = markerClusterOptions(),
             popup = paste("<strong>", "Site: ", "</strong>", as.character(diatom_subset$SiteName), "<br>",
                           "<strong>", "Habitat: ", "</strong>", as.character(diatom_subset$Habitat), "<br>",
                           "<strong>", "Sample type: ", "</strong>", as.character(diatom_subset$SampleType), "<br>",
                           "<strong>", "Collection: ", "</strong>", as.character(diatom_subset$Collection), "<br>",
                           "<strong>", "Year: ", "</strong>", as.character(diatom_subset$Year)))
                           
map 
```

Create a shiny map to display habitat types. Click on the markers to see site's basic information

\`\`\`{r TheShinyExample, warning = FALSE} shinyApp( ui = fluidPage( tags$div(title = "This input has a tool tip",  selectInput(inputId = "habitat",  label = "Habitat type",  choices = sort(unique(diatom\_subset$Habitat)))), leafletOutput("MapPlot1") ),

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
      

leafletProxy("MapPlot1") %&gt;% clearMarkers() %&gt;% addCircleMarkers(lng = sites*l**o**n**g*, *l**a**t* = *s**i**t**e**s*lat, popup = paste("<strong>", "Site: ", "</strong>", as.character(sites$SiteName), "&lt;br&gt;",  "&lt;strong&gt;", "Habitat: ", "&lt;/strong&gt;", as.character(sites$Habitat), "<br>", "<strong>", "Sample type: ", "</strong>", as.character(sites$SampleType), "&lt;br&gt;",  "&lt;strong&gt;", "Collection: ", "&lt;/strong&gt;", as.character(sites$Collection), "<br>", "<strong>", "Year: ", "</strong>", as.character(sites$Year)))

    })

}, options = list(height = 600) )

\`\`\`
