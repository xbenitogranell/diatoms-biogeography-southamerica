## Tropical South American Diatom Database
## Shiny app for site visualization and data exploration

library(leaflet)
library(shiny)
library(tidyverse)
library(shinyWidgets)

# Create data directory
data_dir <- "~/R/diatoms-biogeography-southamerica/data"

# Read in diatom taxa (types) names for harmonisation 
changes_training <- read.csv("data/old_new_nms_trainingset.csv", sep=";", stringsAsFactors = FALSE)

# Read in region names
all_regions <- read.csv("data/all_regions_new.csv", row.names=1)
colnames(all_regions) <- "region"


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Tropical South American Diatom Database"),
  
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      selectInput("region",
                  "Region",
                  choices = sort(all_regions$region)),
      width = 6
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Region", tableOutput("region_info")),
        tabPanel("Map", leafletOutput("map",width="80%",height="600px")),
        tabPanel("Environment-table", tableOutput("env_data")),
        tabPanel("Environment-boxplots", plotOutput("boxplots", height = "2000px")),
        tabPanel("Taxa", tableOutput("species")),
        tabPanel("Ecological groups", plotOutput("sppplots", height = "600px")))
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$species <- renderTable({
    readr::read_csv(file = glue::glue("{data_dir}/diatom-datasets/{input$region} .csv"))
    }, striped = TRUE, width="auto")
  
  species_data <- reactive({
    readr::read_csv(file = glue::glue("{data_dir}/diatom-datasets/{input$region} .csv"))
  })
  
  output$env_data <- renderTable({
    readr::read_csv(file = glue::glue("{data_dir}/region-datasets/{input$region} .csv")) 
  }, striped = TRUE, width="auto") 
  

  output$region_info <- renderTable({
    readr::read_csv(file = glue::glue("{data_dir}/sites-datasets/{input$region} .csv"))
    }, striped = TRUE, width="auto")

  site_data <- reactive({
    readr::read_csv(file = glue::glue("{data_dir}/sites-datasets/{input$region} .csv"))
  })
  
  
  # # To display transposed table
  # output$region_info <- renderTable({
  #   req(input$region)
  #   t(site_data())
  # })
  
  ## create static element
  output$map <- renderLeaflet({
    leaflet(site_data()) %>%
    setView(lng = -68, lat = -15, zoom = 3)
  })

  observe({
    leafletProxy("map", data = site_data()) %>%
      clearMarkers() %>%   ## clear previous markers
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = mean(~Long.DD.W), lat = mean(~Long.DD.W), zoom = 05) %>%
      
    
    addMarkers(
      lng = ~Long.DD.W, # note the tildes before values, required
      lat = ~Lat.DD.S,
      popup = ~paste(
         SiteName,
         "<br>",
         "<strong>Habitat:</strong>",
         Habitat,
         "<br>",
         "<strong>Substrate:</strong>",
         Substrate,
         "<br>",
         "<strong>Year:</strong>",
         Year
      )
    )
})
  
  region_data <- reactive({
    readr::read_csv(file = glue::glue("{data_dir}/region-datasets/{input$region} .csv"))
  })
  
  output$boxplots <- renderPlot({
    region_env_plot <- region_data() %>%
    dplyr::select(pH, Cond, Water.T, TP, Depth_avg, Ca, Mg, K, Elevation,
                  MAT, P.season, MAP, T.season, 
                  Depth_avg, area_waterbody, Wshd_area, 
                  lake_depth_ratio, lake_catch_ratio, catch_vol_ratio,
                  HFP2009,Agriculture, Crops.and.town, Grassland.and.shrubs) %>%
    gather(key=variable, value=value)
    
    
  region_env_plot %>% ggplot(aes(x=variable, y=value)) +
    geom_boxplot() +
    facet_wrap(~variable, scale="free") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    theme_bw()
  })

  #make spp plots by ecological groups
  output$sppplots <- renderPlot({
    species_plt <- species_data() %>%
      gather(key=taxa, value=count, -X1) %>%
      mutate(taxa = plyr::mapvalues(taxa, from=changes_training$old, to=changes_training$new_2)) %>% #ecological grouping
      group_by(X1, taxa) %>%
      summarise(count = sum(count)) %>%
      filter(!count == "0" ) %>% #this is to remove empty samples (rows)
      ungroup() %>%
      group_by(X1) %>%
      mutate(relative_abundance_percent = count / sum(count) * 100) %>%
      mutate(plank=sum(count[taxa=="freshwater_planktic" | taxa=="tycoplanktonic"])) %>%
      mutate(benthic=sum(count[taxa=="epiphytics"| taxa== "saline" | taxa=="benthic"])) %>%
      mutate(P_B=plank/benthic) %>%
      mutate(P_B2=(plank-benthic)/(plank+benthic)) %>% #[-1(benthic dominated) to 1(planktic dominated)]
      filter(taxa %in% c("benthic", "saline", "epiphytics",
                         "freshwater_planktic", "tycoplanktonic", "oligosaline_planktic"))%>%
      ungroup() 
    
    species_plt %>% ggplot(aes(x="", y=count, fill=taxa)) +
      geom_bar(stat="identity")+
      coord_polar("y", start=0) +
      labs(x = NULL, y = NULL, fill = NULL, title = "")+
      theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank())+
      theme_void() 
    
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)


