

library(leaflet)
library(shiny)
library(tidyverse)

data_dir <- "~/diatoms-biogeography-southamerica/data"
all_regions <- read.csv("data/all_regions.csv", row.names=1)
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
                  choices = all_regions$region),
      width = 6
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        #tabPanel("summary", tableOutput("summary_table")),
        tabPanel("taxa", tableOutput("species")),
        tabPanel("environment", tableOutput("env_data")),
        tabPanel("region", tableOutput("region_info")),
        tabPanel("map", leafletOutput("map",width="80%",height="600px")),
        tabPanel("plots", plotOutput("boxplots", height = "2000px")))
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$species <- renderTable({
    readr::read_csv(file = glue::glue("{data_dir}/diatom-datasets/{input$region} .csv"))
    }, striped = TRUE, width="auto")

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

}


# Run the application 
shinyApp(ui = ui, server = server)




