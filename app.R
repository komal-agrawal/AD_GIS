library(leaflet)
library(tigris)
library(shiny)
library(shinydashboard)

master.data <- read.csv("C:/Users/komal/Documents/AD_GIS_Project/Data/master_data.csv")
us_regions <- regions(resolution = '20m')

# Choices for Radiobutton
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "TNSSS vs AD"),
  dashboardSidebar(),
  dashboardBody(
    fluidPage(
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          # implementing radio button
          radioButtons(inputId = "Analyte", label = "Analyte: ", 
                       c("Aluminum" = "ALUMINUM", "Arsenic" = "ARSENIC", 
                         "Cadmium" = "CADMIUM", "Caffeine" = "CAFFEINE", 
                         "Doxycycline" = "DOXYCYCLINE", 
                         "Fluoxetine" = "FLUOXETINE", 
                         "Ibuprofen" = "IBUPROFEN", 
                         "Minocycline" = "MINOCYCLINE",
                         "17 Alpha-Estradiol" = "17 ALPHA-ESTRADIOL",
                         "Progesterone" = "PROGESTERONE"))),
        
        # Show a plot of the generated distribution
        mainPanel
        (
          fluidRow(
            tags$head(tags$style('.col-sm-6 {padding-left: 0px; padding-right: 0px ;}')),
            box(title = "TNSSS Visualization", leafletOutput("analyteMap", width = "200%", height = "500px"))),
          fluidRow(box(title = "Crude Rate of Alzheimer's disease in U.S.", leafletOutput("adMap", width = "200%", height = "500px")))
        )
      ))))

server <- function(input, output) {
  
  output$analyteMap <- renderLeaflet({
    # Data Processing
    # browser()
    sub.data <- master.data[master.data$Analyte == input$Analyte, ]
    analyte <- sub.data[, c("Analyte", "Region", "Analyte.Conc.")] 
    analyte <- na.omit(analyte)
    # Average means from 4 US Census regions
    analyte.NE <- analyte[analyte$Region == "Northeast", ]
    analyte.S <- analyte[analyte$Region == "South", ]
    analyte.MW <- analyte[analyte$Region == "Midwest", ]
    analyte.W <- analyte[analyte$Region == "West", ]
    mean.NE <- mean(analyte.NE$Analyte.Conc)
    mean.S <- mean(analyte.S$Analyte.Conc.)
    mean.MW <- mean(analyte.MW$Analyte.Conc.)
    mean.W <- mean(analyte.W$Analyte.Conc.)
    
    # Merge Aluminum means into dataframe
    df <- data.frame("Region" = c("Northeast", "South", "Midwest", "West"), "Avg Concentration" = c(mean.NE, mean.S, mean.MW, mean.W))
    
    # Merge Aluminum tabular data and spatial data
    df.merge <- geo_join(spatial_data = us_regions, data_frame = df, by_sp = "NAME", by_df = "Region")
    
    
    # Make aluminum map using leaflet
    pal <- colorNumeric(
      palette = "Reds",
      domain = df.merge$Analyte.Concentration)
    
    al.labels <- sprintf(
      "<strong>%s</strong><br/> Average []: %g",
      df.merge$Region, df.merge$Avg.Concentration
    ) %>% lapply(htmltools::HTML)
    
    map <- leaflet(df.merge) %>%
      addTiles() %>%
      setView(-97.922211, 39.381266, zoom = 2) %>%
      addPolygons(
        data = df.merge, fillColor = ~pal(Avg.Concentration),
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        smoothFactor = 0.2,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = al.labels,
        labelOptions = labelOptions
        (
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ))%>% 
      addLegend(pal = pal, values = ~Avg.Concentration, opacity = 0.7, position = "bottomright", title = "Average Analyte Concentration")
  })
  
  
  output$adMap <- renderLeaflet({
    #CDC Wonder 2007 Alzheimer's disease data
    ad_states <- geojsonio::geojson_read("C:/Users/komal/Documents/AD_GIS_Project/AD-Crude-Rate-us.geojson", what = "sp")
    #rgdal::getGDALVersionInfo() Debug: version 2.2.3
    class(ad_states)
    names(ad_states)
    #Create basic basemap to iterate and add layers
    p <- leaflet(ad_states) %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Add state outlines
    p %>% addPolygons()
    
    #Color scale for Crude Rate of AD
    bins <- c(0, 50, 100, 150, 200, 250, 300, 350, 500)
    pal <- colorBin("YlOrRd", domain = ad_states$crude_rate, bins = bins)
    p %>% addPolygons(
      fillColor = ~pal(crude_rate),
      weight = 2,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 6,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE))
    labels <- sprintf(
      "<strong>%s</strong><br/> Crude Rate: %g",
      ad_states$name, ad_states$crude_rate
    ) %>% lapply(htmltools::HTML)
    
    p <- p %>% addPolygons(
      fillColor = ~pal(crude_rate),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
      addLegend(pal = pal, values = ~crude_rate, opacity = 0.7, title = NULL,
                position = "bottomright")
  })
  
}

shinyApp(ui, server)