#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(terra)
library(viridis)

path.input <- '/Users/RDEL1VMM/Desktop/current projects/Aquinnah Herring Hatchery/Model_results_5_7_24/'

# Fetch corresponding raster data based on the selected parameter
## Preprocessing

# Load Raster of area
A_area.rast <- terra::rast(paste0(path.input,("A_area_mask_10m.tif")))

# Load Shapefile of area
A_area.shp <- terra::vect(paste0(path.input,("Aquinnah_Project_Area.shp")))
A_area.shp <- terra::project(A_area.shp, "EPSG:4269")

# Load Shapefile of squibnocket
S_area.shp <- terra::vect(paste0(path.input,("Squibnocket_pond_only.shp")))
S_area.shp <- terra::project(S_area.shp, "EPSG:4269")

# Set Resolution
res <- res(A_area.rast) # comment this out if you change the resolution
r <- rast(resolution=res,crs= "EPSG:4269",extent=ext(A_area.rast))

#################### LOAD FIELD DATA ###############################
# Load Sediment Raster
# Load Sediment Raster
sed_rast <- terra::rast(paste0(path.input,("Idw_Sediment.tif")))
sed_rast <- terra::project(sed_rast, "EPSG:4269")

# crop to squibnocket because data only covers squibnocket
sed_rast_cropped <- terra::mask(sed_rast, S_area.shp)

# Bathymetry
bath_rast <- terra::rast(paste0(path.input,('Idw_Aquinnah3.tif')))
bath_rast <- terra::project(bath_rast, "EPSG:4269")

# Salinity
salinity_rast <- terra::rast(paste0(path.input, "Idw_Salinity_4.tif"))
salinity_rast <- terra::project(salinity_rast, "EPSG:4269")

# Velocity
velocity_rast <- terra::rast(paste0(path.input, "Idw_FlowTest_1.tif"))
velocity_rast <- terra::project(velocity_rast, "EPSG:4269")

# Temperature
temperature_rast <- terra::rast(paste0(path.input, "Idw_TempTest.tif"))
temperature_rast <- terra::project(temperature_rast, "EPSG:4269")

# SAV
SAV_rast <- terra::rast(paste0(path.input, "Reclass_10p32_1.tif"))
SAV_rast <- terra::project(SAV_rast, "EPSG:4269")

####################################################################
# Resample rasters to match the resolution of raster_mask without interpolation

# Sediment
sed_rast_resampled <- resample(sed_rast_cropped, r)

# Bathymetry
bath_rast_resampled <- resample(bath_rast,r)

# Salinity
salinity_rast_resampled <- resample(salinity_rast, r)

# Velocity
velocity_rast_resampled <- resample(velocity_rast, r)
# convert cm/s to m/s
velocity_rast_resampled <- abs(velocity_rast_resampled / 100)

# Temperature
temperature_rast_resampled <- resample(temperature_rast, r)

# Sub-Aquatic Vegetation
SAV_rast_resampled <- resample(SAV_rast, r)
# Reclassify values
# Define the classification matrix
classification_matrix <- matrix(c(0.5, Inf, 1), ncol = 3)

# Reclassify values
SAV_rast_reclassified <- classify(SAV_rast_resampled, classification_matrix)

# If you want to include values less than 0.5 as 0, you can add an 'others' argument
SAV_rast_reclassified <- classify(SAV_rast_resampled, classification_matrix, others = 0)

ui <- dashboardPage(
  dashboardHeader(title = "River Herring Habitat Model Application: Aquinnah, Massachusetts"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Area", tabName = "project_area", icon = icon("home")),
      menuItem("Input Data", tabName = "input_data", icon = icon("th")),
      menuItem("Alewife Suitability Indices", tabName = "suitability", icon = icon("fish"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "project_area",
              fluidRow(
                box(title = "Herring Creek Fishery", status = "info", solidHeader = TRUE,
                    leafletOutput("map", height = 600)),
                box(title = "Area/Project Description", status = "info", solidHeader = TRUE,
                    HTML("<p>Insert your description here.</p>"))
              )
      ),
      tabItem(tabName = "input_data",
              fluidRow(
                column(width = 3,
                       selectInput("parameter", "Select Parameter", choices = c("Average Daily Temperature (C)", "Depth (m)", "Salinity (psu)", "Average Daily Flow Velocity (m/s)", "Substrate Classification", "Sub-Aquatic Vegetation: Presence/Absence (1,0)"))),
                column(width = 6,
                       box(status = "info", solidHeader = FALSE,
                           leafletOutput("input_map", height = 600, width = 600)))
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Input Data Description", status = "info", solidHeader = TRUE,
                           textOutput("data_description")))  # Output for data description
              )
      ),
      tabItem(tabName = "suitability",
              h2("Suitability Indices for Alewives"),
              h3("Temperature"),
              fluidRow(
                box(title = "Spawning Adults", status = "info", solidHeader = TRUE,
                    plotOutput("spawning_temp_plot", height = 250)),
                box(title = "Eggs & Larvae", status = "info", solidHeader = TRUE,
                    plotOutput("eggs_temp_plot", height = 250)),
                box(title = "Non-Migratory Juveniles", status = "info", solidHeader = TRUE,
                    plotOutput("juveniles_temp_plot", height = 250))
              ),
              h3("Depth"),
              fluidRow(
                box(title = "Spawning Adults", status = "info", solidHeader = TRUE,
                    plotOutput("spawning_depth_plot", height = 250)),
                box(title = "Eggs & Larvae", status = "info", solidHeader = TRUE,
                    plotOutput("eggs_depth_plot", height = 250)),
                box(title = "Non-Migratory Juveniles", status = "info", solidHeader = TRUE,
                    plotOutput("juveniles_depth_plot", height = 250))
              ),
              h3("Salinity"),
              fluidRow(
                box(title = "Spawning Adults", status = "info", solidHeader = TRUE,
                    plotOutput("spawning_salinity_plot", height = 250)),
                box(title = "Eggs & Larvae", status = "info", solidHeader = TRUE,
                    plotOutput("eggs_salinity_plot", height = 250)),
                box(title = "Non-Migratory Juveniles", status = "info", solidHeader = TRUE,
                    plotOutput("juveniles_salinity_plot", height = 250))
              ),
              h3("Flow Velocity"),
              fluidRow(
                box(title = "Spawning Adults", status = "info", solidHeader = TRUE,
                    plotOutput("spawning_velocity_plot", height = 250)),
                box(title = "Eggs & Larvae", status = "info", solidHeader = TRUE,
                    plotOutput("eggs_velocity_plot", height = 250)),
                box(title = "Non-Migratory Juveniles", status = "info", solidHeader = TRUE,
                    plotOutput("juveniles_velocity_plot", height = 250))
              ),
              h3("Substrate"),
              fluidRow(
                box(title = "Spawning Adults", status = "info", solidHeader = TRUE,
                    plotOutput("spawning_substrate_plot", height = 250)),
                box(title = "Eggs & Larvae", status = "info", solidHeader = TRUE,
                    plotOutput("eggs_substrate_plot", height = 250)),
                box(title = "Non-Migratory Juveniles", status = "info", solidHeader = TRUE,
                    plotOutput("juveniles_substrate_plot", height = 250))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Project Area
  output$map <- renderLeaflet({
    # Example map centered on Aquinnah, Massachusetts
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%  # Add satellite imagery
      setView(lng = -70.7916, lat = 41.3328, zoom = 13) %>%  # Adjust the coordinates and zoom level as needed
      addMarkers(lng = -70.785763, lat = 41.330420, popup = "Herring Creek Fishery")  # Add marker with popup
  })
  
  # Input Data
  observeEvent(input$parameter, {
    
    # Get the selected parameter
    selected_parameter <- input$parameter
    
    # Fetch corresponding raster data based on the selected parameter
    raster_layer <- switch(selected_parameter,
                           "Average Daily Temperature (C)" = temperature_rast_resampled,
                           "Depth (m)" = bath_rast_resampled,
                           "Salinity (psu)" = salinity_rast_resampled,
                           "Average Daily Flow Velocity (m/s)" = velocity_rast_resampled,
                           "Substrate Classification" = sed_rast_resampled,
                           "Sub-Aquatic Vegetation: Presence/Absence (1,0)" = SAV_rast_reclassified)
    
    # Calculate the minimum and maximum values of the raster data
    min_value <- min(values(raster_layer), na.rm = TRUE)
    max_value <- max(values(raster_layer), na.rm = TRUE)
    
    # Add the raster layer to the map on the "Input Data" tab
    output$input_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%  # Add satellite imagery
        setView(lng = -70.7916, lat = 41.3328, zoom = 12) %>%  # Adjust the coordinates and zoom level as needed
        addRasterImage(raster_layer) %>%
        addLegend("bottomright", colors = viridis(10), labels = seq(min_value, max_value, length.out = 10))
    })
    
    # Output parameter information (placeholder)
    output$parameter_info <- renderText({
      paste("You selected:", selected_parameter)
      # Add more information about the selected parameter here if needed
    })
  })
  # Example data for the suitability indices
  suitability_index <- function(x, shift = 0) { 1 / (1 + exp(-x + 5 + shift)) }
  x <- seq(0, 10, length.out = 100)
  
  # Generate example plots for each parameter and category
  # Temperature
  output$spawning_temp_plot <- renderPlot({
    y <- suitability_index(x)
    plot(x, y, type = "l", col = "blue", lwd = 2, main = "Spawning Adults", xlab = "Temperature", ylab = "Suitability Index")
  })
  
  output$eggs_temp_plot <- renderPlot({
    y <- suitability_index(x, shift = 1)
    plot(x, y, type = "l", col = "green", lwd = 2, main = "Eggs & Larvae", xlab = "Temperature", ylab = "Suitability Index")
  })
  
  output$juveniles_temp_plot <- renderPlot({
    y <- suitability_index(x, shift = -1)
    plot(x, y, type = "l", col = "red", lwd = 2, main = "Non-Migratory Juveniles", xlab = "Temperature", ylab = "Suitability Index")
  })
  
  # Depth
  output$spawning_depth_plot <- renderPlot({
    y <- suitability_index(x / 2)
    plot(x, y, type = "l", col = "blue", lwd = 2, main = "Spawning Adults", xlab = "Depth", ylab = "Suitability Index")
  })
  
  output$eggs_depth_plot <- renderPlot({
    y <- suitability_index(x / 2, shift = 1)
    plot(x, y, type = "l", col = "green", lwd = 2, main = "Eggs & Larvae", xlab = "Depth", ylab = "Suitability Index")
  })
  
  output$juveniles_depth_plot <- renderPlot({
    y <- suitability_index(x / 2, shift = -1)
    plot(x, y, type = "l", col = "red", lwd = 2, main = "Non-Migratory Juveniles", xlab = "Depth", ylab = "Suitability Index")
  })
  
  # Salinity
  output$spawning_salinity_plot <- renderPlot({
    y <- suitability_index(x + 1)
    plot(x, y, type = "l", col = "blue", lwd = 2, main = "Spawning Adults", xlab = "Salinity", ylab = "Suitability Index")
  })
  
  output$eggs_salinity_plot <- renderPlot({
    y <- suitability_index(x + 2, shift = 1)
    plot(x, y, type = "l", col = "green", lwd = 2, main = "Eggs & Larvae", xlab = "Salinity", ylab = "Suitability Index")
  })
  
  output$juveniles_salinity_plot <- renderPlot({
    y <- suitability_index(x, shift = -1)
    plot(x, y, type = "l", col = "red", lwd = 2, main = "Non-Migratory Juveniles", xlab = "Salinity", ylab = "Suitability Index")
  })
  
  # Flow Velocity
  output$spawning_velocity_plot <- renderPlot({
    y <- suitability_index(x - 1)
    plot(x, y, type = "l", col = "blue", lwd = 2, main = "Spawning Adults", xlab = "Flow Velocity", ylab = "Suitability Index")
  })
  
  output$eggs_velocity_plot <- renderPlot({
    y <- suitability_index(x - 2, shift = 1)
    plot(x, y, type = "l", col = "green", lwd = 2, main = "Eggs & Larvae", xlab = "Flow Velocity", ylab = "Suitability Index")
  })
  
  output$juveniles_velocity_plot <- renderPlot({
    y <- suitability_index(x - 1, shift = -1)
    plot(x, y, type = "l", col = "red", lwd = 2, main = "Non-Migratory Juveniles", xlab = "Flow Velocity", ylab = "Suitability Index")
  })
  
  # Substrate
  output$spawning_substrate_plot <- renderPlot({
    y <- suitability_index(x / 2 + 1)
    plot(x, y, type = "l", col = "blue", lwd = 2, main = "Spawning Adults", xlab = "Substrate", ylab = "Suitability Index")
  })
  
  output$eggs_substrate_plot <- renderPlot({
    y <- suitability_index(x / 2 + 2, shift = 1)
    plot(x, y, type = "l", col = "green", lwd = 2, main = "Eggs & Larvae", xlab = "Substrate", ylab = "Suitability Index")
  })
  
  output$juveniles_substrate_plot <- renderPlot({
    y <- suitability_index(x / 2, shift = -1)
    plot(x, y, type = "l", col = "red", lwd = 2, main = "Non-Migratory Juveniles", xlab = "Substrate", ylab = "Suitability Index")
  })

}

shinyApp(ui = ui, server = server)
