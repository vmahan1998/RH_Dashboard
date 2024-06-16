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
#bath_rast <- terra::mask(bath_rast, A_area.rast)

# Salinity
salinity_rast <- terra::rast(paste0(path.input, "Idw_Salinity_4.tif"))
salinity_rast <- terra::project(salinity_rast, "EPSG:4269")
#salinity_rast <- terra::mask(salinity_rast, S_area.shp)

# Velocity
velocity_rast <- terra::rast(paste0(path.input, "Idw_FlowTest_1.tif"))
velocity_rast <- terra::project(velocity_rast, "EPSG:4269")
#velocity_rast <- terra::mask(velocity_rast, A_area.rast)

# Temperature
temperature_rast <- terra::rast(paste0(path.input, "Idw_TempTest.tif"))
temperature_rast <- terra::project(temperature_rast, "EPSG:4269")
#temperature_rast <- terra::mask(temperature_rast, S_area.shp)

# SAV
SAV_rast <- terra::rast(paste0(path.input, "Reclass_10p32_1.tif"))
SAV_rast <- terra::project(SAV_rast, "EPSG:4269")
#SAV_rast <- terra::mask(SAV_rast, S_area.shp)

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

ui <- fluidPage(
  titlePanel("River Herring Ecological Modeling"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Homepage", 
                           h2("Welcome to the River Herring Project"),
                           p("This app is designed to visualize and analyze data on river herring populations in Aquinnah, MA. This project was completed in collaboration with the Wampanoag Tribe and all results and data contained within this app remain property of the Tribe. Explicit permission must be granted for reuse."),
                           p("Data and information in this app are based on the following reports:"),
                           tags$ul(
                             tags$li("River Herring Habitat Model Report 2024"),
                             tags$li("Incorporating Traditional Ecological Knowledge (TEK) into Ecological Modeling")
                           ),
                           p("River herring are anadromous fish species, including alewives and blueback herring, that migrate from the ocean to freshwater rivers and streams to spawn.")
                  ),
                  tabPanel("Species Information", 
                           h2("Species Information"),
                           p("Detailed information about the river herring species, including alewives and blueback herring."),
                           h3("Alewives"),
                           p("Alewives (Alosa pseudoharengus) are a species of anadromous fish in the herring family."),
                           tags$ul(
                             tags$li("Description"),
                             tags$li("Habitat"),
                             tags$li("Life Cycle")
                           ),
                           h3("Blueback Herring"),
                           p("Blueback herring (Alosa aestivalis) are similar to alewives but have distinct ecological and behavioral differences."),
                           tags$ul(
                             tags$li("Description"),
                             tags$li("Habitat"),
                             tags$li("Life Cycle")
                           )
                  ),
                  tabPanel("Project Description", 
                           h2("Project Area and Background"),
                           p("This project focuses on studying the river herring populations in the designated project area."),
                           leafletOutput("map_project_area")
                  ),
                  tabPanel("Input Data",
                           h2("Input Data"),
                           p("Description of the input data used in the project."),
                           tableOutput("input_data_table"),
                           selectInput("parameter", "Select Parameter:", 
                                       choices = c("Average Daily Temperature (C)",
                                                   "Depth (m)",
                                                   "Salinity (psu)",
                                                   "Average Daily Flow Velocity (m/s)",
                                                   "Substrate Classification",
                                                   "Sub-Aquatic Vegetation: Presence/Absence (1,0)")),
                           leafletOutput("input_map"),
                           textOutput("parameter_info")
                  )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Spawning Adult Alewives",
                 h2("Spawning Adult Alewives"),
                 tabsetPanel(
                   tabPanel("Habitat Results"),
                   tabPanel("Average Daily Temperature (C)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_adult_temp_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth (m)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_adult_depth_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity (psu)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_adult_salinity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity (m/s)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_adult_velocity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate Classification",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_adult_substrate_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation: Presence/Absence (1,0)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_adult_sav_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_sav_habitat")
                              )
                            )
                   )
                 )
        ),
        tabPanel("Alewife Eggs & Larvae",
                 h2("Alewife Eggs & Larvae"),
                 tabsetPanel(
                   tabPanel("Habitat Results"),
                   tabPanel("Average Daily Temperature (C)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_eggs_temp_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth (m)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_eggs_depth_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity (psu)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_eggs_salinity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity (m/s)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_eggs_velocity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate Classification",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_eggs_substrate_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation: Presence/Absence (1,0)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_eggs_sav_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_sav_habitat")
                              )
                            )
                   )
                 )
        ),
        tabPanel("Non-Migratory Juvenile Alewives",
                 h2("Non-Migratory Juvenile Alewives"),
                 tabsetPanel(
                   tabPanel("Habitat Results"),
                   tabPanel("Average Daily Temperature (C)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_juvenile_temp_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth (m)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_juvenile_depth_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity (psu)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_juvenile_salinity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity (m/s)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_juvenile_velocity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate Classification",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_juvenile_substrate_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation: Presence/Absence (1,0)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_alewives_juvenile_sav_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_sav_habitat")
                              )
                            )
                   )
                 )
        ),
        tabPanel("Spawning Adult Blueback Herring",
                 h2("Spawning Adult Blueback Herring"),
                 tabsetPanel(
                   tabPanel("Habitat Results"),
                   tabPanel("Average Daily Temperature (C)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_adult_temp_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth (m)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_adult_depth_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity (psu)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_adult_salinity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity (m/s)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_adult_velocity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate Classification",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_adult_substrate_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation: Presence/Absence (1,0)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_adult_sav_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_sav_habitat")
                              )
                            )
                   )
                 )
        ),
        tabPanel("Blueback Herring Eggs & Larvae",
                 h2("Blueback Herring Eggs & Larvae"),
                 tabsetPanel(
                   tabPanel("Habitat Results"),
                   tabPanel("Average Daily Temperature (C)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_eggs_temp_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth (m)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_eggs_depth_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity (psu)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_eggs_salinity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity (m/s)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_eggs_velocity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate Classification",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_eggs_substrate_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation: Presence/Absence (1,0)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_eggs_sav_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_sav_habitat")
                              )
                            )
                   )
                 )
        ),
        tabPanel("Non-Migratory Juvenile Blueback Herring",
                 h2("Non-Migratory Juvenile Blueback Herring"),
                 tabsetPanel(
                   tabPanel("Habitat Results"),
                   tabPanel("Average Daily Temperature (C)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_juvenile_temp_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth (m)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_juvenile_depth_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity (psu)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_juvenile_salinity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity (m/s)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_juvenile_velocity_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate Classification",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_juvenile_substrate_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation: Presence/Absence (1,0)",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotOutput("plot_blueback_juvenile_sav_suitability")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_sav_habitat")
                              )
                            )
                   )
                 )
        ),
        tabPanel("River Herring Migration Model", 
                 h2("River Herring Migration Model"),
                 fluidRow(
                   column(6, 
                          h3("Baseline Simulation (No Predation)"),
                          plotOutput("plot_migration_baseline")
                   ),
                   column(6, 
                          h3("Description of Model Application"),
                          p("This section provides an overview of the River Herring Migration Model application. The model simulates the migration patterns of river herring under various environmental conditions and management scenarios."),
                          p("The baseline simulation assumes no predation and provides a reference point for comparing the impacts of different factors on migration success. Users can explore how changes in environmental variables, such as temperature, salinity, and flow velocity, affect the migration routes and success rates of river herring."),
                          p("By adjusting model parameters and running simulations, users can gain insights into the potential outcomes of different management strategies and environmental changes, helping to inform conservation and management decisions.")
                   )
                 ),
                 fluidRow(
                   column(6, 
                          h3("Low Predation"),
                          plotOutput("plot_blueback_juvenile_sav_suitability")
                   ),
                   column(6, 
                          h3("Low Predation"),
                          plotOutput("plot_blueback_juvenile_sav_habitat")
                   )
                 ),
                 fluidRow(
                   column(12,
                          h3("Summary of Results"),
                          p("This section provides a summary of the simulation results. It includes key findings and insights derived from the model simulations, highlighting the most significant factors affecting river herring migration success.")
                   )
                 ))
      )
    )
  )
)

server <- function(input, output, session) {
  # Sample data
  data <- reactive({
    # Replace this with actual data loading
    data.frame(date = seq.Date(from = as.Date("2020-01-01"), to = Sys.Date(), by = "month"),
               population_alewives_adult = runif(60, 1000, 5000),
               population_alewives_eggs = runif(60, 5000, 20000),
               population_alewives_juvenile = runif(60, 3000, 15000),
               population_blueback_adult = runif(60, 1000, 5000),
               population_blueback_eggs = runif(60, 5000, 20000),
               population_blueback_juvenile = runif(60, 3000, 15000))
  })
  
  # Sample input data
  input_data <- data.frame(
    Variable = c("Date", "Population Alewives Adult", "Population Alewives Eggs", "Population Alewives Juvenile", "Population Blueback Adult", "Population Blueback Eggs", "Population Blueback Juvenile"),
    Description = c("Date of observation", "Number of spawning adult alewives", "Number of alewife eggs and larvae", "Number of non-migratory juvenile alewives", "Number of spawning adult blueback herring", "Number of blueback herring eggs and larvae", "Number of non-migratory juvenile blueback herring")
  )
  
  output$input_data_table <- renderTable({
    input_data
  })
  
  output$map_project_area <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -70.7916, lat = 41.3328, zoom = 13) %>%
      addMarkers(lng = -70.785763, lat = 41.330420, popup = "Herring Creek Fishery")
  })
  
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
        addProviderTiles("Esri.WorldImagery") %>%
        setView(lng = -70.7916, lat = 41.3328, zoom = 12) %>%
        addRasterImage(raster_layer, colors = viridis(10)) %>%
        addLegend("bottomright", colors = viridis(10), labels = seq(min_value, max_value, length.out = 10))
    })
    
    output$parameter_info <- renderText({
      paste("You selected:", selected_parameter)
    })
  })
  
}

shinyApp(ui = ui, server = server)