#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list=ls(all=TRUE))
# Load packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(terra)
library(viridis)
library(plotly)
library(shinythemes)
library(ggiraph)
library(dplyr)
library(ggmap)
library(leaflet.extras)
library(DT)

path.input <- '/Users/RDEL1VMM/Desktop/current projects/Aquinnah Herring Hatchery/Model_results_5_7_24/'
path.output <- '/Users/RDEL1VMM/Documents/Netlogo/Herring_Squibnocket/AquinnahHerringMigrationModel/raw_data_processing/'
migration.model.input <- "/Users/RDEL1VMM/Documents/Netlogo/Herring_Squibnocket/AquinnahHerringMigrationModel/output"
# Fetch corresponding raster data based on the selected parameter
## Preprocessing
# Set up your Google API key
api_key <- "AIzaSyBXMCgi4IjTws4noaeAl7XvgFkETFgH4Zw"

# Register your Google API key
register_google(key = api_key)

# Load Raster of area
A_area.rast <- terra::rast(paste0(path.input,("A_area_mask_10m.tif")))
A_area.rast <- terra::project(A_area.rast, "EPSG:4269")

# Load Shapefile of area
A_area.shp <- terra::vect(paste0(path.input,("Aquinnah_Proje_ExportFeature.shp")))
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
bath_rast <- terra::mask(bath_rast, A_area.shp)

# Salinity
salinity_rast <- terra::rast(paste0(path.input, "Idw_Salinity_4.tif"))
salinity_rast <- terra::project(salinity_rast, "EPSG:4269")
salinity_rast <- terra::mask(salinity_rast, A_area.shp)

# Velocity
velocity_rast <- terra::rast(paste0(path.input, "Idw_FlowTest_1.tif"))
velocity_rast <- terra::project(velocity_rast, "EPSG:4269")
velocity_rast <- terra::mask(velocity_rast, A_area.shp)

# Temperature
temperature_rast <- terra::rast(paste0(path.input, "Idw_TempTest.tif"))
temperature_rast <- terra::project(temperature_rast, "EPSG:4269")
temperature_rast <- terra::mask(temperature_rast, A_area.shp)

# SAV
SAV_rast <- terra::rast(paste0(path.input, "Reclass_10p32_1.tif"))
SAV_rast <- terra::project(SAV_rast, "EPSG:4269")
SAV_rast <- terra::mask(SAV_rast, A_area.shp)

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
classification_matrix <- matrix(c(0, Inf, 1), ncol = 3)

# Reclassify values
SAV_rast_reclassified <- classify(SAV_rast_resampled, classification_matrix)

# If you want to include values less than 0.5 as 0, you can add an 'others' argument
SAV_rast_reclassified <- classify(SAV_rast_resampled, classification_matrix, others = 0)

# Load Habitat Model Results
adult_alewife <- terra::rast(paste0(path.input,"adult_alewife_habitat_data.tif"))
egg_alewife <- terra::rast(paste0(path.input,"egg_larvae_alewife_habitat_data.tif"))
juv_alewife <- terra::rast(paste0(path.input,"nonmigratory_juvenile_alewife_habitat_data.tif"))

adult_blueback <- terra::rast(paste0(path.input,"adult_blueback_habitat_data.tif"))
egg_blueback <- terra::rast(paste0(path.input,"egg_larvae_blueback_habitat_data.tif"))
juv_blueback <- terra::rast(paste0(path.input,"nonmigratory_juvenile_blueback_habitat_data.tif"))

# Suitability Indices
# Adult Alewife
adult_alewife_temp_suitability_data <- data.frame(
  Temperature = c(0, 3, 7, 11, 16, 28, 30, 35),  # Add a higher value for plotting purposes
  SuitabilityIndex = c(0.0, 0.1, 0.3, 0.5, 1.0, 0.1, 0.0, 0.0)  # Ensure the length matches Temperature
)
adult_alewife_depth_suitability_data <- data.frame(
  Depth = c(-3, 0, 2, 5, 10, 15),
  SuitabilityIndex = c(0.8, 1.0, 0.8, 0.1, 0.0, 0.0)
)

adult_alewife_salinity_suitability_data <- data.frame(
  Salinity = c(0, 0.5, 12.0, 15.0, 20.0, 25.0),
  SuitabilityIndex = c(0.8, 1.0, 0.75, 0.7, 0.65, 0.65)
)

adult_alewife_velocity_suitability_data <- data.frame(
  Velocity = c(0, 0.03, 0.12, 0.3, 3.5, 4.5, 5.0),
  SuitabilityIndex = c(0.7, 1.0, 0.5, 0.3, 0.1, 0.0, 0.0)
)

adult_alewife_substrate_suitability_data <- data.frame(
  Substrate = c("Hard Substrate", "Soft Substrate", "Peat Substrate"),
  SuitabilityIndex = c(0.5, 1.0, 0.0)
)

adult_alewife_SAV_suitability_data <- data.frame(
  SAV = c("Present SAV", "Absent SAV"),
  SuitabilityIndex = c(1.0, 0.5)
)

# Alewife Eggs & Larvae
alewife_eggs_temp_suitability_data <- data.frame(
  Temperature = c(0, 3, 7, 11, 20, 23, 28, 30),  # Add a higher value for plotting purposes
  SuitabilityIndex = c(0.0, 0.1, 0.5, 0.8, 1.0, 0.5, 0.1, 0.1)  # Ensure the length matches Temperature
)

alewife_eggs_depth_suitability_data <- data.frame(
  Depth = c(-3, 0, 5.0, 10.0, 20.0, 25.0),
  SuitabilityIndex = c(0.5, 1.0, 0.7, 0.5, 0.0, 0.0)
)

alewife_eggs_salinity_suitability_data <- data.frame(
  Salinity = c(0, 0.5, 10.0, 25.0, 30.0),
  SuitabilityIndex = c(0.0, 0.5, 1.0, 0.8, 0.8)
)

alewife_eggs_velocity_suitability_data <- data.frame(
  Velocity = c(0, 0.1, 0.17, 0.3, 3.5, 4.5, 5.0),
  SuitabilityIndex = c(1.0, 0.7, 0.5, 0.3, 0.1, 0.0, 0.0)
)

alewife_eggs_substrate_suitability_data <- data.frame(
  Substrate = c("Hard Substrate", "Soft Substrate", "Peat Substrate"),
  SuitabilityIndex = c(1.0, 0.3, 0.0)
)

alewife_eggs_SAV_suitability_data <- data.frame(
  SAV = c("Present SAV", "Absent SAV"),
  SuitabilityIndex = c(1.0, 0.5)
)

# Juvenile ALewives
juvenile_alewife_temp_suitability_data <- data.frame(
  Temperature = c(0, 3, 7, 11, 20, 23, 28, 30),  # Add a higher value for plotting purposes
  SuitabilityIndex = c(0.0, 0.1, 0.5, 0.8, 1.0, 0.5, 0.1, 0.1)  # Ensure the length matches Temperature
)

juvenile_alewife_depth_suitability_data <- data.frame(
  Depth = c(-3, 0, 5.0, 10.0, 20.0, 25.0),
  SuitabilityIndex = c(0.5, 1.0, 0.7, 0.5, 0.0, 0.0)
)

juvenile_alewife_salinity_suitability_data <- data.frame(
  Salinity = c(0, 0.5, 10.0, 25.0, 30.0),
  SuitabilityIndex = c(0.0, 0.5, 1.0, 0.8, 0.8)
)

juvenile_alewife_velocity_suitability_data <- data.frame(
  Velocity = c(0, 0.1, 0.17, 0.3, 3.5, 4.5, 5.0),
  SuitabilityIndex = c(1.0, 0.7, 0.5, 0.3, 0.1, 0.0, 0.0)
)

juvenile_alewife_substrate_suitability_data <- data.frame(
  Substrate = c("Hard Substrate", "Soft Substrate", "Peat Substrate"),
  SuitabilityIndex = c(1.0, 0.3, 0.0)
)

juvenile_alewife_SAV_suitability_data <- data.frame(
  SAV = c("Present SAV", "Absent SAV"),
  SuitabilityIndex = c(1.0, 0.5)
)

# Blueback Herring
# Adult Blueback Herring
blueback_adult_temp_suitability_data <- data.frame(
  Temperature = c(0, 3, 10, 13, 17, 20, 25, 27, 30),  # Add a higher value for plotting purposes
  SuitabilityIndex = c(0.0, 0.3, 0.5, 0.7, 1.0, 0.5, 0.3, 0.0, 0.0)  # Ensure the length matches Temperature
)
blueback_adult_depth_suitability_data <- data.frame(
  Depth = c(-3, 0, 2.0, 5.0, 10.0, 15.0, 20.0, 25.0),
  SuitabilityIndex = c(1.0, 1.0, 0.8, 0.5, 0.3, 0.1, 0.0, 0.0)
)
blueback_adult_salinity_suitability_data <- data.frame(
  Salinity = c(0, 8.0, 15.0, 20.0, 25.0),
  SuitabilityIndex = c(1.0, 0.8, 0.5, 0.0, 0.0)
)
blueback_adult_velocity_suitability_data <- data.frame(
  Velocity = c(0, 0.1, 0.5, 1.7, 3.5, 4.5, 5.0),
  SuitabilityIndex = c(0.3, 1.0, 0.8, 0.5, 0.1, 0.0, 0.0)
)
blueback_adult_substrate_suitability_data <- data.frame(
  Substrate = c("Hard Substrate", "Soft Substrate", "Peat Substrate"),
  SuitabilityIndex = c(1.0, 0.1, 0.0)
)
blueback_adult_SAV_suitability_data <- data.frame(
  SAV = c("Present SAV", "Absent SAV"),
  SuitabilityIndex = c(1.0, 0.1)
)
# Suitability Indices for Blueback Herring Eggs and Larvae
blueback_eggs_temp_suitability_data <- data.frame(
  Temperature = c(0, 3, 10, 13, 19, 24, 26, 34, 35),  # Add a higher value for plotting purposes
  SuitabilityIndex = c(0.0, 0.3, 0.5, 1.0, 0.7, 0.5, 0.1, 0.0, 0.0)  # Ensure the length matches Temperature
)
blueback_eggs_depth_suitability_data <- data.frame(
  Depth = c(-3, 0.8, 2, 5, 10, 15),
  SuitabilityIndex = c(0.5, 1.0, 0.3, 0.1, 0.0, 0.0)
)
blueback_eggs_salinity_suitability_data <- data.frame(
  Salinity = c(0, 0.5, 12.0, 15.0, 20.0, 25.0),
  SuitabilityIndex = c(1.0, 0.8, 0.8, 0.8, 0.75, 0.75)
)
blueback_eggs_velocity_suitability_data <- data.frame(
  Velocity = c(0, 0.1, 0.5, 1.0, 2.0, 3.0, 4.5, 5.0),
  SuitabilityIndex = c(0.3, 0.5, 0.7, 1.0, 0.5, 0.1, 0.0, 0.0)
)
blueback_eggs_substrate_suitability_data <- data.frame(
  Substrate = c("Hard Substrate", "Soft Substrate", "Peat Substrate"),
  SuitabilityIndex = c(0.1, 1.0, 0.0)
)
blueback_eggs_SAV_suitability_data <- data.frame(
  SAV = c("Present SAV", "Absent SAV"),
  SuitabilityIndex = c( 1.0, 0.1)
)

# Juvenile Blueback Herring
juvenile_blueback_temp_suitability_data <- data.frame(
  Temperature = c(0, 3, 7, 11, 21, 30, 33, 35),  # Add a higher value for plotting purposes
  SuitabilityIndex = c(0.0, 0.1, 0.5, 0.8, 1.0, 0.3, 0.0, 0.0)  # Ensure the length matches Temperature
)
juvenile_blueback_depth_suitability_data <- data.frame(
  Depth = c(-3, 0, 5.0, 10.0, 20.0, 25.0),
  SuitabilityIndex = c(0.3, 1.0, 0.7, 0.5, 0.0, 0.0)
)
juvenile_blueback_salinity_suitability_data <- data.frame(
  Salinity = c(0, 0.5, 10.0, 25.0, 30.0),
  SuitabilityIndex = c(0.5, 0.5, 1.0, 1.0, 1.0)
)
juvenile_blueback_velocity_suitability_data <- data.frame(
  Velocity = c(0, 0.1, 0.17, 0.3, 3.5, 4.5, 5.0),
  SuitabilityIndex = c(1.0, 0.7, 0.5, 0.3, 0.1, 0.0, 0.0)
)
juvenile_blueback_substrate_suitability_data <- data.frame(
  Substrate = c("Hard Substrate", "Soft Substrate", "Peat Substrate"),
  SuitabilityIndex = c(1.0, 0.1, 0.0)
)
juvenile_blueback_SAV_suitability_data <- data.frame(
  SAV = c("Present SAV", "Absent SAV"),
  SuitabilityIndex = c(1.0, 0.5)
)

##########################################################################
# Migration Model Results
# Load Model Results
adult_alewife_data <- read.csv(paste0(path.input,("adult_alewife_habitat_data.csv"))) #HSI
adult_alewife_patch_data <- read.csv(paste0(path.output,("adult_alewife_baseline_results_with_lat_long.csv")))
adult_alewife_patch_data_low <- read.csv(paste0(path.output,("adult_alewife_low_results_with_lat_long.csv")))
adult_alewife_patch_data_mod <- read.csv(paste0(path.output,("adult_alewife_moderate_results_with_lat_long.csv")))
adult_alewife_patch_data_high <- read.csv(paste0(path.output,("adult_alewife_high_results_with_lat_long.csv")))

# Combine all datasets to find the global min and max for scaling
all_data <- bind_rows(adult_alewife_data, adult_alewife_patch_data_low, adult_alewife_patch_data_mod, adult_alewife_patch_data_high)

global_spawning_min <- min(all_data$spawning.events.in.patch)
global_spawning_max <- max(all_data$spawning.events.in.patch)
global_predation_min <- min(all_data$prey.eaten.in.patch)
global_predation_max <- max(all_data$prey.eaten.in.patch)

######################################3
filtered_baseline_spawning_data <- adult_alewife_patch_data %>% filter(spawning.events.in.patch > 0)

# Low Predation
filtered_low_spawning_data <- adult_alewife_patch_data_low %>% filter(spawning.events.in.patch > 0)
filtered_low_predation_data <- adult_alewife_patch_data_low %>% filter( prey.eaten.in.patch > 0)

# Moderate Predation
filtered_mod_spawning_data <- adult_alewife_patch_data_mod %>% filter(spawning.events.in.patch > 0)
filtered_mod_predation_data <- adult_alewife_patch_data_mod %>% filter( prey.eaten.in.patch > 0)

# High Predation
filtered_high_spawning_data <- adult_alewife_patch_data_high %>% filter(spawning.events.in.patch > 0)
filtered_high_predation_data <- adult_alewife_patch_data_high %>% filter( prey.eaten.in.patch > 0)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  tags$head(
    tags$style(HTML("
      .nav-tabs > li > a {
        background-color: #f0f8ff;
        color: #333;
        font-weight: bold;
      }
      .nav-tabs > li > a:hover, .nav-tabs > li > a:focus, .nav-tabs > li.active > a {
        background-color: #8fbc8f !important;
        color: white !important;
      }
      .tab-content {
        background-color: #f8f9fa;
        padding: 20px;
        border: 1px solid #ddd;
        border-top: none;
      }
      .panel-body {
        background-color: #e9f7ef;
        padding: 20px;
        border: 1px solid #ddd;
      }
      table {
        background-color: #ffffff;
        border: 1px solid #ddd;
      }
      th, td {
        padding: 10px;
        border: 1px solid #ddd;
      }
      h2, h3, p {
        color: #333;
      }
      body {
        background-color: #f5f5f5;
      }
      table.dataTable thead th {
      background-color: #E8F5E9;
      color: #333;
    }
    "))
  ),
  titlePanel("River Herring Ecological Modeling"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel(
                    title = tagList(icon("home"), "Homepage"), 
                    h2("Welcome to the River Herring Project"),
                    p("This app is designed to visualize and analyze data on river herring populations in Aquinnah, MA. This project was completed in collaboration with the Wampanoag Tribe and all results and data contained within this app remain property of the Tribe. Explicit permission must be granted for reuse."),
                    p("Data and information in this app are based on the following reports:"),
                    tags$ul(
                      tags$li("River Herring Habitat Model Report 2024"),
                      tags$li("Incorporating Traditional Ecological Knowledge (TEK) into Ecological Modeling")
                    ),
                    p("River herring are anadromous fish species, including alewives and blueback herring, that migrate from the ocean to freshwater rivers and streams to spawn.")
                  ),
                  tabPanel(
                    title = tagList(icon("fish"),"Species Information"), 
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
                  tabPanel(
                    title = tagList(icon("user"), "Project Description"), 
                    h2("Project Area and Background"),
                    p("This project focuses on studying the river herring populations in the designated project area."),
                    leafletOutput("map_project_area")
                  ),
                  tabPanel(
                    title = tagList(icon("cog"), "Ecological Modeling"), 
                    h2("Habitat Suitability Modeling"),
                    tags$div(style = "text-align:center;", 
                             tags$img(src = "Habitat_Conceptual_Model.png", style = "width: 100%; height: auto;")),
                    p("The habitat suitability model calculation"),
                    h2("Agent-Based Modeling"),
                    tags$div(style = "text-align:center;", 
                             tags$img(src = "Conceptual_Model_B_W.png", style = "width: 100%; height: auto;"))
                  ),
                  tabPanel(
                    title = tagList(icon("line-chart"), "Input Data"),
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
                  ),
                  tabPanel(
                    title = tagList(icon("book"), "Glossary"), 
                    h2("Glossary of Terms"),
                    p("This section provides definitions and explanations of key terms used in the River Herring Ecological Modeling Project."),
                    tags$ul(
                      tags$li(tags$b("Agent-based Model:"), " A class of computational models for simulating the actions and interactions of autonomous agents to assess their effects on the system."),
                      tags$li(tags$b("Agents:"), " Individual entities within an agent-based model that interact with each other and their environment according to predefined rules."),
                      tags$li(tags$b("Alewives:"), " A species of anadromous fish (Alosa pseudoharengus) that migrates from the ocean to freshwater rivers and streams to spawn."),
                      tags$li(tags$b("Anadromous Fish:"), " Fish that migrate from the sea to freshwater rivers and streams to spawn."),
                      tags$li(tags$b("Blueback Herring:"), " A species of anadromous fish (Alosa aestivalis) similar to alewives but with distinct ecological and behavioral differences."),
                      tags$li(tags$b("Diadromous Fish:"), " Fish that migrate between freshwater and saltwater during their life cycle, which includes anadromous and catadromous fish."),
                      tags$li(tags$b("Ecological Modeling:"), " The use of mathematical and computational techniques to represent and study the interactions within ecosystems."),
                      tags$li(tags$b("Flow Velocity:"), " The speed of water movement in a river or stream, usually measured in meters per second (m/s)."),
                      tags$li(tags$b("Habitat Quality:"), " An assessment of the overall condition of a habitat, including its ability to support the species of interest."),
                      tags$li(tags$b("Habitat Suitability:"), " An assessment of the suitability of a specific area for supporting a particular species based on environmental variables."),
                      tags$li(tags$b("Habitat Suitability Model:"), " A model used to evaluate the appropriateness of habitat conditions for a particular species based on environmental factors."),
                      tags$li(tags$b("Hard Substrates:"), " Firm and stable bottom materials in a river or stream, such as rocks or gravel."),
                      tags$li(tags$b("MLLW (Mean Lower Low Water):"), " The average of the lower low water height of each tidal day observed over the National Tidal Datum Epoch."),
                      tags$li(tags$b("Patches:"), " Discrete areas within a model's environment where agents interact and experience different conditions."),
                      tags$li(tags$b("Peat:"), " A type of substrate made up of partially decomposed plant material, commonly found in wetlands."),
                      tags$li(tags$b("Predation:"), " The act of one organism hunting and consuming another organism for food."),
                      tags$li(tags$b("Salinity:"), " The concentration of salt in water, usually measured in practical salinity units (psu)."),
                      tags$li(tags$b("Soft Substrates:"), " Unconsolidated bottom materials in a river or stream, such as sand or silt."),
                      tags$li(tags$b("Spawning:"), " The process by which fish release eggs and sperm into the water for fertilization."),
                      tags$li(tags$b("Striped Bass:"), " A species of anadromous fish (Morone saxatilis) known for migrating between freshwater and saltwater, commonly found along the Atlantic coast."),
                      tags$li(tags$b("Sub-Aquatic Vegetation:"), " Aquatic plants that grow below the water's surface, providing habitat and food for various aquatic species."),
                      tags$li(tags$b("Substrate:"), " The type of bottom material in a river or stream, such as sand, gravel, or rock."),
                      tags$li(tags$b("Suitability Index:"), " A numerical scale used to evaluate the appropriateness of habitat conditions for a particular species."),
                      tags$li(tags$b("Temperature:"), " The warmth or coldness of water, usually measured in degrees Celsius (°C).")
                    )
                  )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Spawning Adult Alewives",
                 h2(""),
                 tabPanel(
                   title = tagList(icon("map-marker-alt"), "Habitat Model Results"), 
                   fluidRow(
                     column(6, 
                   h2("Net Habitat Suitability"),
                   leafletOutput("map_adult_alewife_habitat_results")))
                 ),
                 tabsetPanel(
                   tabPanel("Average Daily Temperature",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_alewife_temp_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_alewife_depth_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_alewife_salinity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_alewife_velocity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_adult_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_alewife_sediment_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("adult_alewife_substrate_plot")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_alewife_SAV_plot")
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
                 h2(""),
                 tabPanel(
                   title = tagList(icon("map-marker-alt"), "Habitat Model Results"), 
                   fluidRow(
                     column(6, 
                   h2("Net Habitat Suitability"),
                   leafletOutput("map_egg_larvae_alewife_habitat_results")))
                 ),
                 tabsetPanel(
                   tabPanel("Average Daily Temperature",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_alewife_temp_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_alewife_depth_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_alewife_salinity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_alewife_velocity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_alewife_sediment_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_eggs_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_alewife_SAV_plot")
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
                 h2(""),
                 tabPanel(
                   title = tagList(icon("map-marker-alt"), "Habitat Model Results"), 
                   fluidRow(
                     column(6, 
                   h2("Net Habitat Suitability"),
                   leafletOutput("map_juv_alewife_habitat_results")))
                 ),
                 tabsetPanel(
                   tabPanel("Average Daily Temperature",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_alewife_temp_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_alewife_depth_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_alewife_salinity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_alewife_velocity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_alewife_sediment_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_alewives_juvenile_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_alewife_SAV_plot")
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
                 h2(""),
                 tabPanel(
                   title = tagList(icon("map-marker-alt"), "Habitat Model Results"), 
                   fluidRow(
                     column(6, 
                   h2("Net Habitat Suitability"),
                   leafletOutput("map_adult_blueback_habitat_results")))
                 ),
                 tabsetPanel(
                   tabPanel("Average Daily Temperature",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_blueback_temp_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_blueback_depth_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_blueback_salinity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_blueback_velocity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_blueback_sediment_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_adult_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("adult_blueback_SAV_plot")
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
                 h2(""),
                 tabPanel(
                   title = tagList(icon("map-marker-alt"), "Habitat Model Results"), 
                   fluidRow(
                     column(6, 
                   h2("Net Habitat Suitability"),
                   leafletOutput("map_egg_larvae_blueback_habitat_results")))
                 ),
                 tabsetPanel(
                   tabPanel("Average Daily Temperature",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_blueback_temp_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_blueback_depth_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_blueback_salinity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_blueback_velocity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_blueback_sediment_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_eggs_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("eggs_blueback_SAV_plot")
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
                 h2(""),
                 tabPanel(
                   title = tagList(icon("map-marker-alt"), "Habitat Model Results"), 
                   fluidRow(
                     column(6, 
                   h2("Net Habitat Suitability"),
                   leafletOutput("map_juv_blueback_habitat_results")))
                 ),
                 tabsetPanel(
                   tabPanel("Average Daily Temperature",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_blueback_temp_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_temp_habitat")
                              )
                            )
                   ),
                   tabPanel("Depth",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_blueback_depth_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_depth_habitat")
                              )
                            )
                   ),
                   tabPanel("Salinity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_blueback_salinity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_salinity_habitat")
                              )
                            )
                   ),
                   tabPanel("Average Daily Flow Velocity",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_blueback_velocity_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_velocity_habitat")
                              )
                            )
                   ),
                   tabPanel("Substrate",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_blueback_sediment_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_substrate_habitat")
                              )
                            )
                   ),
                   tabPanel("Sub-Aquatic Vegetation",
                            fluidRow(
                              column(6, 
                                     h3("Suitability Index"),
                                     plotlyOutput("juv_blueback_SAV_plot")
                              ),
                              column(6, 
                                     h3("Habitat Suitability"),
                                     plotOutput("plot_blueback_juvenile_sav_habitat")
                              )
                            )
                   )
                 )
        ),
tabPanel(
  title = tagList(icon("star"), "River Herring Migration Model"), 
  h2(""),
  fluidRow(
    column(6, 
           div(style = "text-align:center", h2(tags$b(style = "color: #8fbc8f;", "River Herring Migration Model"))),
           h2(""),
           p("This section provides an overview of the River Herring Migration Model application to Aquinnah, MA. This section provides an overview of the River Herring Migration Model application. The Agent-Based-Model (ABM) simulates the migration patterns of river herring under various striped bass predation conditions. By analyzing the spatial and temporal dynamics of river herring spawning success under different predation pressures, this model can be used to identify locations and times where predation impacts river herring migration and spawning."),
           h2(""),
           p("The simulation in Aquinnah, MA tested three levels of increasing predation pressure against a baseline scenario with no predation, as shown in the table below."),
           div(style = "text-align:center", h4(tags$b(style = "color: #8fbc8f;", "Simulation Design"))),
           div(style = "text-align:center", DTOutput("predationTable")),
           h2("")
    ),
    column(6, 
           tags$iframe(width="600", height="600", src="Model_Demo_w_Bass.gif", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen= TRUE)
    )
  ),
  fluidRow(
    column(6, 
           leafletOutput("baselinePlot")
    ),
    column(6, 
           div(style = "text-align:center", h4(tags$b(style = "color: #8fbc8f;", "Baseline Spawning Encounters"))),
           h2(""),
           p("The baseline simulation assumes no predation and provides a reference point for comparing the impacts of different factors on migration success. Users can explore how changes in environmental variables, such as temperature, salinity, and flow velocity, affect the migration routes and success rates of river herring. By adjusting model parameters and running simulations, users can gain insights into the potential outcomes of different management strategies and environmental changes, helping to inform conservation and management decisions.")
    )
  ),
  tabsetPanel(
    tabPanel("Low Predation",
             fluidRow(
               column(6, 
                      h3("Fish Consumed"),
                      leafletOutput("lowPred_Plot")
               ),
               column(6, 
                      h3("Spawning Encounters"),
                      leafletOutput("lowPlot")
               )
             )
    ),
    tabPanel("Moderate Predation",
             fluidRow(
               column(6, 
                      h3("Fish Consumed"),
                      leafletOutput("modPred_Plot")
               ),
               column(6, 
                      h3("Spawning Encounters"),
                      leafletOutput("modPlot")
               )
             )
    ),
    tabPanel("High Predation",
             fluidRow(
               column(6, 
                      h3("Fish Consumed"),
                      leafletOutput("highPred_Plot")
               ),
               column(6, 
                      h3("Spawning Encounters"),
                      leafletOutput("highPlot")
               )
             )
    )
  ),
  fluidRow(
    column(12,
           div(style = "text-align:center", h4(tags$b(style = "color: #8fbc8f;", "Summary of Results"))),
           p("The model simulation is designed to test the impact of varying predation levels on migrating river herring in Aquinnah, MA. Specifically, the ABM evaluates if increased predation pressures by striped bass delay river herring migration and reduce spawning success by testing three levels of increasing predation pressure against a baseline scenario with no predation."),
           p("Low predation is defined as when the predator population is approximately 0.1% of the prey population. Moderate predation is when the predator population is around 0.5%, and high predation is defined at approximately 1.0% of the prey population. The default model parameters for the simulations are detailed in the appendix."),
           p("GIS-derived shape files define distinct habitat areas, including the sea, estuary, channel, and pond, guiding the spatial dynamics of the simulation. A salinity gradient, represented either by real measured data or randomized by a scaled gradient, enhances the visual portrayal of the aquatic environment. This gradient visually signifies varying salinity levels, serving an illustrative purpose without directly impacting the model’s functionality."),
           p("The habitat suitability data, a crucial component, incorporates Habitat Suitability Index (HSI) model results from the River Herring Habitat Model Report 2024. This data defines spawning habitat quality for river herring across patches on a scale of 0-1 using real environmental conditions like depth, substrate type, average daily flow velocity, average daily temperature, salinity, and the presence/absence of sub-aquatic vegetation. If an output from a habitat suitability model is unavailable, a randomized value of 0-1 can be a placeholder, expanding the model’s utility in data-poor environments."),
           p("Pre-processing data is essential for ensuring spatial accuracy within the model. This process involved re-projecting multiple shape files representing the project area and re-projecting them to a common coordinate reference system (CRS). The World Geodetic System 1984 (WGS 84) was utilized for its compatibility with the NetLogo environment. The habitat suitability data needed to be converted from latitude and longitude into patch coordinates compatible with the patch coordinates in the virtual system for implementation into the ABM. This involves reading the data, defining the spatial extent and CRS, and calculating the patch coordinates based on this defined extent. The data with transformed coordinates is then integrated into the virtual system.")
    )
  )
)
        
      )
    )
  )
)

server <- function(input, output, session) {
  
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
      addProviderTiles("Esri.WorldTopoMap") %>%  # Change to terrain map
      setView(lng = -70.785784, lat = 41.330439, zoom = 14) %>%
      addAwesomeMarkers(
        lng = -70.785784, lat = 41.330439, 
        icon = awesomeIcons(
          icon = 'fish',  # Font Awesome icon name
          iconColor = 'white',
          markerColor = 'blue',
          library = 'fa'
        ),
        popup = "Herring Creek Fishery",
        label = "Herring Creek Fishery"
      ) %>%
      addLegend(
        position = "bottomright", 
        colors = "blue", 
        labels = "Herring Creek Fishery", 
        title = NULL
      )
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
        addRasterImage(raster_layer, colors = viridis(10)) #%>%
        #addLegend("bottomright", colors = viridis(10), labels = seq(min_value, max_value, length.out = 10))
    })
    
    output$parameter_info <- renderText({
      paste("You selected:", selected_parameter)
    })
  })
  ## Habitat Results
  output$map_adult_alewife_habitat_results <- renderLeaflet({
    min_value <- min(values(adult_alewife), na.rm = TRUE)
    max_value <- max(values(adult_alewife), na.rm = TRUE)
    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -70.7916, lat = 41.3328, zoom = 12) %>%
      addRasterImage(adult_alewife, colors = viridis(10)) %>%
      addLegend("bottomright", pal = colorNumeric(viridis(10), domain = c(0, 1)), values = seq(0, 1), title = "Habitat Suitability")
  })
  
  output$map_egg_larvae_alewife_habitat_results <- renderLeaflet({
    min_value <- min(values(egg_alewife), na.rm = TRUE)
    max_value <- max(values(egg_alewife), na.rm = TRUE)
    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -70.7916, lat = 41.3328, zoom = 12) %>%
      addRasterImage(egg_alewife, colors = viridis(10)) %>%
      addLegend("bottomright", pal = colorNumeric(viridis(10), domain = c(0, 1)), values = seq(0, 1), title = "Habitat Suitability")
  })
  
  output$map_juv_alewife_habitat_results <- renderLeaflet({
    min_value <- min(values(juv_alewife), na.rm = TRUE)
    max_value <- max(values(juv_alewife), na.rm = TRUE)
    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -70.7916, lat = 41.3328, zoom = 12) %>%
      addRasterImage(juv_alewife, colors = viridis(10)) %>%
      addLegend("bottomright", pal = colorNumeric(viridis(10), domain = c(0, 1)), values = seq(0, 1), title = "Habitat Suitability")
  })
  
  output$map_adult_blueback_habitat_results <- renderLeaflet({
    min_value <- min(values(adult_blueback), na.rm = TRUE)
    max_value <- max(values(adult_blueback), na.rm = TRUE)
    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -70.7916, lat = 41.3328, zoom = 12) %>%
      addRasterImage(adult_blueback, colors = viridis(10)) %>%
      addLegend("bottomright", pal = colorNumeric(viridis(10), domain = c(0, 1)), values = seq(0, 1), title = "Habitat Suitability")
  })
  
  output$map_egg_larvae_blueback_habitat_results <- renderLeaflet({
    min_value <- min(values(egg_blueback), na.rm = TRUE)
    max_value <- max(values(egg_blueback), na.rm = TRUE)
    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -70.7916, lat = 41.3328, zoom = 12) %>%
      addRasterImage(egg_blueback, colors = viridis(10)) %>%
      addLegend("bottomright", pal = colorNumeric(viridis(10), domain = c(0, 1)), values = seq(0, 1), title = "Habitat Suitability")
  })
  
  output$map_juv_blueback_habitat_results <- renderLeaflet({
    min_value <- min(values(juv_blueback), na.rm = TRUE)
    max_value <- max(values(juv_blueback), na.rm = TRUE)
    
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = -70.7916, lat = 41.3328, zoom = 12) %>%
      addRasterImage(juv_blueback, colors = viridis(10)) %>%
      addLegend("bottomright", pal = colorNumeric(viridis(10), domain = c(0, 1)), values = seq(0, 1), title = "Habitat Suitability")
  })
  
  #####################################################################################
  # Adult Alewife Suitability Indices
  output$adult_alewife_temp_plot <- renderPlotly({
    plot_ly(
      data = adult_alewife_temp_suitability_data,
      x = ~Temperature,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Temperature (°C)",
      xaxis = list(title = "Temperature (°C)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_alewife_depth_plot <- renderPlotly({
    plot_ly(
      data = adult_alewife_depth_suitability_data,
      x = ~Depth,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Depth (meters)",
      xaxis = list(title = "Depth (m)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_alewife_salinity_plot <- renderPlotly({
    plot_ly(
      data = adult_alewife_salinity_suitability_data,
      x = ~Salinity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Salinity (psu)",
      xaxis = list(title = "Salinity (psu)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_alewife_velocity_plot <- renderPlotly({
    plot_ly(
      data = adult_alewife_velocity_suitability_data,
      x = ~Velocity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Flow Velocity (m/s)",
      xaxis = list(title = "Average Daily Flow Velocity (m/s)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_alewife_sediment_plot <- renderPlotly({
    plot_ly(
      data = adult_alewife_substrate_suitability_data,
      x = ~Substrate,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Substrate Classification",
      xaxis = list(title = "Substrate Types"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_alewife_SAV_plot <- renderPlotly({
    plot_ly(
      data = adult_alewife_SAV_suitability_data,
      x = ~SAV,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Sub-Aquatic Vegetation (SAV)",
      xaxis = list(title = "Presence/Absent of SAV"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  #  Alewife Eggs & Larvae  Suitability Indices
  output$eggs_alewife_temp_plot <- renderPlotly({
    plot_ly(
      data = alewife_eggs_temp_suitability_data,
      x = ~Temperature,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Temperature (°C)",
      xaxis = list(title = "Temperature (°C)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_alewife_depth_plot <- renderPlotly({
    plot_ly(
      data = alewife_eggs_depth_suitability_data,
      x = ~Depth,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Depth (meters)",
      xaxis = list(title = "Depth (m)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_alewife_salinity_plot <- renderPlotly({
    plot_ly(
      data = alewife_eggs_salinity_suitability_data,
      x = ~Salinity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Salinity (psu)",
      xaxis = list(title = "Salinity (psu)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_alewife_velocity_plot <- renderPlotly({
    plot_ly(
      data = alewife_eggs_velocity_suitability_data,
      x = ~Velocity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Flow Velocity (m/s)",
      xaxis = list(title = "Average Daily Flow Velocity (m/s)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_alewife_sediment_plot <- renderPlotly({
    plot_ly(
      data = alewife_eggs_substrate_suitability_data,
      x = ~Substrate,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Substrate Classification",
      xaxis = list(title = "Substrate Types"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_alewife_SAV_plot <- renderPlotly({
    plot_ly(
      data = alewife_eggs_SAV_suitability_data,
      x = ~SAV,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Sub-Aquatic Vegetation (SAV)",
      xaxis = list(title = "Presence/Absent of SAV"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  # Juvenule  Alewife Suitability Indices
  output$juv_alewife_temp_plot <- renderPlotly({
    plot_ly(
      data = juvenile_alewife_temp_suitability_data,
      x = ~Temperature,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Temperature (°C)",
      xaxis = list(title = "Temperature (°C)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_alewife_depth_plot <- renderPlotly({
    plot_ly(
      data = juvenile_alewife_depth_suitability_data,
      x = ~Depth,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Depth (meters)",
      xaxis = list(title = "Depth (m)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_alewife_salinity_plot <- renderPlotly({
    plot_ly(
      data = juvenile_alewife_salinity_suitability_data,
      x = ~Salinity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Salinity (psu)",
      xaxis = list(title = "Salinity (psu)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_alewife_velocity_plot <- renderPlotly({
    plot_ly(
      data = juvenile_alewife_velocity_suitability_data,
      x = ~Velocity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Flow Velocity (m/s)",
      xaxis = list(title = "Average Daily Flow Velocity (m/s)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_alewife_sediment_plot <- renderPlotly({
    plot_ly(
      data = juvenile_alewife_substrate_suitability_data,
      x = ~Substrate,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Substrate Classification",
      xaxis = list(title = "Substrate Types"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_alewife_SAV_plot <- renderPlotly({
    plot_ly(
      data = juvenile_alewife_SAV_suitability_data,
      x = ~SAV,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Sub-Aquatic Vegetation (SAV)",
      xaxis = list(title = "Presence/Absent of SAV"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  ##############################################################################
  # Adult Alewife Suitability Indices
  output$adult_blueback_temp_plot <- renderPlotly({
    plot_ly(
      data = blueback_adult_temp_suitability_data,
      x = ~Temperature,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Temperature (°C)",
      xaxis = list(title = "Temperature (°C)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_blueback_depth_plot <- renderPlotly({
    plot_ly(
      data = blueback_adult_depth_suitability_data,
      x = ~Depth,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Depth (meters)",
      xaxis = list(title = "Depth (m)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_blueback_salinity_plot <- renderPlotly({
    plot_ly(
      data = blueback_adult_salinity_suitability_data,
      x = ~Salinity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Salinity (psu)",
      xaxis = list(title = "Salinity (psu)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_blueback_velocity_plot <- renderPlotly({
    plot_ly(
      data = blueback_adult_velocity_suitability_data,
      x = ~Velocity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Flow Velocity (m/s)",
      xaxis = list(title = "Average Daily Flow Velocity (m/s)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_blueback_sediment_plot <- renderPlotly({
    plot_ly(
      data = blueback_adult_substrate_suitability_data,
      x = ~Substrate,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Substrate Classification",
      xaxis = list(title = "Substrate Types"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$adult_blueback_SAV_plot <- renderPlotly({
    plot_ly(
      data = blueback_adult_SAV_suitability_data,
      x = ~SAV,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Sub-Aquatic Vegetation (SAV)",
      xaxis = list(title = "Presence/Absent of SAV"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  #  Alewife Eggs & Larvae  Suitability Indices
  output$eggs_blueback_temp_plot <- renderPlotly({
    plot_ly(
      data = blueback_eggs_temp_suitability_data,
      x = ~Temperature,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Temperature (°C)",
      xaxis = list(title = "Temperature (°C)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_blueback_depth_plot <- renderPlotly({
    plot_ly(
      data = blueback_eggs_depth_suitability_data,
      x = ~Depth,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Depth (meters)",
      xaxis = list(title = "Depth (m)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_blueback_salinity_plot <- renderPlotly({
    plot_ly(
      data = blueback_eggs_salinity_suitability_data,
      x = ~Salinity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Salinity (psu)",
      xaxis = list(title = "Salinity (psu)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_blueback_velocity_plot <- renderPlotly({
    plot_ly(
      data = blueback_eggs_velocity_suitability_data,
      x = ~Velocity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Flow Velocity (m/s)",
      xaxis = list(title = "Average Daily Flow Velocity (m/s)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_blueback_sediment_plot <- renderPlotly({
    plot_ly(
      data = blueback_eggs_substrate_suitability_data,
      x = ~Substrate,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Substrate Classification",
      xaxis = list(title = "Substrate Types"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$eggs_blueback_SAV_plot <- renderPlotly({
    plot_ly(
      data = blueback_eggs_SAV_suitability_data,
      x = ~SAV,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Sub-Aquatic Vegetation (SAV)",
      xaxis = list(title = "Presence/Absent of SAV"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  # Juvenule  Alewife Suitability Indices
  output$juv_blueback_temp_plot <- renderPlotly({
    plot_ly(
      data = juvenile_blueback_temp_suitability_data,
      x = ~Temperature,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Temperature (°C)",
      xaxis = list(title = "Temperature (°C)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_blueback_depth_plot <- renderPlotly({
    plot_ly(
      data = juvenile_blueback_depth_suitability_data,
      x = ~Depth,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Depth (meters)",
      xaxis = list(title = "Depth (m)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_blueback_salinity_plot <- renderPlotly({
    plot_ly(
      data = juvenile_blueback_salinity_suitability_data,
      x = ~Salinity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Salinity (psu)",
      xaxis = list(title = "Salinity (psu)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_blueback_velocity_plot <- renderPlotly({
    plot_ly(
      data = juvenile_blueback_velocity_suitability_data,
      x = ~Velocity,
      y = ~SuitabilityIndex,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(shape = 'hv')  # 'hv' for step-like plot
    ) %>% layout(
      title = "Average Daily Flow Velocity (m/s)",
      xaxis = list(title = "Average Daily Flow Velocity (m/s)"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_blueback_sediment_plot <- renderPlotly({
    plot_ly(
      data = juvenile_blueback_substrate_suitability_data,
      x = ~Substrate,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Substrate Classification",
      xaxis = list(title = "Substrate Types"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })
  
  output$juv_blueback_SAV_plot <- renderPlotly({
    plot_ly(
      data = juvenile_blueback_SAV_suitability_data,
      x = ~SAV,
      y = ~SuitabilityIndex,
      type = 'bar',  # Change to 'bar' for bar graph
      marker = list(color = 'viridis')
    ) %>% layout(
      title = "Sub-Aquatic Vegetation (SAV)",
      xaxis = list(title = "Presence/Absent of SAV"),
      yaxis = list(title = "Suitability Value", range = c(0, 1))
    )
  })

############## Migration Model Results ##################
  ############## Migration Model Results ##################
  output$baselinePlot <- renderLeaflet({
    # Calculate the mean of x and y values
    center <- c(mean(adult_alewife_data$x), mean(adult_alewife_data$y))
    
    # Create a color palette
    palette <- colorNumeric(palette = viridis(length(unique(filtered_baseline_spawning_data$spawning.events.in.patch))), 
                            domain = filtered_baseline_spawning_data$spawning.events.in.patch)
    
    # Create a leaflet map
    leaflet(data = filtered_baseline_spawning_data) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = mean(center[1]), lat = mean(center[2]), zoom = 13) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~spawning.events.in.patch * 0.5,  # Scale down radius by a factor of 0.5
        color = "#21908CFF", stroke = FALSE, fillOpacity = 0.5,
        label = ~spawning.events.in.patch
      )
  })
  
  output$predationTable <- renderDT({
    datatable(
      data.frame(
        "Simulation" = c("Baseline", "Low", "Moderate", "High"),
        "Initial Prey Population (River Herring)" = c(1000, 1000, 1000, 1000),
        "Predation Level (%)" = c(0, 0.1, 0.5, 1),
        "Initial Predator Population (Striped Bass)" = c(0, 1, 5, 10)
      ),
      colnames = c("Simulation", "Initial Prey Population (River Herring)", "Predation Level (%)", "Initial Predator Population (Striped Bass)"),
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        dom = 't<"clear">',
        columnDefs = list(list(width = '200px', targets = "_all"))
      ),
      rownames = FALSE,
      class = 'table table-striped table-bordered'
    )
  })
  
  output$lowPlot <- renderLeaflet({
    # Calculate the mean of x and y values
    center <- c(mean(adult_alewife_data$x), mean(adult_alewife_data$y))
    
    # Create a color palette
    palette <- colorNumeric(palette = viridis(length(unique(filtered_baseline_spawning_data$spawning.events.in.patch))), 
                            domain = filtered_baseline_spawning_data$spawning.events.in.patch)
    
    # Create a leaflet map
    leaflet(data = filtered_low_spawning_data) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = mean(center[1]), lat = mean(center[2]), zoom = 13) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~spawning.events.in.patch * 0.5,  # Scale down radius by a factor of 0.5
        color = "#21908CFF", stroke = FALSE, fillOpacity = 0.5,
        label = ~spawning.events.in.patch
      )
  })
  
  output$lowPred_Plot <- renderLeaflet({
    # Calculate the mean of x and y values
    center <- c(mean(adult_alewife_data$x), mean(adult_alewife_data$y))
    
    # Create a color palette
    palette <- colorNumeric(palette = viridis(length(unique(filtered_high_predation_data$prey.eaten.in.patch))), 
                            domain = filtered_high_predation_data$prey.eaten.in.patch)
    
    # Create a leaflet map
    leaflet(data = filtered_low_predation_data) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = mean(center[1]), lat = mean(center[2]), zoom = 13) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~prey.eaten.in.patch * 0.5,  # Scale down radius by a factor of 0.5
        color = "#FDE725FF", stroke = FALSE, fillOpacity = 0.5,
        label = ~prey.eaten.in.patch
      )
  })
  
  output$modPlot <- renderLeaflet({
    # Calculate the mean of x and y values
    center <- c(mean(adult_alewife_data$x), mean(adult_alewife_data$y))
    
    # Create a color palette
    palette <- colorNumeric(palette = viridis(length(unique(filtered_baseline_spawning_data$spawning.events.in.patch))), 
                            domain = filtered_baseline_spawning_data$spawning.events.in.patch)
    
    # Create a leaflet map
    leaflet(data = filtered_mod_spawning_data) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = mean(center[1]), lat = mean(center[2]), zoom = 13) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~spawning.events.in.patch * 0.5,  # Scale down radius by a factor of 0.5
        color = "#21908CFF", stroke = FALSE, fillOpacity = 0.5,
        label = ~spawning.events.in.patch
      )
  })
  
  output$modPred_Plot <- renderLeaflet({
    # Calculate the mean of x and y values
    center <- c(mean(adult_alewife_data$x), mean(adult_alewife_data$y))
    
    # Create a color palette
    palette <- colorNumeric(palette = viridis(length(unique(filtered_high_predation_data$prey.eaten.in.patch))), 
                            domain = filtered_high_predation_data$prey.eaten.in.patch)
    
    # Create a leaflet map
    leaflet(data = filtered_mod_predation_data) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = mean(center[1]), lat = mean(center[2]), zoom = 13) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~prey.eaten.in.patch * 0.5,  # Scale down radius by a factor of 0.5
        color = "#FDE725FF", stroke = FALSE, fillOpacity = 0.5,
        label = ~prey.eaten.in.patch
      )
  })
  
  output$highPlot <- renderLeaflet({
    # Calculate the mean of x and y values
    center <- c(mean(adult_alewife_data$x), mean(adult_alewife_data$y))
    
    # Create a color palette
    palette <- colorNumeric(palette = viridis(length(unique(filtered_baseline_spawning_data$spawning.events.in.patch))), 
                            domain = filtered_baseline_spawning_data$spawning.events.in.patch)
    
    # Create a leaflet map
    leaflet(data = filtered_high_spawning_data) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = mean(center[1]), lat = mean(center[2]), zoom = 13) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~spawning.events.in.patch * 0.5,  # Scale down radius by a factor of 0.5
        color = "#21908CFF", stroke = FALSE, fillOpacity = 0.5,
        label = ~spawning.events.in.patch
      )
  })
  
  output$highPred_Plot <- renderLeaflet({
    # Calculate the mean of x and y values
    center <- c(mean(adult_alewife_data$x), mean(adult_alewife_data$y))
    
    # Create a color palette
    palette <- colorNumeric(palette = viridis(length(unique(filtered_high_predation_data$prey.eaten.in.patch))), 
                            domain = filtered_high_predation_data$prey.eaten.in.patch)
    
    # Create a leaflet map
    leaflet(data = filtered_high_predation_data) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = mean(center[1]), lat = mean(center[2]), zoom = 13) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = ~prey.eaten.in.patch * 0.5,  # Scale down radius by a factor of 0.5
        color = "#FDE725FF", stroke = FALSE, fillOpacity = 0.5,
        label = ~prey.eaten.in.patch
      )
  })
}

shinyApp(ui = ui, server = server)