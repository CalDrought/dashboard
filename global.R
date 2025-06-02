# ---------- Load Packages ----------
library(here)             # File path management.
library(janitor)          # Cleaning dataset variable names.
library(tidyverse)        # Data transformation tools.
library(sf)               # Managing spatial variables.
library(tmap)             # Creating spatial visualizations.
library(rmapshaper)       # Limiting spatial resolution of district boundaries.
library(shiny)            # Core Shiny web framework
library(shinydashboard)   # Useful for box UI and dashboards
library(tools)            # Includes toTitleCase(), used to clean up dataset names
library(shinyjs)          # Enables JavaScript interaction inside Shiny
library(spnaf)            # Spatial analysis functions for network flows
library(shinyBS)          # Bootstrap popovers and modals
library(paletteer)        # For access to a wide variety of color palettes
library(readr)            # CSV translations in r.
library(ggplot2)          # Creating Visualizations.
library(dplyr)            # Filtering, Sorting, and so on.
library(bslib)            # Adding shiny app themes.
library(shinyWidgets)     # Extra shiny app widgets/tools.
library(plotly)           # Creating interactive plots 
library(DT)               # For tabling data.
library(shinycssloaders)  # Adding loading spinner ui.

# ---------- Sourcing Files ----------
source(here("R", "data_cleaning.R"))
source(here("R", "name_cleaning.R"))
source(here("R", "tmap_plot_functions.R"))
source(here("R", "dashboard_functions.R"))
source(here("R", "calculate_na_summary.R"))
source(here("R", "sum_stat_functions.R"))

# ---------- Tmap Options ----------
tmap_mode("view") # Interactive mode.

# ---------- Path for cached data file ----------
cache_rds <- here("data", "cached_data.rds")

spatial_files <- c(
  here("data", "cal_drinking", "California_Drinking_Water_System_Area_Boundaries.shx"),
  here("data", "cal_drinking", "California_Drinking_Water_System_Area_Boundaries.shp"),
  here("data", "cal_drinking", "California_Drinking_Water_System_Area_Boundaries.prj"),
  here("data", "cal_drinking", "California_Drinking_Water_System_Area_Boundaries.dpf"),
  here("data", "cal_drinking", "California_Drinking_Water_System_Area_Boundaries.cpg")
)

script_files <- c(
  here("R", "data_cleaning.R"),
  here("R", "name_cleaning.R"),
  here("R", "tmap_plot_functions.R"),
  here("R", "dashboard_functions.R"),
  here("R", "calculate_na_summary.R"),
  here("R", "sum_stat_functions.R")
)

latest_time <- function(paths) {
  times <- sapply(paths, function(p) {
    if (file.exists(p)) file.info(p)$mtime else as.POSIXct(0)
  })
  max(mtimes, na.rm = TRUE)
}

# Create cache file is it doesn't exists. (UPDATE w/ ADDITIONAL ARGUMENTS SOON)
if (file.exists(cache_rds)) {
  
  # Read in cache file.
  cached <- readRDS(cache_rds)
  
  # Load respective datasets stored in cache. 
  water_data <- cached$water_data # All main datasets.
  spatial_data <- cached$spatial_data # District boundary data.
  tmap_data <- cached$tmap_data # Tmap filtering by datasets.
} else {

  # Create cache RDS file.
  water_data <- load_water_data() # Read in data from api.
  spatial_data <- load_spatial_data() # Read in data from shape file.
  
  # Tmap filtering by dataset for all plottable datasets.
  actual_shortage_tmap_data <- actual_shortage_tmap_filtering()
  monthly_outlook_tmap_data <- monthly_outlook_tmap_filtering()
  five_year_outlook_tmap_data <- five_year_outlook_tmap_filtering()
  historical_production_tmap_data <- historical_production_tmap_filtering()
  
  # Compact tmap filtered data into a list for storage similar to water/spatial data.
  tmap_data <- list(
    actual_shortage_tmap_data = actual_shortage_tmap_data,
    monthly_outlook_tmap_data = monthly_outlook_tmap_data,
    five_year_outlook_tmap_data = five_year_outlook_tmap_data,
    historical_production_tmap_data = historical_production_tmap_data
  )
  
  # Store our variables into a file labeled cache_rds.
  saveRDS(list(
    water_data = water_data, 
    spatial_data = spatial_data,
    tmap_data = tmap_data
    ), cache_rds)
}

