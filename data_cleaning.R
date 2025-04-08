library(here)
library(janitor)
library(tidyverse)
library(sf)
library(tmap)

# ------- Loading Datasets on Call-------
load_water_data <- function() {
  actual_shortage <- read_csv(here("data", "actual_water_shortage_level.csv")) |>
    clean_names()
  
  five_year_outlook <- read_csv(here("data", "five_year_water_shortage_outlook.csv")) |>
    clean_names()
  
  historical_production <- read_csv(here("data", "historical_production_delivery.csv")) |>
    clean_names()
  
  monthly_water_outlook <- read_csv(here("data", "monthly_water_shortage_outlook.csv")) |>
    clean_names()
  
  source_name <- read_csv(here("data", "source_name.csv")) |>
    clean_names()
  
  list(
    actual_shortage = actual_shortage,
    five_year_outlook = five_year_outlook,
    historical_production = historical_production,
    monthly_water_outlook = monthly_water_outlook,
    source_name = source_name
  )
}

# Needed to define this first.
water_data <- load_water_data()

# ------- Loading Spatial Data ------- 
load_spatial_data <- function() {
  district_boundaries <- read_csv(here("data", "California_Drinking_Water_System_Area_Boundaries.csv")) |>
    clean_names()
  
  district_shape <- read_sf(here("data", "cal_drinking", "California_Drinking_Water_System_Area_Boundaries.shp")) |> 
    clean_names()
  
  district_json <- read_sf("data", "California_Drinking_Water_System_Area_Boundaries", "California_Drinking_Water_System_Area_Boundaries.geojson")
  
  
  # Filter out coordinate NAs
  source_geo <- water_data$source_name %>% 
    filter(!is.na(latitude)) %>% 
    filter(!is.na(org_id))

  list(
    district_boundaries = district_boundaries,
    district_shape = district_shape,
    district_json = district_json,
    source_geo = source_geo
  )
}

spatial_data <- load_spatial_data()

