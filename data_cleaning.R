library(here)
library(janitor)
library(tidyverse)
library(sf)
library(tmap)

source("functions/name_cleaning.R")

# ------- Configuring file paths -------

csv_data <- list.files(here("data"), pattern = "\\.csv$", full.names = TRUE)
shp_data <- here("data","cal_drinking","California_Drinking_Water_System_Area_Boundaries.shp")
cache_rds <- here("data","cached_data.rds")
curr_file <- here("data_cleaning.R")
name_cleaning_file <- here("name_cleaning.R")

# ------- Checking timestamps -------

# Get timestamp from our cache file or NA if it doesn't exist.
rds_mtime <- if (file.exists(cache_rds)) file.info(cache_rds)$mtime else as.POSIXct(NA)

# Check if the CSV files has been updated. 
raw_mtimes <- file.info(csv_data)$mtime
raw_newer <- any(raw_mtimes > rds_mtime, na.rm = TRUE)

# Check if the shape file has been updated.
shp_newer <- FALSE
if (file.exists(shp_data) && file.exists(cache_rds)) {
  shp_newer <- file.info(shp_data)$mtime > rds_mtime
}

# Check if 
script_newer <- FALSE
if (file.exists(curr_file) && file.exists(cache_rds)) {
  script_newer <- file.info(curr_file)$mtime > rds_mtime
}

# name_cleaning.R newer?
name_cleaner_newer <- FALSE
if (file.exists(name_cleaning_file) && file.exists(cache_rds)) {
  name_cleaner_newer <- file.info(name_cleaning_file)$mtime > rds_mtime
}

# now combine
needs_rebuild <- 
  !file.exists(cache_rds) ||
  raw_newer ||
  shp_newer ||
  script_newer ||
  name_cleaner_newer

if (needs_rebuild) {
  message("Rebuilding:", basename(cache_rds))
  
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
    
    historical_clean <- clean_supplier_name(historical_production, "water_system_name")
    fiveyr_clean <- clean_supplier_name(five_year_outlook, "supplier_name")
    monthly_clean <- clean_supplier_name(monthly_water_outlook, "supplier_name")
    actual_clean <- clean_supplier_name(actual_shortage, "supplier_name")
    
    supplier_data <- bind_rows(
      historical_clean %>% select(org_id, supplier_name),
      fiveyr_clean %>% select(org_id, supplier_name),
      monthly_clean %>% select(org_id, supplier_name),
      actual_clean %>% select(org_id, supplier_name)
    ) %>%
      distinct() |> 
      mutate(name_with_id = paste(supplier_name, "-", org_id))
    
    
    list(
      actual_shortage = actual_shortage,
      five_year_outlook = five_year_outlook,
      historical_production = historical_production,
      monthly_water_outlook = monthly_water_outlook,
      supplier_data = supplier_data,
      source_name = source_name
    )
  }
  
  # Needed to define this first.
  water_data <- load_water_data()
  
  # ------- Loading Spatial Data ------- 
  load_spatial_data <- function() {
    
    # ------- Merging source_name data and making it displayable as points on Tmap.
    # ****Not currently in use.
    
    # Prepare source_name data by removing NA values in org_id, lat, and longitude.
    # Dropping name, status, availability, and id as we only want to display type & location.
    source_geo <- water_data$source_name |> 
      
      # Filter out coordinate NAs & org_id NAs.
      filter(!is.na(latitude)) |>
      filter(!is.na(longitude)) |>
      filter(!is.na(org_id)) |> 
      
      # Remove unnecessary from source_geo.
      select(-c(
        "source_facility_name", 
        "source_facility_activity_status", 
        "source_facility_availability", 
        "source_facility_id"))
    
    # Convert our coordinates into spatial data for Tmap.
    source_geo <- st_as_sf(source_geo, coords = c("longitude", "latitude"), crs = "EPSG:4269") 
    
    # ------- Importing & Cleaning district boundaries for our Tmap render.
    
    district_shape <- read_sf(here("data", "cal_drinking", "California_Drinking_Water_System_Area_Boundaries.shp")) |> 
      clean_names() |> # Clean up column names. 
      select(c("water_syst", "geometry")) |> 
      st_transform("EPSG:4269") |> # Change CRS of district_shape data.
      st_make_valid()
    
    list(
      district_shape = district_shape,
      source_geo = source_geo
    )
  }
  
  spatial_data <- load_spatial_data()
  
  # save both into one RDS
  saveRDS(list(water_data = water_data, spatial_data = spatial_data), cache_rds)
}

# ------- Loading from cache ------- 
cached <- readRDS(cache_rds)
water_data <- cached$water_data
spatial_data <- cached$spatial_data

# clean up
rm(cached, needs_rebuild, csv_data, cache_rds, curr_file)







