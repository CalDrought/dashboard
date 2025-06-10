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
library(jsonlite)         # Using jsonlite to parse timestamps.

# IMPORTANT HTTR has a function similar to plotly called ::conifg(). 
# Loading this library will make our plotly visuals not work. 
# DO NOT LOAD library(httr) we explicit call it elsewhere in the code where needed.

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
  here("data", "cal_drinking", "California_Drinking_Water_System_Area_Boundaries.dbf"),
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

# Helper function to find the time stamps of our files.
file_ts <- function(path) {
  if (file.exists(path)) {
    return(file.info(path)$mtime)
  } else {
    return(as.POSIXct(NA)) 
  }
}

# Helper function to return the time stamp of the files from our api calls.
ckan_ts <- function(id) {
  url <- sprintf("https://data.cnra.ca.gov/api/3/action/resource_show?id=%s", id)
  resp <- httr::GET(url, httr::timeout(5))
  message("CKAN timestamp ", substr(id, 1, 6), " ... status ", httr::status_code(resp), "\n")
  
  # Check if the status code is 200 (i.e. successful).
  if (httr::status_code(resp) != 200) {
    return(as.POSIXct(NA))
  }
  
  # Last modified time stamp for our call.
  lm_str <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "utf-8"))$
    result$last_modified
  
  # Check if the time stamp is null if not format to UTC.
  if (is.null(lm_str)) {
    return(as.POSIXct(NA))
  } else {
    return(as.POSIXct(lm_str, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
}


check_updates <- function(cache_path = cache_rds, 
                          spatial_src = spatial_files, 
                          script_src = script_files, 
                          resource_ids = RESOURCE_ID) {
  
  # If cache file exists pull previous manifest of time stamps.
  if (file.exists(cache_path)) {
    
    # Read in previous manifest.
    manifest_prev <- readRDS(cache_path)$manifest
    
    # Convert the current_ts of the previous manifest into col name "prev_ts".
    manifest_prev <- manifest_prev |>
      select(item, prev_ts = current_ts)
  } else {
    
    # If cache file doesn't exist we make a default empty manifest.
    manifest_prev <- tibble(
      item = character(),
      prev_ts = as.POSIXct(NA)
    )[0, ] # Keep it zero‑row & correct types.
  }
  
  # Building a manifest including the current time stamps of our data files.
  local_tbl <- tibble(
    item = basename(c(spatial_src, script_src)),
    current_ts = vapply(c(spatial_src, script_src), file_ts,
                        FUN.VALUE = as.POSIXct(NA))
  )
  
  # Getting the time stamps of our api read data.
  remote_tbl <- tibble(
    item = names(resource_ids),
    current_ts = vapply(resource_ids, ckan_ts,
                        FUN.VALUE = as.POSIXct(NA))
  )
  
  # Combine the time stamp tables for our api calls and local files.
  manifest_now <- bind_rows(local_tbl, remote_tbl)
  
  # Merge the previous manifest with the new one and look for file updates.
  manifest_full <- manifest_now |>
    left_join(manifest_prev, by = "item") |>
    mutate(
      updated = ifelse(is.na(prev_ts), TRUE, current_ts > prev_ts)
    ) |>
    arrange(desc(updated), item)
  
  # After `manifest_full` is created check if updates are needed.
  message("check_updates() -> ",
          sum(manifest_full$updated), " of ",
          nrow(manifest_full), " items newer than cache")
  
  # Read out which items were updated.
  if (any(manifest_full$updated)) {
    message("Updated items: ",
            paste(manifest_full$item[manifest_full$updated], collapse = ", \n"))
  }
  
  return(manifest_full)
}

# ---------- Decide whether the cache is stale ----------

# Check if the `RESOURCE_ID` of our api calls exists and is properly read in from data_cleaning.R . 
if (!exists("RESOURCE_ID", inherits = FALSE)) {
  stop("RESOURCE_ID not found. Make sure the script that defines it is sourced before calling check_updates().")
}

# Returns our manifest of time stamps.
updates_df <- check_updates()

# If any of them have been updates (i.e. if the previous/current time stamps have a poisitive difference return TRUE).
needs_refresh <- any(updates_df$updated)

# If the cache file is there and we don't have any files needing a refresh load the cache file.
if (!needs_refresh && file.exists(cache_rds)) {
  message("The cache is current --- loading from cache.")
  cache_obj <- readRDS(cache_rds)
  water_data <- cache_obj$water_data
  spatial_data <- cache_obj$spatial_data
  tmap_data <- cache_obj$tmap_data
} else {
  
  # Alternatively we rebuild the cache 
  message("Rebuilding cache (changed items: ",
          paste(updates_df$item[updates_df$updated], collapse = ", "), ")")
  message("Updating snapshot of manifest inside cache file.")
  print(dplyr::select(updates_df, item, prev_ts, current_ts, updated))
  
  # ---------- Re-build Everything ----------
  water_data <- load_water_data()
  spatial_data <- load_spatial_data()
  
  tmap_data <- list(
    actual_shortage_tmap_data = actual_shortage_tmap_filtering(),
    monthly_outlook_tmap_data = monthly_outlook_tmap_filtering(),
    five_year_outlook_tmap_data = five_year_outlook_tmap_filtering(),
    historical_production_tmap_data = historical_production_tmap_filtering()
  )
  
  # Saving our cached_data.rds file.
  saveRDS(
    list(
      water_data = water_data,
      spatial_data = spatial_data,
      tmap_data = tmap_data,
      manifest = updates_df |>
        select(item, current_ts) # Only saving important columns of our manifest.
    ),
    cache_rds
  )
}


