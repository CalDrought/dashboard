library(here)
library(janitor)
library(tidyverse)
library(sf)
library(tmap)
source("data_cleaning.R")

# Filtering helper function for actual_shortage data.
actual_shortage_tmap_filtering <- function(){
  
  # Find the latest year with data.
  last_year <- water_data$historical_production |>
    select(start_date) |>
    mutate(year = year(start_date)) |>
    pull(year) |>
    max(na.rm = TRUE)
  
  # Computes average shortage across all actual_shortage data. Grouped by org_id & pwsid.
  mean_shortages <- water_data$actual_shortage |>
    filter(year(start_date) == last_year) |>
    group_by(org_id, pwsid) |>
    summarize(mean_level = mean(state_standard_shortage_level, na.rm = TRUE), .groups = "drop")
    
  # Join actual_shortage w/ spatial boundaries and cleaned supplier names.
  actual_shortage_by_district <- spatial_data$district_shape |>
    inner_join(mean_shortages, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id")
  
  # Return filtered/cleaned df.
  return(actual_shortage_by_district)
}

actual_shortage_tmap <- function(){
  
  # Access filtered/cleaned actual_shortage data from helper function.
  actual_shortage_filtered <- actual_shortage_tmap_filtering()
  
  # Interactive mode.
  tmap_mode("view") 
  
  # Build tmap of mean shortage levels by org_id & pwsid boundary.
  actual_shortage_tmap <-
    tm_shape(actual_shortage_filtered) +
    tm_fill("mean_level",
            title = "Mean Shortage Level",
            palette = "Reds",
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    
    # Move the legend into the bottom right.
    tm_view(
      view.legend.position  = c("right", "bottom"), # Moves the legend to bottom right.
      control.position = c("left", "bottom") # Moves the layer picker to bottom left.
    )
  
  # Return tmap.
  actual_shortage_tmap
}

monthly_outlook_tmap_filtering <- function(){
  
  # Find the latest year with data.
  last_year <- water_data$monthly_water_outlook |>
    select(forecast_start_date) |>
    mutate(year = year(forecast_start_date)) |>
    pull(year) |>
    max(na.rm = TRUE)
  
  # Computes average shortage across all monthly_water_outlook data. Grouped by org_id & pwsid.
  mean_shortages <- water_data$monthly_water_outlook |>
    filter(year(forecast_start_date) == 2025) |>
    group_by(org_id, pwsid) |>
    summarize(mean_level = mean(state_standard_shortage_level, na.rm = TRUE), .groups = "drop")
  view(mean_shortages)
  
  # Join monthly_water_outlook w/ spatial boundaries and cleaned supplier names.
  monthly_shortage_by_district <- spatial_data$district_shape |>
    inner_join(mean_shortages, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id")
  
  # Return filtered/cleaned df.
  return(monthly_shortage_by_district)
}

monthly_outlook_tmap <- function(){
  
  # Access filtered/cleaned monthly_water_outlook data from helper function.
  monthly_outlook_filtered <- monthly_outlook_tmap_filtering()
  
  # Interactive mode.
  tmap_mode("view")
  
  # Build tmap of mean shortage levels by org_id & pwsid boundary.
  monthly_tmap <-
    tm_shape(monthly_outlook_filtered) +
    tm_fill("mean_level",
            title = "Mean Shortage Level",
            palette = "Reds",
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    
    # Move the legend into the bottom right.
    tm_view(
      view.legend.position  = c("right", "bottom"), # Moves the legend to bottom right.
      control.position = c("left", "bottom") # Moves the layer picker to bottom left.
    )
  
  # Return tmap.
  monthly_tmap
}

five_year_outlook_tmap_filtering <- function(){
  
  # Find the latest year with data.
  last_year <- water_data$five_year_outlook |>
    select(forecast_start_date) |>
    mutate(year = year(forecast_start_date)) |>
    pull(year) |>
    max(na.rm = TRUE)
  
  # Computes mean difference between supply & use across all five_year_outlook data. Grouped by org_id & pwsid.
  mean_diffs_per_org <- water_data$five_year_outlook |>
    filter(year(forecast_start_date) == last_year) |>
    mutate(difference_supply_and_use = water_supplies_acre_feet - water_use_acre_feet) |>
    group_by(org_id, pwsid) |>
    summarise(mean_difference_supply_and_use = mean(difference_supply_and_use, na.rm = TRUE), .groups = "drop")
  
  # Join five_year_outlook w/ spatial boundaries and cleaned supplier names.
  five_year_outlook_by_district <- spatial_data$district_shape |>
    inner_join(mean_diffs_per_org, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id")
  
  # Return filtered/cleaned df.
  return(five_year_outlook_by_district)
}

five_year_outlook_tmap <- function(){
  
  # Access filtered/cleaned five_year_outlook data from helper function.
  five_year_filtered <- five_year_outlook_tmap_filtering()
  
  # Interactive mode.
  tmap_mode("view")
  
  # Build tmap of the difference between supply & use by org_id & pwsid boundary.
  five_year_tmap <-
    tm_shape(five_year_filtered) +
    tm_fill("mean_difference_supply_and_use",
            title = "Supply - Use (Acre Ft)",
            palette = "RdBu",
            midpoint = 0, 
            stlye = "cont",
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    
    # Move the legend into the bottom right.
    tm_view(
      view.legend.position  = c("right", "bottom"), # Moves the legend to bottom right.
      control.position = c("left", "bottom") # Moves the layer picker to bottom left.
    )
  
  # Return tmap.
  five_year_tmap
}

historical_production_tmap_filtering <- function(){
  
  # Find the latest year with data.
  last_year <- water_data$historical_production |>
    select(start_date) |>
    mutate(year = year(start_date)) |>
    pull(year) |>
    max(na.rm = TRUE)
  
  # Computes mean difference between production & delivery across all historical_production data. Grouped by org_id & pwsid.
  difference_produced_vs_delivered <- water_data$historical_production |>
    filter(year(start_date) == last_year) |>
    pivot_wider(names_from = water_produced_or_delivered,
                values_from = quantity_acre_feet,
                values_fill = list(quantity_acre_feet = NA_real_)) |>
    group_by(org_id, pwsid, start_date) |>
    summarise(
      total_delivered = sum(`water delivered`, na.rm = TRUE),
      total_produced = sum(`water produced`,  na.rm = TRUE),
      difference = total_delivered - total_produced,
      .groups = "drop"
    ) |>
    group_by(org_id, pwsid) |>
    summarise(mean_difference = mean(difference, na.rm = TRUE), .groups = "drop")
  
  # Join historical_production w/ spatial boundaries and cleaned supplier names.
  historical_production_by_district <- spatial_data$district_shape |>
    inner_join(difference_produced_vs_delivered, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id")
  
  # Return filtered/cleaned df.
  return(historical_production_by_district)
}

historical_production_tmap <- function(){
  
  # Access filtered/cleaned historical_production data from helper function.
  historical_filtered <- historical_production_tmap_filtering()
  
  # Interactive mode.
  tmap_mode("view")
  
  # Build tmap of the difference between production & delivery by org_id & pwsid boundary.
  historical_tmap <-
    tm_shape(historical_filtered) +
    tm_fill("mean_difference",
            title = "Produced - Delivered (Acre Ft)",
            palette = "RdBu",
            midpoint = 0, 
            stlye = "cont",
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    
    # Move the legend into the bottom right.
    tm_view(
      view.legend.position  = c("right", "bottom"), # Moves the legend to bottom right.
      control.position = c("left", "bottom") # Moves the layer picker to bottom left.
    )
  
  # Return tmap.
  historical_tmap
}