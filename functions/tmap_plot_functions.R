library(here)
library(janitor)
library(tidyverse)
library(sf)
library(tmap)
source("data_cleaning.R")

actual_shortage_tmap_filtering <- function(){
  mean_shortages <- water_data$actual_shortage |>
    group_by(org_id, pwsid) |>
    summarize(mean_level = mean(state_standard_shortage_level, na.rm = TRUE), .groups = "drop")
    
  actual_shortage_by_district <- spatial_data$district_shape |>
    inner_join(mean_shortages, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id")
  
  return(actual_shortage_by_district)
}

actual_shortage_tmap <- function(){
  actual_shortage_filtered <- actual_shortage_tmap_filtering()
  
  tmap_mode("view")  # interactive mode.
  
  # Build base layers.
  base_map <-
    tm_shape(actual_shortage_filtered) +
    tm_fill("mean_level",
            title = "Mean Shortage Level",
            palette = "Reds",
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    
    # move the legend into the bottom right.
    tm_view(
      view.legend.position  = c("right", "bottom"),   # moves the legend to bottom right.
      control.position = c("left", "bottom")    # moves the layer picker there too.
    )
  
  base_map
}

monthly_outlook_tmap_filtering <- function(){
  mean_shortages <- water_data$monthly_water_outlook |>
    group_by(org_id, pwsid) |>
    summarize(mean_level = mean(state_standard_shortage_level, na.rm = TRUE), .groups = "drop")
  
  monthly_shortage_by_district <- spatial_data$district_shape |>
    inner_join(mean_shortages, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id")
  
  return(monthly_shortage_by_district)
}

monthly_outlook_tmap <- function(){
  monthly_outlook_filtered <- monthly_outlook_tmap_filtering()
  
  tmap_mode("view")  # interactive mode.
  
  # Build base layers.
  base_map <-
    tm_shape(monthly_outlook_filtered) +
    tm_fill("mean_level",
            title = "Mean Shortage Level",
            palette = "Reds",
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    
    # move the legend into the bottom right.
    tm_view(
      view.legend.position  = c("right", "bottom"),   # moves the legend to bottom right.
      control.position = c("left", "bottom")    # moves the layer picker there too.
    )
  
  base_map
}

five_year_outlook_tmap_filtering <- function(){
  mean_diffs_per_org <- water_data$five_year_outlook %>%
    mutate(difference_supply_and_use = water_supplies_acre_feet - water_use_acre_feet) %>%
    group_by(org_id, pwsid) %>%
    summarise(mean_difference_supply_and_use = mean(difference_supply_and_use, na.rm = TRUE), .groups = "drop")
  view(mean_diffs_per_org)
  
  five_year_outlook_by_district <- spatial_data$district_shape |>
    inner_join(mean_diffs_per_org, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id")
  view(five_year_outlook_by_district)
  
  return(five_year_outlook_by_district)
}

five_year_outlook_tmap <- function(){
  five_year_filtered <- five_year_outlook_tmap_filtering()
  
  tmap_mode("view")  # interactive mode.
  
  # Build base layers.
  base_map <-
    tm_shape(five_year_filtered) +
    tm_fill("mean_difference_supply_and_use",
            title = "Supply - Use (Acre Ft)",
            palette = "RdBu",
            midpoint = 0, 
            stlye = "cont",
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    
    # move the legend into the bottom right.
    tm_view(
      view.legend.position  = c("right", "bottom"),   # moves the legend to bottom right.
      control.position = c("left", "bottom")    # moves the layer picker there too.
    )
  
  base_map
}

historical_production_tmap_filtering <- function(){
  difference_produced_vs_delivered <- water_data$historical_production |> 
    pivot_wider(names_from = water_produced_or_delivered,
                values_from = quantity_acre_feet,
                values_fill = list(quantity_acre_feet = 0)) |>
    group_by(org_id, pwsid, start_date) |>
    summarise(
      total_delivered = sum(`water delivered`, na.rm = TRUE),
      total_produced = sum(`water produced`,  na.rm = TRUE),
      difference = total_delivered - total_produced,
      .groups = "drop"
    ) |>
    group_by(org_id, pwsid) |>
    summarise(mean_difference = mean(difference, na.rm = TRUE), .groups = "drop")
  view(difference_produced_vs_delivered)
  
  historical_production_by_district <- spatial_data$district_shape |>
    inner_join(difference_produced_vs_delivered, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id")
  
  return(historical_production_by_district)
}

historical_production_tmap <- function(){
  historical_filtered <- historical_production_tmap_filtering()
  
  tmap_mode("view")
  
  base_map <-
    tm_shape(historical_filtered) +
    tm_fill("mean_difference",
            title = "Produced - Delivered (Acre Ft)",
            palette = "RdBu",
            midpoint = 0, 
            stlye = "cont",
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    
    # move the legend into the bottom right.
    tm_view(
      view.legend.position  = c("right", "bottom"),   # moves the legend to bottom right.
      control.position = c("left", "bottom")    # moves the layer picker there too.
    )
  
  base_map
}