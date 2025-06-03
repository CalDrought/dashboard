# Find the last available year that has at least 10 valid suppliers w/ data.
#
# **Note**
#  All available orgs will be plotted later just need to make 
#  sure there to find the latest year with enough valid data.
find_latest_valid_year <- function(data, min_suppliers = 10) {
  
  # Check which date column is available (forecast or not).
  if ("forecast_start_date" %in% names(data)) {
    date_col <- "forecast_start_date"
  } else if ("start_date" %in% names(data)) {
    date_col <- "start_date"
  } else {
    stop("Neither 'forecast_start_date' nor 'start_date' found in data.")
  }
  
  # Find the last available year.
  latest_valid_year <- data |>
    filter(!is.na(!!rlang::sym(date_col)), !is.na(org_id), !is.na(pwsid)) |>
    mutate(year = year(!!rlang::sym(date_col))) |>
    group_by(org_id, pwsid, year) |>
    summarise(n = n(), .groups = "drop") |>
    count(year, name = "num_valid_suppliers") |>
    filter(num_valid_suppliers >= min_suppliers) |>
    summarise(last_valid_year = max(year)) |>
    pull(last_valid_year)
  
  return(latest_valid_year)
}

# ---------- ---------- ---------- ---------- ---------- ---------- -----
# ---------- Actual Shortage Tmap Functions ----------
# ---------- ---------- ---------- ---------- ---------- ---------- -----

# Filtering helper function for actual_shortage data.
actual_shortage_tmap_filtering <- function(){
  
  # Find the latest valid year available.
  last_year <- find_latest_valid_year(water_data$actual_shortage)
  
  # Computes average shortage across the last valid/available year actual_shortage data. Grouped by org_id & pwsid.
  mean_shortages <- water_data$actual_shortage |>
    filter(year(start_date) == last_year) |>
    group_by(org_id, pwsid) |>
    summarize(mean_level = mean(state_standard_shortage_level, na.rm = TRUE), .groups = "drop")
    
  # Join actual_shortage w/ spatial boundaries and cleaned supplier names.
  actual_shortage_by_district <- spatial_data$district_shape |>
    inner_join(mean_shortages, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id") |>
    group_by(org_id) |>
    slice(1) |>
    ungroup() 
  
  # Return filtered/cleaned df.
  return(actual_shortage_by_district)
}

actual_shortage_tmap <- function(){
  
  # Access filtered/cleaned actual_shortage data from helper function.
  actual_shortage_filtered <- tmap_data$actual_shortage_tmap_data
  
  # Build tmap of mean shortage levels by org_id & pwsid boundary.
  actual_shortage_tmap <-
    tm_shape(actual_shortage_filtered) +
    tm_fill("mean_level",
            fill.scale = tm_scale_intervals(
              values = "brewer.reds",
              style = "fixed",
              breaks = c(0:6, Inf)),
            fill.legend = tm_legend(
              title = paste0("Mean Shortage Level (", find_latest_valid_year(water_data$monthly_water_outlook),")")),
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    tm_view(
      legend.position = c("right", "bottom"), # Moves the legend to bottom right.
      control.position = c("left", "bottom") # Moves the layer picker to bottom left.
    )
  
  # Return tmap.
  return(actual_shortage_tmap)
}

# ---------- ---------- ---------- ---------- ---------- ---------- -----
# ---------- Monthly Water Outlook Tmap Functions ----------
# ---------- ---------- ---------- ---------- ---------- ---------- -----

monthly_outlook_tmap_filtering <- function(){
  
  # Find the latest valid year available.
  last_year <- find_latest_valid_year(water_data$monthly_water_outlook)
  
  # Computes average shortage across the last valid/availalbe year monthly_water_outlook data. Grouped by org_id & pwsid.
  mean_shortages <- water_data$monthly_water_outlook |>
    filter(year(forecast_start_date) == last_year) |>
    group_by(org_id, pwsid) |>
    summarize(mean_level = mean(state_standard_shortage_level, na.rm = TRUE), .groups = "drop")
  
  # Join monthly_water_outlook w/ spatial boundaries and cleaned supplier names.
  monthly_shortage_by_district <- spatial_data$district_shape |>
    inner_join(mean_shortages, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id") |>
    group_by(org_id) |>
    slice(1) |>
    ungroup() 
  
  # Return filtered/cleaned df.
  return(monthly_shortage_by_district)
}

monthly_outlook_tmap <- function(){
  
  # Access filtered/cleaned monthly_water_outlook data from helper function.
  monthly_outlook_filtered <- tmap_data$monthly_outlook_tmap_data
  
  # Build tmap of mean shortage levels by org_id & pwsid boundary.
  monthly_tmap <-
    tm_shape(monthly_outlook_filtered) +
    tm_fill("mean_level",
            fill.scale = tm_scale_intervals(
              values = "brewer.reds",
              style = "fixed",
              breaks = c(0:6, Inf)),
            fill.legend = tm_legend(title = paste0("Mean Shortage Level (", find_latest_valid_year(water_data$monthly_water_outlook),")")),
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() + 
    tm_view(
      legend.position = c("right", "bottom"), # Moves the legend to bottom right.
      control.position = c("left", "bottom") # Moves the layer picker to bottom left.
    )
  
  # Return tmap.
  return(monthly_tmap)
}

# ---------- ---------- ---------- ---------- ---------- ---------- -----
# ---------- Five Year Water Outlook Tmap Functions ----------
# ---------- ---------- ---------- ---------- ---------- ---------- -----

five_year_outlook_tmap_filtering <- function(){
  
  # Find the latest valid year available.
  last_year <- find_latest_valid_year(water_data$five_year_outlook)

  # Computes mean difference between supply & use across the last valid/availalbe year five_year_outlook data. Grouped by org_id & pwsid.
  mean_diffs_per_org <- water_data$five_year_outlook |>
    filter(year(forecast_start_date) == last_year) |>
    mutate(difference_supply_and_use = water_supplies_acre_feet - water_use_acre_feet) |>
    group_by(org_id, pwsid) |>
    summarise(mean_difference_supply_and_use = mean(difference_supply_and_use, na.rm = TRUE), .groups = "drop")
  
  # Join five_year_outlook w/ spatial boundaries and cleaned supplier names.
  five_year_outlook_by_district <- spatial_data$district_shape |>
    inner_join(mean_diffs_per_org, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id") |>
    group_by(org_id) |>
    slice(1) |>
    ungroup() 
  
  # Return filtered/cleaned df.
  return(five_year_outlook_by_district)
}

five_year_outlook_tmap <- function(){
  
  # Access filtered/cleaned five_year_outlook data from helper function.
  five_year_filtered <- tmap_data$five_year_outlook_tmap_data
  
  # Build tmap of the difference between supply & use by org_id & pwsid boundary.
  five_year_tmap <-
    tm_shape(five_year_filtered) +
    tm_fill("mean_difference_supply_and_use",
            fill.scale = tm_scale_intervals(
              values = "brewer.rd_bu",
              midpoint = 0),
            fill.legend = tm_legend(title = paste0("Supply Minus Use In Acre Ft (", find_latest_valid_year(water_data$five_year_outlook), ")")),
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    tm_view(
      legend.position = c("right", "bottom"), # Moves the legend to bottom right.
      control.position = c("left", "bottom") # Moves the layer picker to bottom left.
    )
  
  # Return tmap.
  return(five_year_tmap)
}

# ---------- ---------- ---------- ---------- ---------- ---------- -----
# ---------- Historical Production and Delivery Tmap Functions ----------
# ---------- ---------- ---------- ---------- ---------- ---------- -----

historical_production_tmap_filtering <- function(){
  
  # Find the latest valid year available.
  last_year <- find_latest_valid_year(water_data$historical_production)
  
  # Computes mean difference between production & delivery across the last valid/availalbe year historical_production data. Grouped by org_id & pwsid.
  difference_produced_vs_delivered <- water_data$historical_production |>
    filter(year(start_date) == last_year) |>
    pivot_wider(names_from = water_produced_or_delivered,
                values_from = quantity_acre_feet,
                values_fill = list(quantity_acre_feet = NA_real_)) |>
    group_by(org_id, pwsid, start_date) |>
    summarise(
      total_delivered = sum(`water delivered`, na.rm = TRUE),
      total_produced = sum(`water produced`,  na.rm = TRUE),
      difference = total_produced - total_delivered,
      .groups = "drop") |>
    group_by(org_id, pwsid) |>
    summarise(mean_difference_produced_and_delivered = mean(difference, na.rm = TRUE), .groups = "drop")
  
  # Join historical_production w/ spatial boundaries and cleaned supplier names.
  historical_production_by_district <- spatial_data$district_shape |>
    inner_join(difference_produced_vs_delivered, by = c("water_syst" = "pwsid")) |>
    inner_join(water_data$supplier_data, by = "org_id") |>
    group_by(org_id) |>
    slice(1) |>
    ungroup() 
  
  # Return filtered/cleaned df.
  return(historical_production_by_district)
}

historical_production_tmap <- function(){
  
  # Access filtered/cleaned historical_production data from helper function.
  historical_filtered <- tmap_data$historical_production_tmap_data
  
  # Build tmap of the difference between production & delivery by org_id & pwsid boundary.
  historical_tmap <-
    tm_shape(historical_filtered) +
    tm_fill("mean_difference_produced_and_delivered",
            fill.scale = tm_scale_intervals(
              values = "brewer.rd_bu",
              midpoint = 0),
            fill.legend = tm_legend(title = paste0("Produced Minus Delivered In Acre Ft (", find_latest_valid_year(water_data$historical_production), ")")),
            popup.vars = FALSE,
            id = "name_with_id") +
    tm_borders() +
    tm_view(
      legend.position = c("right", "bottom"), # Moves the legend to bottom right.
      control.position = c("left", "bottom") # Moves the layer picker to bottom left.
      )
  
  # Return tmap.
  return(historical_tmap)
}