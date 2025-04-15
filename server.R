#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load packages.
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(janitor)
library(shinyjs)
library(dplyr)
library(tools)

source("data_cleaning.R") # Load in data once.

# Subset our list of datasets for `source_name` data.
source_name <- water_data$source_name

server <- function(input, output, session) {
  
  #-----------------------------------------
  # Toggle Button Logic for Facility Type and Shortage Year Headers
  #-----------------------------------------
  
  # facility_type toggle button actions.
  observeEvent(input$toggle_facility_type, {
    if (input$toggle_facility_type %% 2 == 1) {
      
      # Toggle style to button on.
      removeClass(id = "toggle_facility_type", class = "btn-toggle-off")
      addClass(id = "toggle_facility_type", class = "btn-toggle-on")
    } else {
      
      # Toggle style to button off
      removeClass(id = "toggle_facility_type", class = "btn-toggle-on")
      addClass(id = "toggle_facility_type", class = "btn-toggle-off")
    }
  })
  
  # shortage_year toggle button actions.
  observeEvent(input$toggle_shortage_year, {
    if (input$toggle_shortage_year %% 2 == 1) {
      
      # Toggle style to button on.
      removeClass(id = "toggle_shortage_year", class = "btn-toggle-off")
      addClass(id = "toggle_shortage_year", class = "btn-toggle-on")
    } else {
      
      # Toggle style to button off.
      removeClass(id = "toggle_shortage_year", class = "btn-toggle-on")
      addClass(id = "toggle_shortage_year", class = "btn-toggle-off")
    }
  })
  
  #-----------------------------------------
  # Load Data: Interactive Tmap Data
  #-----------------------------------------
  
  # Convert our coordinates into spatial data for tmap
  source_geo <- spatial_data$source_geo |> 
    select(-c("source_facility_name", "source_facility_activity_status", "source_facility_availability", "source_facility_id"))
  
  # Convert our coordinates into spatial data for tmap
  source_geo <- st_as_sf(source_geo, coords = c("longitude", "latitude"), crs = "EPSG:4269") 
  st_crs(CA_polygon)
  
  # Change CRS of district_shape data
  district_shape <- spatial_data$district_shape |> 
    st_transform("EPSG:4269") |>
    st_make_valid()
  
  st_crs(district_shape)
  
  # Merge supplier names
  supplier_data <- read_csv(here("clean_names", "supplier_table.csv"))
  
  # Join shortage levels to district shapes
  districts_with_data <- district_shape |>
    inner_join(water_data$actual_shortage, by = c("water_syst" = "pwsid")) |>
    filter(end_date == max(end_date, na.rm = TRUE)) |>
    left_join(supplier_data, by = "org_id")  # Merge in supplier names
  
  districts_with_data <- districts_with_data |>
    mutate(name_label = paste0(supplier_name.y, " - ", water_syst), .before = objectid_1)

  #------------------------------------------------
  # Filter your data reactively based on water source selection.
  #------------------------------------------------
  filtered_source_geo <- reactive({
    
    # Starting with our spatial data coordinates we set to crs = 4269.
    data <- source_geo 
    colnames(data)
    
    # If the user has picked facility types, filter by them.
    if (!is.null(input$facility_type) && length(input$facility_type) > 0) {
      data <- data |> 
        filter(toTitleCase(.data$source_facility_type) %in% input$facility_type)
    } else{
      data <- data[0, ] # Nothing by default.
    }
    data
  })

  #------------------------------------------------
  # Render Tmap of California
  #------------------------------------------------
  
  # Render Tmap of california.
  output$shortage_map <- renderTmap({
    tmap_mode("view")  # interactive mode.
    
    tmap_options(check.and.fix = TRUE)
    
    # Build base layers.
    base_map <- 
      tm_shape(districts_with_data) +
      tm_fill("state_standard_shortage_level", 
              title = "Shortage Level", 
              palette = "Reds", 
              style = "cat",
              labels = c("0", "1", "2", "3", "4")) +
      tm_borders()
    
    # Filtered data.
    filtered_options <- filtered_source_geo()
    
    base_map
  })
  
  #------------------------------------------------
  # Render Function for Shortage Level from actual shortage.
  #------------------------------------------------
  
  actual_shortage_fun <- function(id) {
    df <- water_data$actual_shortage %>%
      filter(org_id == id)
    
    ggplot(df, aes(x = start_date, 
                   y = state_standard_shortage_level)) +
      geom_col(fill = "lightblue", color = "black") +
      # Force the y-axis to range from 0 to 6 (since you mentioned 0 through 6)
      scale_y_continuous(limits = c(0, 6), breaks = 0:6) +
      labs(
        x = "Forecast Start Date",
        y = "Shortage Level (1–6)",
        title = paste("Forecasted shortage level for", id)
      ) +
      theme_minimal()
  }
  
  # Populate Org_id dropdown with unique list of id's.
  observe({
    all_ids <- sort(unique(water_data$actual_shortage$org_id))
    updateSelectInput(session, "org_id", choices = all_ids, selected = all_ids[1])
  })
  
  output$supply_demand_plots <- renderPlot({
    actual_shortage_fun(input$org_id)
  })
}

