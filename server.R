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
library(bslib)
library(shinyWidgets)
library(paletteer)

source("data_cleaning.R") # Load in data once.
source("functions/dashboard_functions.R") # Load dashboard plot functions.

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
    mutate(name_pwsid_label = paste0(supplier_name.y, " - ", water_syst), .before = objectid_1)
  
  districts_with_data <- districts_with_data |>
    mutate(name_org_label = paste0(supplier_name.y, " - ", org_id), .before = objectid_1)
  
  #------------------------------------------------
  # Search Bar Code
  #------------------------------------------------
  
  # Updates the ORG_ID's & names from our districts. 
  observeEvent(input$dataset_selector, {
    df <- water_data[[input$dataset_selector]]
    org_names <- sort(unique(districts_with_data$name_org_label))
    
    updateSelectizeInput(session, "search_bar",
                         choices  = org_names, selected = character(0))
  })
  
  #-----------T-------------------------------------
  # Render Tmap of California
  #------------------------------------------------
  
  # Render Tmap of california.
  output$shortage_map <- renderTmap({
    tmap_mode("view")  # interactive mode.
    
    # tmap_options(check.and.fix = TRUE)
    
    # Build base layers.
    base_map <- 
      tm_shape(districts_with_data) +
      tm_fill("state_standard_shortage_level", 
              title = "Shortage Level", 
              palette = "Reds", 
              style = "cat",
              labels = c("0", "1", "2", "3", "4")) +
      tm_borders() + 
      
      # move the legend into the bottom right.
      tm_view(
      view.legend.position  = c("right", "bottom"),   # moves the legend to bottom right.
      control.position = c("left", "bottom")    # moves the layer picker there too.
    )
    
    base_map
  })
  
  # actual_shortage_fun <- function(id) {
  #   df <- water_data$actual_shortage %>%
  #     filter(org_id == id)
  #   
  #   ggplot(df, aes(x = start_date, 
  #                  y = state_standard_shortage_level)) +
  #     geom_col(fill = "lightblue", color = "black") +
  #     # Force the y-axis to range from 0 to 6 (since you mentioned 0 through 6)
  #     scale_y_continuous(limits = c(0, 6), breaks = 0:6) +
  #     labs(
  #       x = "Forecast Start Date",
  #       y = "Shortage Level (1–6)",
  #       title = paste("Forecasted shortage level for", id)
  #     ) +
  #     theme_minimal()
  #   
  # }
  
  #------------------------------------------------
  # Plot outputs widget + widgets
  #------------------------------------------------
  
  # ---------- Column 1: Inside Box 1: Row 1 (Dataset Selection Dropdown) ----------
  
  # Populate Dataset selection dropdown with our dataset names.
  observe({
    # all_dataset_names <- toTitleCase(gsub("_", " ", ((sort(names(water_data)))))) # Using gsub to clean snake case to title case.
    all_dataset_names <- sort(names(water_data)) # sort dataset names.
    updateSelectInput(session, "dataset_selector", choices = all_dataset_names, selected = all_dataset_names[2]) # update dataset dropdown with new selection.
  })
  
  # ---------- Initializing UI Date Pickers ----------
  
  # Helper function for date selection. **Reactive** to dataset choice.
  date_range <- reactive({
    req(input$dataset_selector)
    df <- water_data[[input$dataset_selector]]
    
    # actual vs forecast
    if (input$dataset_selector %in% 
        c("actual_shortage","historical_production","monthly_water_outlook")) {
      dates <- as.Date(df$start_date %||% df$forecast_start_date)
    } else {
      dates <- NULL
    }
    
    list(
      minDate = min(dates),
      maxDate = max(dates),
      default = min(dates)
    )
  })
  
  # Whenever the dataset changes, reset org_id + date or year pickers
  observeEvent(input$dataset_selector, {
    df <- water_data[[input$dataset_selector]]
    orgs <- sort(unique(df$org_id))
    
    updateSelectInput(session, "org_id",
                      choices  = orgs,
                      selected = orgs[1])
    
    # date‐based datasets
    if (input$dataset_selector %in% 
        c("actual_shortage","monthly_water_outlook","historical_production")) {
      
      dr <- date_range()
      
      # set the start‐picker to the earliest month,
      # limited between overall min/max
      updateAirDateInput(
        session, "date_picker_start",
        value = dr$default,
        options = list(
          minDate = dr$minDate,
          maxDate = dr$maxDate
        )
      )
      
      # set the end‐picker to the latest month,
      # same overall bounds for now
      updateAirDateInput(
        session, "date_picker_end",
        value = dr$default,
        options = list(
          minDate = dr$minDate,
          maxDate = dr$maxDate
        )
      )
      
      # five‐year outlook -> year dropdown
    } else if (input$dataset_selector == "five_year_outlook") {
      yrs <- sort(unique(format(as.Date(df$forecast_start_date), "%Y")))
      updateSelectInput(session, "year",
                        choices  = yrs,
                        selected = yrs[1])
    }
  })
  
  observeEvent(input$date_picker_start, {
    start <- as.Date(input$date_picker_start)
    dr <- date_range()
    updateAirDateInput(
      session, "date_picker_end",
      options = list(minDate = start,
                     maxDate = dr$maxDate)
    )
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                          Reactive Plot UI Output                         ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # -------- START  Column 1: Inside Box 1: Row 2 (Reactive Plot Controls) -------
  
  # Render plot controls here.
  output$plot_controls<- renderUI({
    req(input$dataset_selector)
    dr <- date_range()
    
    # Depending on the dataset you select the UI controls will change.
    # Below we have the server UI for each selected dataset.
    switch(input$dataset_selector,
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Actual Shortage Plot  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           
           # Actual Shortage only uses org_id and date (as [start, end])
           "actual_shortage" = fluidRow(
             
             # Org_id selection drop down
             column(4,
                    selectInput("org_id", "Select Org ID", choices = NULL)
             ),
             
             # Month-Year end selection drop down
             column(4,
                    airDatepickerInput(
                      "date_picker_start", 
                      label = "Start month",
                      view = "months", 
                      minView = "months",
                      dateFormat = "yyyy-MM",
                      value = dr$default,
                      minDate = dr$minDate,
                      maxDate = dr$maxDate
                    )
             ),
             
             # Month-Year end selection drop down
             column(4,
                    airDatepickerInput(
                      "date_picker_end", 
                      label = "End month",
                      view = "months",   
                      minView = "months",
                      dateFormat = "yyyy-MM",
                      value = dr$maxDate,
                      minDate = dr$minDate,
                      maxDate = dr$maxDate
                    )
             )
           ), # END ACTUAL SHORTAGE WIDGET
           
           
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Monthly Water Outlook Plot  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           
           "monthly_water_outlook" = fluidRow(
             
             # Org_id selection drop down
             column(4,
                    selectInput("org_id", "Select Org ID", choices = NULL)
             ),
             
             # Month-Year end selection drop down
             column(4,
                    airDatepickerInput(
                      "date_picker_start", 
                      label = "Start month",
                      view = "months", 
                      minView = "months",
                      dateFormat = "yyyy-MM",
                      value = dr$default,
                      minDate = dr$minDate,
                      maxDate = dr$maxDate
                    )
             ),
             
             # Month-Year end selection drop down
             column(4,
                    airDatepickerInput(
                      "date_picker_end", 
                      label = "End month",
                      view = "months",   
                      minView = "months",
                      dateFormat = "yyyy-MM",
                      value = dr$maxDate,
                      minDate = dr$minDate,
                      maxDate = dr$maxDate
                    )
             )
           ), ### END MONTHLY PLOT WIDGET
           
           
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Five Year Outlook Plot  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           "five_year_outlook" = fluidRow(
             
             # Org_id selection drop down
             column(4,
                    selectInput("org_id", "Select Org ID", choices = NULL)
             ),
             
             # Year start selection drop down
             column(4,
                    airDatepickerInput(
                      "date_picker_start", 
                      label = "Start year",
                      view = "years", 
                      minView = "years",
                      dateFormat = "yyyy",
                      value = dr$default,
                      minDate = dr$minDate,
                      maxDate = dr$maxDate
                    )
             ),
             
             # Year end selection drop down
             column(4,
                    airDatepickerInput(
                      "date_picker_end", 
                      label = "End year",
                      view = "years",   
                      minView = "years",
                      dateFormat = "yyyy",
                      value = dr$maxDate,
                      minDate = dr$minDate,
                      maxDate = dr$maxDate
                    )
             )
           ), # END FIVE YEAR PLOT WIDGET
           
           
           
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Historical Production Plot  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           "historical_production" = tagList(
             
             
             fluidRow(
               
               # Org_id selection drop down
               column(4,
                      selectInput("org_id", "Select Org ID", choices = NULL)
               ),
               
               # Month-Year end selection drop down
               column(4,
                      airDatepickerInput(
                        "date_picker_start", 
                        label = "Start month",
                        view = "months", 
                        minView = "months",
                        dateFormat = "yyyy-MM",
                        value = dr$default,
                        minDate = dr$minDate,
                        maxDate = dr$maxDate
                      )
               ),
               
               # Month-Year end selection drop down
               column(4,
                      airDatepickerInput(
                        "date_picker_end", 
                        label = "End month",
                        view = "months",   
                        minView = "months",
                        dateFormat = "yyyy-MM",
                        value = dr$maxDate,
                        minDate = dr$minDate,
                        maxDate = dr$maxDate
                      )
               )
             ),
             
             # Second row for water type selection
             fluidRow(
               
               column(6,
                      
                      # Selecting water type
                      selectInput("delivered_type", "Select Deliver Type", c("Agriculture",
                                                          "Single-Family Residential",
                                                          "Commercial/Institutional",
                                                          "Industrial",
                                                          "Landscape Irrigation",
                                                          "Multi-Family Residential",
                                                          "Other",
                                                          "Other Pws",
                                                          "Total"),
                                  
                                  multiple = TRUE, # Able to select multiple "Types"
                                  width = "100%")),  # Selection bar covers all of the fitted area
               
               column(6,
                      
                      # Selecting Production Water Type
                      selectInput("produced_type", "Select Produced Type",
                                  c("Recycled", "Surface Water", "Groundwater Wells", 
                                    "Non-Potable (Total Excluded Recycled)",  "Purchased Or Received From Another Pws",
                                    "Sold To Another Pws","Non-Potable Water Sold To Another Pws", "Total"
                                    ),
                                  multiple = TRUE,
                                  width = "100%"))
               
               
             ) # END SECOND ROW Water type selection
             
             
             
           ) # END HISTORICAL PRODUCTION WIDGET
    )
  }) # -------- END Column 1: Inside Box 1: Row 2 (Reactive Plot Controls) -------
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                          Reactive Plot Rendering                         ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  combined_water_types <- reactive({
    delivered <- input$delivered_type
    produced  <- input$produced_type
    
    # If both are empty, return NULL
    if ((is.null(delivered) || length(delivered) == 0) && 
        (is.null(produced)  || length(produced) == 0)) {
      return(NULL)
    }
    
    # If only delivered selected
    if (length(delivered) > 0 && (is.null(produced) || length(produced) == 0)) {
      return(delivered)
    }
    
    # If only produced selected
    if (length(produced) > 0 && (is.null(delivered) || length(delivered) == 0)) {
      return(produced)
    }
    
    # If both are selected
    return(c(delivered, produced))
  })
  
  
  # -------START Column 1: Inside Box 1: Row 3 (Plot Display) --------
  
  # This is where we update the plot functions based on the selection of datasets.
  output$plot_output <- renderPlot({
    
    
    # Need dataset, org_id, start/end dates.
    req(input$dataset_selector, input$org_id, input$date_picker_start, input$date_picker_end)
    
    # Find dataset based on name.
    selected_name <- input$dataset_selector
    selected_df <- water_data[[selected_name]]
    
    # Extra Requirement for Historical_production
    if (selected_name == "historical_production"){
      req(combined_water_types())
    }
    
    # Filtering the dates to the proper input format. 
    # Format the date selection from the date picker to ensure consistency across all datasets
    start_ym <- format(as.Date(input$date_picker_start), "%Y-%m")
    end_ym   <- format(as.Date(input$date_picker_end),   "%Y-%m")
    
    # Filtering for years to the proper input format
    # This is used for the "Five_year_outlook" as it is displayed in years
    start_y <- format(as.Date(input$date_picker_start), "%Y")
    end_y  <- format(as.Date(input$date_picker_end),   "%Y")
    
    print(c(input$date_picker_start, input$date_picker_end))
    print(c(start_ym, end_ym))
    
    water_types <- combined_water_types()
    
    # Switch statement to change function based on dataset
    plot <- switch(selected_name,
                   
                   # --- Monthly Water Plot output Function --- #
                   "monthly_water_outlook" = monthly_plot_function(input$org_id, c(start_ym, end_ym)),
                   
                   # --- Five Year Plot Output Function --- #
                   "five_year_outlook" = five_year_plot(input$org_id, c(start_y, end_y)),
                   
                   # --- Historical Production Plot Output Function --- # 
                   "historical_production" = hist_plot_function(input$org_id, c(start_ym, end_ym), water_types),
                  
                  # --- Actual Water Shortage Plot Output Function --- # 
                   "actual_shortage" = actual_plot_function(input$org_id, c(start_ym, end_ym))
    )
    
    # Output plot
    plot
  }) # -------END Column 1: Inside Box 1: Row 3 (Plot Display) --------

  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                       Summary Statistics Reactive UI                     ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# START Summary Stats UI 
output$summary_stats <- renderUI({
  
  # Required dataset for UI output
  req(input$dataset_selector)
  
  # Switch function containing UI properties for each dataset
  switch(input$dataset_selector,
         
         
         ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ##  ~ Monthly Water Outlook  ----
         ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         
         # START Monthly Water Summary Stats 
         "monthly_water_outlook" = tagList(
           
           # First row containing title & Info Button 
           tagList(
             
             # Info button icon
             div(
               style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
               tags$span(actionButton("info_summary", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             
             # Info button information
             bsPopover(
               id = "info_summary",
               title = "Information",
               content = "Displaying the summary stats.",
               placement = "right",
               trigger = "hover",
               options = list(container = "body")
             ),
             
             # Title after info button
             h3("Summary Statistics", style = "text-align: center;")
             
             
           ), # END First row
           
           # Second row containing sub categories Value, # months
           fluidRow(
             
             # Dividing into thirds, first is empty column
             column(4),
             
             # Second column: Value(Acre-Feet)
             column(4, h4("Value (Acre-Feet)")),
             
             # Third column: Number of Months
             column(4, h4("Number of Months"))
           ),
           
           # Third row containing shortage/surplus values & # months
           fluidRow(
             
             # First column: shortage/surplus title
             column(4, h4("Shortage/Surplus")),
             
             # Second column: The actual Value in acre-feet
             column(4),
             
             # Third column: Number of months data present for shortage/surplus
             column(4)
             
           ),
           
           # Fourth row containing supply augmentation values & # months
           fluidRow(
             
             # First column: supply augmentation title
             column(4, h4("Supply Augmentation")),
             
             # Second column: The actual value in acre-feet
             column(4),
             
             # Third column: Number of months data present for agumentation
             column(4)
           ),
           
           # Fifth row containing demand reduction values & # months
           fluidRow(
             
             # First column: demand reduction title
             column(4, h4("Demand Reduction")),
             
             # Second column: The actual value in acre-feet
             column(4),
             
             # Third column: Number of months data present for reduction
             column(4)
             
           )
           
           
         ), # END Monthly Water  
         
         
         ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ##  ~ Five Year Outlook  ----
         ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
         
         # START Five Year Summary Stats
         "five_year_outlook" = tagList(
           
           # First row containing title
           tagList(
             
             # Info button icon
             div(
               style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
               tags$span(actionButton("info_summary", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             
             # Info button information
             bsPopover(
               id = "info_summary",
               title = "Information",
               content = "Displaying the summary stats.",
               placement = "right",
               trigger = "hover",
               options = list(container = "body")
             ),
             
             # Title after info button
             h3("Summary Statistics", style = "text-align: center;")
             
           ), # END first row
           
           # Second Row containing total use & supply
           fluidRow(
             
             # First column: total use
             column(6,
                    h4("Total Use (Acre-Feet)")
             ),
             
             # Second column: total supply
             column(6,
                    h4("Total Supply (Acre-Feet)")
             )
             
           ), # END Second Row
           
           # Third row containing total reduction & augmentation
           fluidRow(
             
             # First column: total reduction
             column(6,
                    h4("Total Reduction (Acre-Feet)")
             ),
             
             # Second column: total augmentation 
             column(6,
                    h4("Total Augmentation (Acre-Feet)")
             )
             
           )
           
         ), # END Five Year
         
         
         ##~~~~~~~~~~~~~~~~~~~~~~~~~
         ##  ~ Actual Shortage  ----
         ##~~~~~~~~~~~~~~~~~~~~~~~~~
         
         # START Actual Shortage Summary Stats
         "actual_shortage" = tagList(
           
           # First row containing title
           tagList(
             
             # Info button icon
             div(
               style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
               tags$span(actionButton("info_summary", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             
             # Info button information
             bsPopover(
               id = "info_summary",
               title = "Information",
               content = "Displaying the summary stats.",
               placement = "right",
               trigger = "hover",
               options = list(container = "body")
             ),
             
             # Title after info button
             h3("Summary Statistics", style = "text-align: center;")
             
           )
           
         ), # END Actual Shortage
         
         
         ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         ##  ~ Historical Production  ----
         ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         
         # START Historical Production Summary Stats
         "historical_production" = tagList(
           
           # First row containing title
           tagList(
             
             # Info button icon
             div(
               style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
               tags$span(actionButton("info_summary", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             
             # Info button information
             bsPopover(
               id = "info_summary",
               title = "Information",
               content = "Displaying the summary stats.",
               placement = "right",
               trigger = "hover",
               options = list(container = "body")
             ),
             
             # Title after info button
             h3("Summary Statistics", style = "text-align: center;")
             
           )
           
         ) # END Historical Production
         )
})

  
  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            NA Values Reactive UI                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # START NA Values Reactive UI
  output$na_values <- renderUI({
    
    # Required dataset for UI Output
    req(input$dataset_selector)
    
    # START Reactive NA UI
    switch(input$dataset_selector,
           
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Monthly Water Outlook  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           
           "monthly_water_outlook" = tagList(
             
             # Info button icon
             div(
               style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
               tags$span(actionButton("info_NA", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             
             # Info button information
             bsPopover(
               id = "info_NA",
               title = "Information",
               content = "Displaying the missing information.",
               placement = "left",
               trigger = "hover",
               options = list(container = "body")
             ),
             
             # Title after info button
             h3("Missing Information", style = "text-align: center;")
             
           ), # END Monthly Outlook
           
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Five Year Outlook  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
           
           "five_year_outlook" = tagList(
             
             # Info button icon
             div(
               style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
               tags$span(actionButton("info_NA", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             
             # Info button information
             bsPopover(
               id = "info_NA",
               title = "Information",
               content = "Displaying the missing information.",
               placement = "left",
               trigger = "hover",
               options = list(container = "body")
             ),
             
             # Title after info button
             h3("Missing Information", style = "text-align: center;")
             
           ), # END Five Year Outlook
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Actual Shortage  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~
           
           "actual_shortage" = tagList(
             
             # Info button icon
             div(
               style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
               tags$span(actionButton("info_NA", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             
             # Info button information
             bsPopover(
               id = "info_NA",
               title = "Information",
               content = "Displaying the missing information.",
               placement = "left",
               trigger = "hover",
               options = list(container = "body")
             ),
             
             # Title after info button
             h3("Missing Information", style = "text-align: center;")
             
           ), # END Actual Shortage 
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Historical Production  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           
           "historical_production" = tagList(
             
             # Info button icon
             div(
               style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
               tags$span(actionButton("info_NA", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             
             # Info button information
             bsPopover(
               id = "info_NA",
               title = "Information",
               content = "Displaying the missing information.",
               placement = "left",
               trigger = "hover",
               options = list(container = "body")
             ),
             
             # Title after info button
             h3("Missing Information", style = "text-align: center;")
             
           ), # END Historical Production 
           
           ) # END Reactive NA UI
  })

}

