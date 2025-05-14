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
source("functions/calculate_na_summary.R")
source("functions/tmap_plot_functions.R")
source("functions/sum_stat_functions.R")

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
  
  #------------------------------------------------
  # Search Bar Code
  #------------------------------------------------
  
  # ----- Fill in via on-click of our Tmap -----
  # observeEvent(input$shortage_map_shape_click, {
  #   
  #   # map selection id's are the labels we provided for each location with 
  #   # periods replacing spaces & special characters.
  #   raw_id <- input$shortage_map_shape_click$id
  #   
  #   # fixing the incorrectly formatted labels back to our format so we can
  #   # access the corresponding observational data (aka grab the org_id).
  #   formatted_label <- raw_id %>%
  #     str_replace_all("\\.\\.\\.", " - ") %>%
  #     str_replace_all("\\.", " ")
  #   
  #   # grabbing the full row observations based on the searched name+org label.
  #   matched_row <- spatial_data$actual_shortage_by_district %>%
  #     filter(name_pwsid_label == formatted_label) %>%
  #     slice(1)
  #   
  #   # grab the org_id from the row observation from our search.
  #   matched_org_id <- matched_row$org_id
  #   
  #   # fill in search bar based on selection.
  #   if (!is.null(matched_org_id) && length(matched_org_id) > 0) {
  #     updateSelectizeInput(session, "search_bar", selected = matched_org_id)
  #   }
  # })
  
  # ----- Fill in via search -----
  observe({
    
    # Linking org_id+name labels to org_id values.
    org_choices <- setNames(water_data$supplier_data$org_id, water_data$supplier_data$name_with_id)
    
    # Saving current selection.
    current_selection <- isolate(input$search_bar)
    
    # If the current selection is still valid keep it else reset.
    if (!is.null(current_selection) && current_selection %in% org_choices) {
      updateSelectizeInput(session, "search_bar",
                           choices = org_choices, selected = current_selection)
    } else {
      updateSelectizeInput(session, "search_bar",
                           choices = org_choices, selected = character(0))
    }
  })
  
  #------------------------------------------------
  # Render Tmap of California
  #------------------------------------------------
  
  # Render plot controls here.
  output$tmap_by_dataset <- renderTmap({
    
    # Need dataset, org_id, start/end dates.
    req(input$dataset_selector, input$search_bar)
    
    # Depending on the dataset you select the Tmap render will change.
    switch(input$dataset_selector,
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Actual Shortage Tmap  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           "actual_shortage" = actual_shortage_tmap(),
             
             ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             ##  ~ Monthly Water Outlook Tmap  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           "monthly_water_outlook" = monthly_outlook_tmap(),
             
             ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             ##  ~ Five Year Outlook Tmap  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           "five_year_outlook" = five_year_outlook_tmap(),
             
             ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             ##  ~ Historical Production Tmap  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           "historical_production" = historical_production_tmap()
           )
    })
  
  #------------------------------------------------
  # Plot outputs widget + widgets
  #------------------------------------------------
  
  # ---------- Column 1: Inside Box 1: Row 1 (Dataset Selection Dropdown) ----------
  
  # Populate Dataset selection dropdown with our dataset names.
  
  #old one (can delete): observe({
  #   # all_dataset_names <- toTitleCase(gsub("_", " ", ((sort(names(water_data)))))) # Using gsub to clean snake case to title case.
  #   all_dataset_names <- sort(names(water_data)) # sort dataset names.
  #   updateSelectInput(session, "dataset_selector", choices = all_dataset_names, selected = all_dataset_names[2]) # update dataset dropdown with new selection.
  # })
  
  observe({
  # Named datatype: display name (label) = internal_value
    dataset_labels <- c(
      "Reported Monthly Shortage Levels" = "actual_shortage",
      "5 Year Outlook - Water Surplus/Shortage" = "five_year_outlook",
      "Historical Water Production/Delivery" = "historical_production",
      "Monthly Surplus/Shortage Forecast" = "monthly_water_outlook"
    )

  updateSelectInput(session,
                    "dataset_selector",
                    choices = dataset_labels,
                    selected = "actual_shortage")
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
  
  year_range <- reactive({
    req(input$dataset_selector)
    df <- water_data[[input$dataset_selector]]
    
    if (input$dataset_selector == "five_year_outlook" && !is.null(df$forecast_start_date)) {
      dates <- as.Date(df$forecast_start_date)
      
      return(list(
        minDate = min(dates),
        maxDate = max(dates),
        default = min(dates)
      ))
    }
    
    # Fallback
    return(list(minDate = NULL, maxDate = NULL, default = NULL))
  })
  
  # Whenever the dataset changes, reset date or year pickers
  observeEvent(input$dataset_selector, {
    df <- water_data[[input$dataset_selector]]
    
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
      
      yr <- year_range()
      
      updateAirDateInput(
        session, "date_picker_start",
        value = yr$default,
        options = list(
          minDate = yr$minDate,
          maxDate = yr$maxDate
        )
      )
      
      updateAirDateInput(
        session, "date_picker_end",
        value = yr$maxDate,
        options = list(
          minDate = yr$minDate,
          maxDate = yr$maxDate
        )
      )
    }
  })
  
  observeEvent(input$date_picker_start, {
    req(input$date_picker_start)
    
    start <- as.Date(input$date_picker_start)
    dr <- date_range()
    
    # Get current end date or fall back to max
    current_end <- input$date_picker_end
    end <- if (!is.null(current_end)) as.Date(current_end) else dr$maxDate
    
    # If out of bounds, use max date
    if (end < start || end > dr$maxDate) {
      end <- dr$maxDate
    }
    
    updateAirDateInput(
      session, "date_picker_end",
      value = end,
      options = list(
        minDate = start,
        maxDate = dr$maxDate
      )
    )
  })
  
  observeEvent({
    input$date_picker_start
    input$dataset_selector
  }, {
    req(input$dataset_selector == "five_year_outlook")
    req(input$date_picker_start)
    
    start <- as.Date(input$date_picker_start)
    yr <- year_range()
    
    current_end <- input$date_picker_end
    end <- if (!is.null(current_end)) as.Date(current_end) else yr$maxDate
    
    if (end < start || end > yr$maxDate) {
      end <- yr$maxDate
    }
    
    updateAirDateInput(
      session, "date_picker_end",
      value = end,
      options = list(
        minDate = start,
        maxDate = yr$maxDate
      )
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
    yr <- year_range()
    
    # Depending on the dataset you select the UI controls will change.
    # Below we have the server UI for each selected dataset.
    switch(input$dataset_selector,
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Actual Shortage Plot  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           
           # Actual Shortage only uses org_id and date (as [start, end])
           "actual_shortage" = fluidRow(
             
             
             # Month-Year end selection drop down
             column(6,
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
             column(6,
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
             ),
             
           ), # END ACTUAL SHORTAGE WIDGET
           
           
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Monthly Water Outlook Plot  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           
           "monthly_water_outlook" = fluidRow(
             
             # Month-Year end selection drop down
             column(6,
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
             column(6,
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

             # Year start selection drop down
             column(6,
                    airDatepickerInput(
                      "date_picker_start", 
                      label = "Start year",
                      view = "years", 
                      minView = "years",
                      dateFormat = "yyyy",
                      value = yr$default,
                      minDate = yr$minDate,
                      maxDate = yr$maxDate
                    )
             ),
             
             # Year end selection drop down
             column(6,
                    airDatepickerInput(
                      "date_picker_end", 
                      label = "End year",
                      view = "years",   
                      minView = "years",
                      dateFormat = "yyyy",
                      value = yr$maxDate,
                      minDate = yr$minDate,
                      maxDate = yr$maxDate
                    )
             )
           ), # END FIVE YEAR PLOT WIDGET
           
           
           
           
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           ##  ~ Historical Production Plot  ----
           ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           "historical_production" = tagList(
             
             
             fluidRow(
               
               # Month-Year end selection drop down
               column(6,
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
               column(6,
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
               
               # column(6,
               #        
               #        # Selecting water type
               #        selectInput("delivered_type", "Select Deliver Type", c("Agriculture",
               #                                                               "Single-Family Residential",
               #                                                               "Commercial/Institutional",
               #                                                               "Industrial",
               #                                                               "Landscape Irrigation",
               #                                                               "Multi-Family Residential",
               #                                                               "Other",
               #                                                               "Other Pws",
               #                                                               "Total"),
               #                    
               #                    multiple = TRUE, # Able to select multiple "Types"
               #                    width = "100%")),  # Selection bar covers all of the fitted area
               # 
               # column(6,
               #        
               #        # Selecting Production Water Type
               #        selectInput("produced_type", "Select Produced Type",
               #                    c("Recycled", "Surface Water", "Groundwater Wells", 
               #                      "Non-Potable (Total Excluded Recycled)",  "Purchased Or Received From Another Pws",
               #                      "Sold To Another Pws","Non-Potable Water Sold To Another Pws", "Total"
               #                    ),
               #                    multiple = TRUE,
               #                    width = "100%"))
               
               
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
    include_total <- input$include_total  # checkboxGroupInput value
    
    types <- c()
    
    # Add delivered and produced if they exist
    if (!is.null(delivered)) types <- c(types, delivered)
    if (!is.null(produced))  types <- c(types, produced)
    
    # Add "Total" if checkbox selected
    if (!is.null(include_total) && "Total" %in% include_total) {
      types <- c(types, "Total")
    }
    
    if (length(types) == 0) return(NULL)
    
    return(unique(types))
  })
  
  
  # to make summary stats possible with drop down as is
  combined_water_types_filtered <- reactive({
    types <- combined_water_types()
    
    # If NULL, return NULL immediately
    if (is.null(types)) return(NULL)
    
    # Remove "Total" from the list
    types <- types[types != "Total"]
    
    # If no types left after removing, return NULL
    if (length(types) == 0) return(NULL)
    
    return(types)
  })
  
  
  # -------START Column 1: Inside Box 1: Row 3 (Plot Display) --------
  
  # This is where we update the plot functions based on the selection of datasets.
  output$plot_output <- renderPlot({
    
    
    # Need dataset, org_id, start/end dates.
    req(input$dataset_selector, input$search_bar, input$date_picker_start, input$date_picker_end)
    
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
                   "monthly_water_outlook" = monthly_plot_function(input$search_bar, c(start_ym, end_ym)),
                   
                   # --- Five Year Plot Output Function --- #
                   "five_year_outlook" = five_year_plot(input$search_bar, c(start_y, end_y)),
                   
                   # --- Historical Production Plot Output Function --- # 
                   "historical_production" = hist_plot_function(input$search_bar, c(start_ym, end_ym), water_types),
                   
                   # --- Actual Water Shortage Plot Output Function --- # 
                   "actual_shortage" = actual_plot_function(input$search_bar, c(start_ym, end_ym))
    )
    
    # Output plot
    plot
  }) # -------END Column 1: Inside Box 1: Row 3 (Plot Display) --------
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                       Summary Statistics Reactive UI                     ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                       Summary Statistics Section                         ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  
  output$summary_stats <- renderUI({
    req(input$dataset_selector)
    card(
      full_screen = TRUE,
      style = "overflow-y: auto; max-height: 290px; padding: 10px;",
      div(
        style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
        tags$span(actionButton("info_summary", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
      ),
      bsPopover(id = "info_summary", title = "Information", content = "Displaying summary statistics for the selected dataset.", placement = "right", trigger = "hover", options = list(container = "body")),
      div(style = "text-align: center; margin-bottom: 10px;", h3("Summary Statistics")),
      switch(input$dataset_selector,
             "five_year_outlook" = summary_five_year_ui(),
             "monthly_water_outlook" = summary_monthly_ui(),
             "actual_shortage" = summary_actual_ui(),
             "historical_production" = summary_historical_ui()
      )
    )
  })
  
  summary_five_year_ui <- function() {
    tagList(
      fluidRow(column(6, valueBoxOutput("fiveyr_use")), column(6, valueBoxOutput("fiveyr_supply"))),
      fluidRow(column(6, valueBoxOutput("fiveyr_aug")), column(6, valueBoxOutput("fiveyr_red")))
    )
  }
  
  summary_monthly_ui <- function() {
    tagList(
      fluidRow(column(6, valueBoxOutput("monthly_shortage_value")), column(6, valueBoxOutput("monthly_shortage_months"))),
      fluidRow(column(6, valueBoxOutput("monthly_aug_value")), column(6, valueBoxOutput("monthly_aug_months"))),
      fluidRow(column(6, valueBoxOutput("monthly_red_value")), column(6, valueBoxOutput("monthly_red_months")))
    )
  }
  
  summary_actual_ui <- function() {
    tagList(
      fluidRow(column(12, valueBoxOutput("average_shortage"))),
      fluidRow(lapply(0:2, function(i) column(4, valueBoxOutput(paste0("shortage_level_", i))))),
      fluidRow(lapply(3:5, function(i) column(4, valueBoxOutput(paste0("shortage_level_", i))))),
      fluidRow(column(4, valueBoxOutput("shortage_level_6")))
    )
  }
  
  summary_historical_ui <- function() {
    tagList(
      fluidRow(
        column(6, valueBoxOutput("total_produced_box")),
        column(6, valueBoxOutput("total_delivered_box"))
      ),
      br(),
      fluidRow(
        column(12, uiOutput("hist_value_boxes"))
      )
    )
  }
  
 
  # -- 5 yr value box ---
  render_five_value_box <- function(label) {
    renderValueBox({
      req(input$search_bar, input$date_picker_start, input$date_picker_end)
      start_year <- year(as.Date(input$date_picker_start))
      end_year   <- year(as.Date(input$date_picker_end))
      
      val <- five_values_function_sum_stat(input$search_bar, c(start_year, end_year)) %>%
        filter(use_supply_aug_red == label) %>%
        pull(total_value)
      
      valueBox(
        value = ifelse(length(val) > 0, scales::comma(val), "0"),
        subtitle = pretty_label(label),
        color = "blue"
      )
    })
  }
  
  output$fiveyr_use    <- render_five_value_box("water_use_acre_feet")
  output$fiveyr_supply <- render_five_value_box("water_supplies_acre_feet")
  output$fiveyr_aug    <- render_five_value_box("benefit_supply_augmentation_acre_feet")
  output$fiveyr_red    <- render_five_value_box("benefit_demand_reduction_acre_feet")
  
  
  # --- Monthly Water Outlook Value Boxes ---

  render_monthly_value <- function(label, color) {
    renderValueBox({
      req(input$search_bar, input$date_picker_start, input$date_picker_end)
      val <- monthly_values_function_sum_stat(input$search_bar, c(input$date_picker_start, input$date_picker_end)) %>%
        filter(use_supply_aug_red == label) %>%
        pull(total_acre_feet)
      
      valueBox(
        value = ifelse(length(val) > 0, scales::comma(val), "0"),
        subtitle = pretty_label(label),
        color = color
      )
    })
  }
  
  render_monthly_months <- function(label, color) {
    renderValueBox({
      req(input$search_bar, input$date_picker_start, input$date_picker_end)
      val <- monthly_months_function(input$search_bar, c(input$date_picker_start, input$date_picker_end)) %>%
        filter(use_supply_aug_red == label) %>%
        pull(num_months)
      
      valueBox(
        value = ifelse(length(val) > 0, val, "0"),
        subtitle = paste(pretty_label(label), "(Months)"),
        color = color
      )
    })
  }
  
  output$monthly_shortage_value <- render_monthly_value("shortage_surplus_acre_feet", "blue")
  output$monthly_shortage_months <- render_monthly_months("shortage_surplus_acre_feet", "teal")
  output$monthly_aug_value       <- render_monthly_value("benefit_supply_augmentation_acre_feet", "blue")
  output$monthly_aug_months      <- render_monthly_months("benefit_supply_augmentation_acre_feet", "teal")
  output$monthly_red_value       <- render_monthly_value("benefit_demand_reduction_acre_feet", "blue")
  output$monthly_red_months      <- render_monthly_months("benefit_demand_reduction_acre_feet", "teal")
  
  # --- Actual Shortage Value Boxes ---

  output$average_shortage <- renderValueBox({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    df <- actual_filter_function_sum_stats(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    
    avg <- mean(df$state_standard_shortage_level, na.rm = TRUE)
    
    valueBox(
      value = round(avg, 2),
      subtitle = "Average Shortage Level",
      color = "yellow"
    )
  })
  
  lapply(0:6, function(i) {
    output[[paste0("shortage_level_", i)]] <- renderValueBox({
      req(input$search_bar, input$date_picker_start, input$date_picker_end)
      df <- actual_filter_function_sum_stats(input$search_bar, c(input$date_picker_start, input$date_picker_end))
      
      count <- sum(df$state_standard_shortage_level == i, na.rm = TRUE)
      
      valueBox(
        value = count,
        subtitle = paste("Months at Shortage Level", i),
        color = "orange"
      )
    })
  })
  
  # --- Historical Production Value Boxes ---
  
  output$total_produced_box <- renderValueBox({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    df <- hist_filt_function_sum_stats(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    
    produced_total <- df %>%
      filter(water_produced_or_delivered == "water produced") %>%
      summarize(total = sum(quantity_acre_feet, na.rm = TRUE)) %>%
      pull(total)
    
    valueBox(
      value = scales::comma(produced_total),
      subtitle = pretty_label("total_produced"),
      color = "light-blue"
    )
  })
  
  
  output$total_delivered_box <- renderValueBox({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    df <- hist_filt_function_sum_stats(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    
    delivered_total <- df %>%
      filter(water_produced_or_delivered == "water delivered") %>%
      summarize(total = sum(quantity_acre_feet, na.rm = TRUE)) %>%
      pull(total)
    
    valueBox(
      value = scales::comma(delivered_total),
      subtitle = pretty_label("total_delivered"),
      color = "light-blue"
    )
  })
  
  output$hist_value_boxes <- renderUI({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    df <- hist_filt_function_sum_stats(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    
    selected_types <- combined_water_types()
    
    if (is.null(selected_types) || length(selected_types) == 0) return(NULL)
    
    # Standardize matching by lowercasing both
    values_df <- df %>%
      filter(str_to_lower(water_type) %in% str_to_lower(selected_types)) %>%
      group_by(water_produced_or_delivered, water_type) %>%
      summarize(total_value = sum(quantity_acre_feet, na.rm = TRUE), .groups = "drop")
    
    produced <- values_df %>% filter(water_produced_or_delivered == "water produced")
    delivered <- values_df %>% filter(water_produced_or_delivered == "water delivered")
    
    tagList(
      if (nrow(produced) > 0) {
        tagList(
          h4("Water Produced", style = "text-align: center; margin-top: 10px;"),
          fluidRow(
            lapply(seq_len(nrow(produced)), function(i) {
              column(6, div(class = "value-box-custom", valueBox(
                value = scales::comma(produced$total_value[i]),
                subtitle = pretty_label(produced$water_type[i], water_type_context = TRUE),
                icon = NULL
              )))
            })
          )
        )
      },
      if (nrow(delivered) > 0) {
        tagList(
          h4("Water Delivered", style = "text-align: center; margin-top: 20px;"),
          fluidRow(
            lapply(seq_len(nrow(delivered)), function(i) {
              column(6, div(class = "value-box-custom", valueBox(
                value = scales::comma(delivered$total_value[i]),
                subtitle = pretty_label(delivered$water_type[i], water_type_context = TRUE),
                icon = NULL
              )))
            })
          )
        )
      }
    )
  })
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                            NA Values Reactive UI                         ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # START NA Values Reactive UI
  output$na_values <- renderUI({
    req(input$dataset_selector)
    
    card(
      full_screen = TRUE,
      style = "overflow-y: auto; max-height: 290px; padding: 10px;",
      div(
        style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
        tags$span(actionButton("info_NA", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
      ),
      bsPopover(
        id = "info_NA",
        title = "Information",
        content = "This panel displays the percentage of missing values across each category. Note:Due to data limitations, it is not possible to distinguish between values that are truly missing and those that are not applicable.",
        placement = "left",
        trigger = "hover",
        options = list(container = "body")
      ),
      div(  # <-- Here wrap the content you want to scroll
        style = "overflow-y: auto; max-height: 240px; padding-right: 10px;",  # Important: slightly smaller inside the card
        tagList(
          switch(input$dataset_selector,
                 
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ##  ~ Monthly Water Outlook  ----
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 "monthly_water_outlook" = tagList(
                   h3("Missing Information", style = "text-align: center;"),
                   fluidRow(
                     column(6, valueBoxOutput("reduction_na")),
                     column(6, valueBoxOutput("surplus_na"))
                   )
                 ),
                 
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ##  ~ Five Year Outlook  ----
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 "five_year_outlook" = tagList(
                   h3("Missing Information", style = "text-align: center;"),
                   fluidRow(
                     column(6, valueBoxOutput("fiveyr_na_percent_water_use")),
                     column(6, valueBoxOutput("fiveyr_na_percent_water_supplies"))
                   ),
                   fluidRow(
                     column(6, valueBoxOutput("fiveyr_na_percent_benefit_supply")),
                     column(6, valueBoxOutput("fiveyr_na_percent_benefit_demand"))
                   )
                 ),
                 
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~
                 ##  ~ Actual Shortage  ----
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~
                 "actual_shortage" = tagList(
                   h3("Missing Information", style = "text-align: center;"),
                   valueBoxOutput("shortage_na_percent")  
                 ),
                 
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ##  ~ Historical Production  ----
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 "historical_production" = tagList(
                   h3("Missing Information", style = "text-align: center;"),
                   uiOutput("historical_na_boxes")
                 )
          )
        )
      )
    )
  })
  
  # -----------------------------------------
  # --- Actual Shortage Value Boxes ---
  # -----------------------------------------
  
  # Render the actual shortage value box
  output$shortage_na_percent <- renderValueBox({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    # Get the data for the org and date range
    df <- actual_filter(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    
    # Calculate the percent missing
    percent_missing <- round(actual_na(df)[3], 2)
    
    # Create and render the valueBox
    valueBox(
      value = paste0(percent_missing, "%"),
      subtitle = "Missing Data:",
      color = "black"
    ) 
  })
  
  
  # -----------------------------------------
  # --- 5 Year Value Boxes ---
  # -----------------------------------------
  render_fiveyr_na_box <- function(field, label) {
    renderValueBox({
      req(input$search_bar, input$date_picker_start, input$date_picker_end)
      
      df2 <- fiveyr_filter(input$search_bar, c(input$date_picker_start, input$date_picker_end))
      na_summary <- fiveyr_na(df2)
      
      percent_missing <- round(na_summary[[field]], 2)
      
      valueBox(
        value = ifelse(is.na(percent_missing), "No data", paste0(percent_missing, "%")),
        subtitle = label,
        color = "black"
      )
    })
  }
  
  # 5yr dataset column display
  output$fiveyr_na_percent_water_use      <- render_fiveyr_na_box("water_use_na", "Water Use Missing Data")
  output$fiveyr_na_percent_water_supplies <- render_fiveyr_na_box("water_supplies_na", "Water Supplies Missing Data")
  output$fiveyr_na_percent_benefit_supply <- render_fiveyr_na_box("benefit_supply_na", "Supply Augmentation Missing Data")
  output$fiveyr_na_percent_benefit_demand <- render_fiveyr_na_box("benefit_demand_na", "Demand Reduction Missing Data")
  
  # -----------------------------------------
  # --- Monthly Value Boxes ---
  # ----------------------------------------- 
  render_monthly_na_box <- function(field, label) {
    renderValueBox({
      req(input$search_bar, input$date_picker_start, input$date_picker_end)
      
      df3 <- monthlywater_filter(input$search_bar, c(input$date_picker_start, input$date_picker_end))
      na_summary <- monthly_na(df3)
      
      percent_missing <- round(na_summary[[field]], 2)
      
      valueBox(
        value = ifelse(is.na(percent_missing), "No data", paste0(percent_missing, "%")),
        subtitle = label,
        color = "black"
      )
    })
  }
  
  # Monthly dataset column display
  output$reduction_na      <- render_monthly_na_box("demand_red_na", "Demand Reduction Missing Data")
  output$surplus_na <- render_monthly_na_box("shortage_na", "Shortage Surplus Data")
  
  # -----------------------------------------
  # --- Historical Production Value Boxes ---
  # -----------------------------------------
  
  # Function to render Historical Production NA percentage boxes
  output$historical_na_boxes <- renderUI({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    water_types <- combined_water_types()
    
    if (is.null(water_types) || length(water_types) == 0) {
      return(h4("No water types selected.", style = "text-align:center;"))
    }
    
    df4 <- historical_filtering(input$search_bar, c(input$date_picker_start, input$date_picker_end), water_types)
    
    if (nrow(df4) == 0) {
      return(h4("No historical production data available for this org/date/type.", style = "text-align:center;"))
    }
    
    na_summary <- hist_na(df4)
    
    value_boxes <- lapply(water_types, function(type) {
      label <- paste("Missing Data:", type)
      
      na_row <- na_summary %>% filter(tolower(water_type) == tolower(type))
      
      if (nrow(na_row) == 0) {
        percent_missing <- 100 
      } else {
        percent_missing <- round(na_row$na_percentage, 2)
      }
      
      column(6, valueBox(
        value = paste0(percent_missing, "%"),
        subtitle = label,
        color = if (is.na(percent_missing)) {
          "gray"
        } else if (percent_missing == 0) {
          "green"
        } else if (percent_missing <= 10) {
          "yellow"
        } else {
          "red"
        }
      ))
    })
    
    fluidRow(value_boxes)
  })
  
  
} # End of server 