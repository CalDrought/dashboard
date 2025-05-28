server <- function(input, output, session) {
  
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
    req(input$dataset_selector)
    
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
        value = dr$maxDate,
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
    req(input$dataset_selector)
    
    start <- as.Date(input$date_picker_start)
    
    # Get correct date bounds depending on dataset type
    if (input$dataset_selector == "five_year_outlook") {
      bounds <- year_range()
    } else {
      bounds <- date_range()
    }
    
    # Get current end or fallback
    current_end <- input$date_picker_end
    end <- if (!is.null(current_end)) as.Date(current_end) else bounds$maxDate
    
    # Check if end is out of bounds or before start
    if (end < start || end > bounds$maxDate) {
      end <- bounds$maxDate
    }
    
    updateAirDateInput(
      session,
      "date_picker_end",
      value = end,
      options = list(
        minDate = start,
        maxDate = bounds$maxDate
      )
    )
  })
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                          Reactive Plot UI Output                         ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # -------- START Column 1: Inside Box 1: Icon Buttons -------------------- #
  
  # Info button for each dataset
  output$plot_info <- renderUI({
    req(input$dataset_selector)
    
    switch(input$dataset_selector,
           
           "actual_shortage" = tagList(
             div(style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
                 tags$span(actionButton("info_graph", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             bsPopover(
               id = "info_graph",
               title = "Information",
               content = "Displays observed shortage level values on a scale of 0 to 6, with 1 representing 0-10% water shortage and 6 representing 50% + shortage. For more information, visit the about page",
               placement = "right",
               trigger = "hover",
               options = list(container = "body")
             )
           ),
           
           "monthly_water_outlook" = tagList(
             
             div(style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
                 tags$span(actionButton("info_graph", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             bsPopover(
               id = "info_graph",
               title = "Information",
               content = "Displays forecasted shortage level values on a scale of 0 to 6, with 1 representing 0-10% shortage and 6, representing 50% + shortage. For more information, visit the about page",
               placement = "right",
               trigger = "hover",
               options = list(container = "body")
             )
           ),
           
           "five_year_outlook" = tagList(
             div(style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
                 tags$span(actionButton("info_graph", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             bsPopover(
               id = "info_graph",
               title = "Information",
               content = "Displays forecasted water supply, use, augmentation, and reduction over an annual scale. For more information, visit the about page",
               placement = "right",
               trigger = "hover",
               options = list(container = "body")
             )
           ),
           
           "historical_production" = tagList(
             div(style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
                 tags$span(actionButton("info_graph", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
             ),
             bsPopover(
               id = "info_graph",
               title = "Information",
               content = "Displays observed delivered and produced water on a monthly scale, For more information visit the about page",
               placement = "right",
               trigger = "hover",
               options = list(container = "body")
             )
           )
             
             )
  }) # END Info Switch Button 
  
  
  
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
  output$plot_output <- plotly::renderPlotly({
    
    # Need dataset, org_id, start/end dates.
    req(input$dataset_selector, input$search_bar, input$date_picker_start, input$date_picker_end)
    
    # Find dataset based on name.
    selected_name <- input$dataset_selector
    selected_df <- water_data[[selected_name]]
    
    # Extra Requirement for Historical_production
    if (selected_name == "historical_production") {
      req(combined_water_types())
    }
    
    # Filtering the dates to the proper input format. 
    # Format the date selection from the date picker to ensure consistency across all datasets
    start_ym <- format(as.Date(input$date_picker_start), "%Y-%m")
    end_ym   <- format(as.Date(input$date_picker_end),   "%Y-%m")
    
    # Filtering for years to the proper input format
    # This is used for the "Five_year_outlook" as it is displayed in years
    start_y <- format(as.Date(input$date_picker_start), "%Y")
    end_y   <- format(as.Date(input$date_picker_end),   "%Y")
    
    print(c(input$date_picker_start, input$date_picker_end))
    print(c(start_ym, end_ym))
    
    water_types <- combined_water_types()
    
    # ---------------------------
    # Check if filtered data exists
    # ---------------------------
    filtered_data <- switch(selected_name,
                            
                            # Filter function used in Monthly Water Outlook
                            "monthly_water_outlook" = monthly_filter(input$search_bar, c(start_ym, end_ym)),
                            
                            # Filter function used in Five Year Outlook
                            "five_year_outlook" = five_filter_function(input$search_bar, c(start_y, end_y)),
                            
                            # Filter for Historical Production with water type filtering
                            "historical_production" = hist_filt_function(input$search_bar, c(start_ym, end_ym)) %>%
                              dplyr::filter(water_type %in% water_types),
                            
                            # Filter function used in Actual Shortage
                            "actual_shortage" = actual_filter_function(input$search_bar, c(start_ym, end_ym))
    )
    
    if (nrow(filtered_data) == 0) {
      # Return a dummy plot with a message centered if no data is available
      return(plotly::ggplotly(
        ggplot() +
          annotate("text", x = 0, y = 0, label = "No data available for this water district and dataset.",
                   size = 6, color = "red", fontface = "bold", hjust = 0.5) +
          theme_void()
      ))
    }
    
    # ---------------------------
    # Switch statement to change function based on dataset
    # ---------------------------
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
    plotly::ggplotly(plot, tooltip = "text") %>% 
      config(displayModeBar = FALSE)
  }) # -------END Column 1: Inside Box 1: Row 3 (Plot Display) --------
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                         Summary Statistics Section                        ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # START of dynamic summary statistics UI block
  output$summary_stats <- renderUI({
    
    # Ensure a dataset has been selected before rendering anything
    req(input$dataset_selector)
    
    # START of card UI container
    card(
      full_screen = TRUE,
      style = "overflow: hidden; max-height: 290px; padding: 10px;",  # prevents scrollbars on the card container
      
      # START of top-right info button
      div(
        style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
        tags$span(
          actionButton(
            inputId = "info_summary",
            label = NULL,
            icon = icon("info-circle"),
            class = "btn btn-info btn-xs"
          )
        )
      ),
      # END of top-right info button
      
      # START of popover for the info icon
      bsPopover(
        id = "info_summary",
        title = "Information",
        content = "Displaying summary statistics for the selected dataset. These statistics have omitted NAs. Please see the missing information section for more details on missing information.",
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      ),
      # END of popover
      
      # START of scrollable summary content container
      div(
        style = "
        overflow-y: auto;       /* vertical scroll only when needed */
        overflow-x: hidden;     /* prevent horizontal scroll */
        max-height: 230px;      /* match space below info icon */
        padding-right: 5px;     /* buffer for vertical scrollbar */
      ",
        
        tagList(
          
          # START of section title
          div(
            style = "text-align: center; margin-bottom: 10px;",
            h3("Summary Statistics")
          ),
          # END of section title
          
          # START of dataset-dependent UI rendering
          switch(
            input$dataset_selector,
            
            # Render 5-year outlook stats
            "five_year_outlook" = summary_five_year_ui(),
            
            # Render monthly water outlook stats
            "monthly_water_outlook" = summary_monthly_ui(),
            
            # Render actual shortage levels summary
            "actual_shortage" = summary_actual_ui(),
            
            # Render historical production & delivery
            "historical_production" = summary_historical_ui()
          )
          # END of dataset-dependent UI
          
        )  # END of tagList
      )
      # END of scrollable summary content container
      
    )  # END of card
  })
  # END of dynamic summary statistics UI block
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##               Dataset-specific UI functions (DT tables only)             ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # START of 5-Year Outlook summary table UI
  summary_five_year_ui <- function() {
    fluidRow(
      column(12, dataTableOutput("fiveyr_table"))
    )
  }
  # END of 5-Year Outlook summary table UI
  
  # START of Monthly Water Outlook summary table UI
  summary_monthly_ui <- function() {
    fluidRow(
      column(12, dataTableOutput("monthly_table"))
    )
  }
  # END of Monthly Water Outlook summary table UI
  
  # START of Actual Shortage summary table UI
  summary_actual_ui <- function() {
    fluidRow(
      column(12, dataTableOutput("actual_table"))
    )
  }
  # END of Actual Shortage summary table UI
  
  # START of Historical Production summary table UI
  summary_historical_ui <- function() {
    fluidRow(
      column(12, dataTableOutput("historical_table"))
    )
  }
  # END of Historical Production summary table UI
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                          Server-side DT table outputs                     ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # START of 5-Year Outlook data table rendering
  output$fiveyr_table <- renderDataTable({
    
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    start_year <- year(as.Date(input$date_picker_start))
    end_year   <- year(as.Date(input$date_picker_end))
    
    df <- five_values_function_sum_stat(input$search_bar, c(start_year, end_year)) %>%
      filter(use_supply_aug_red %in% c(
        "water_use_acre_feet",
        "water_supplies_acre_feet",
        "benefit_supply_augmentation_acre_feet",
        "benefit_demand_reduction_acre_feet"
      )) %>%
      mutate(
        Metric = pretty_label(use_supply_aug_red),
        `Value (Acre-Feet)` = scales::comma(total_value)
      ) %>%
      select(Metric, `Value (Acre-Feet)`)
    
    datatable(df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    
  })
  # END of 5-Year Outlook data table rendering
  
  
  # START of Monthly Water Outlook data table rendering
  output$monthly_table <- renderDataTable({
    
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    values <- monthly_values_function_sum_stat(input$search_bar, c(input$date_picker_start, input$date_picker_end)) %>%
      filter(use_supply_aug_red %in% c(
        "shortage_surplus_acre_feet",
        "benefit_supply_augmentation_acre_feet",
        "benefit_demand_reduction_acre_feet"
      )) %>%
      mutate(
        Metric = pretty_label(use_supply_aug_red),
        `Total Acre-Feet` = scales::comma(total_acre_feet)
      ) %>%
      select(Metric, `Total Acre-Feet`)
    
    datatable(values, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    
  })
  # END of Monthly Water Outlook data table rendering
  
  
  # START of Actual Shortage data table rendering
  output$actual_table <- renderDataTable({
    
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    df <- actual_filter_function_sum_stats(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    
    result <- tibble(
      Metric = c("Average Shortage Level", paste0("Months at Level ", 0:6)),
      Value = c(
        round(mean(df$state_standard_shortage_level, na.rm = TRUE), 2),
        sapply(0:6, function(i) sum(df$state_standard_shortage_level == i, na.rm = TRUE))
      )
    )
    
    datatable(result, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    
  })
  # END of Actual Shortage data table rendering
  
  
  # START of Historical Production & Delivery data table rendering
  output$historical_table <- renderDataTable({
    
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    # Full dataset filtered by organization and date range
    df <- hist_filt_function_sum_stats(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    
    # Get selected water types from user input
    selected_types <- combined_water_types()
    
    # Return nothing if no water types selected
    if (is.null(selected_types) || length(selected_types) == 0) return(NULL)
    
    #----------------------------
    # Compute totals for ALL types (not just selected)
    #----------------------------
    
    produced_total <- df %>%
      filter(water_produced_or_delivered == "water produced") %>%
      summarize(total = sum(quantity_acre_feet, na.rm = TRUE)) %>%
      mutate(
        `Produced or Delivered` = "Total Produced",
        Type = "—",
        `Total Acre-Feet` = scales::comma(total)
      ) %>%
      select(`Produced or Delivered`, Type, `Total Acre-Feet`)
    
    delivered_total <- df %>%
      filter(water_produced_or_delivered == "water delivered") %>%
      summarize(total = sum(quantity_acre_feet, na.rm = TRUE)) %>%
      mutate(
        `Produced or Delivered` = "Total Delivered",
        Type = "—",
        `Total Acre-Feet` = scales::comma(total)
      ) %>%
      select(`Produced or Delivered`, Type, `Total Acre-Feet`)
    
    #----------------------------
    # Breakdown by selected water types
    #----------------------------
    
    filtered_df <- df %>%
      filter(str_to_lower(water_type) %in% str_to_lower(selected_types))
    
    breakdown_df <- filtered_df %>%
      group_by(water_produced_or_delivered, water_type) %>%
      summarize(total_value = sum(quantity_acre_feet, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        `Produced or Delivered` = str_to_title(water_produced_or_delivered),
        Type = pretty_label(water_type, water_type_context = TRUE),
        `Total Acre-Feet` = scales::comma(total_value)
      ) %>%
      select(`Produced or Delivered`, Type, `Total Acre-Feet`)
    
    
    #----------------------------
    # Combine totals and breakdown into one final table
    #----------------------------
    
    final_df <- bind_rows(
      produced_total,
      delivered_total,
      breakdown_df
    )
    
    datatable(final_df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    
  })
  # END of Historical Production & Delivery data table rendering
  
  ## END of summary statistics section
  
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
      div(
        style = "overflow-y: auto; max-height: 240px; padding-right: 10px;",
        tagList(
          ## --- NEW TITLE ADDED HERE ---
          div(style = "text-align: center; margin-bottom: 10px;", h3("Missing Information")),
          
          switch(input$dataset_selector,
                 
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ##  ~ Monthly Water Outlook  ----
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 "monthly_water_outlook" = dataTableOutput("monthly_na_table"),
                 
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ##  ~ Five Year Outlook  ----
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 "five_year_outlook" = dataTableOutput("fiveyr_na_table"),
                 
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~
                 ##  ~ Actual Shortage  ----
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~
                 "actual_shortage" = dataTableOutput("actual_na_table"),
                 
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ##  ~ Historical Production  ----
                 ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 "historical_production" = dataTableOutput("historical_na_table")
          )
        )
      )
    )
  })
  
  # -----------------------------------------
  # --- Actual Shortage NA Table ---
  # -----------------------------------------
  output$actual_na_table <- renderDataTable({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    df <- actual_filter(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    percent_missing <- round(actual_na(df)[3], 2)
    
    na_df <- tibble(
      Category = "Shortage Level",
      `Missing (%)` = paste0(percent_missing, "%")
    )
    
    datatable(na_df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  # -----------------------------------------
  # --- 5 Year Outlook NA Table ---
  # -----------------------------------------
  output$fiveyr_na_table <- renderDataTable({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    df2 <- fiveyr_filter(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    na_summary <- fiveyr_na(df2)
    
    na_df <- tibble(
      Category = c("Water Use", "Water Supplies", "Supply Augmentation", "Demand Reduction"),
      `Missing (%)` = c(
        na_summary$water_use_na,
        na_summary$water_supplies_na,
        na_summary$benefit_supply_na,
        na_summary$benefit_demand_na
      ) |> round(2) |> paste0("%")
    )
    
    datatable(na_df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  # -----------------------------------------
  # --- Monthly Water Outlook NA Table ---
  # -----------------------------------------
  output$monthly_na_table <- renderDataTable({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    df3 <- monthlywater_filter(input$search_bar, c(input$date_picker_start, input$date_picker_end))
    na_summary <- monthly_na(df3)
    
    na_df <- tibble(
      Category = c("Demand Reduction", "Shortage Surplus"),
      `Missing (%)` = c(
        na_summary$demand_red_na,
        na_summary$shortage_na
      ) |> round(2) |> paste0("%")
    )
    
    datatable(na_df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  # -----------------------------------------
  # --- Historical Production NA Table ---
  # -----------------------------------------
  output$historical_na_table <- renderDataTable({
    req(input$search_bar, input$date_picker_start, input$date_picker_end)
    
    water_types <- combined_water_types()
    if (is.null(water_types) || length(water_types) == 0) {
      return(datatable(tibble(Note = "No water types selected."), options = list(dom = 't'), rownames = FALSE))
    }
    
    df4 <- historical_filtering(input$search_bar, c(input$date_picker_start, input$date_picker_end), water_types)
    
    if (nrow(df4) == 0) {
      return(datatable(tibble(Note = "No historical production data available for this org/date/type."), options = list(dom = 't'), rownames = FALSE))
    }
    
    na_summary <- hist_na(df4)
    
    na_df <- na_summary %>%
      mutate(
        Type = pretty_label(water_type, water_type_context = TRUE),
        `Missing (%)` = paste0(round(na_percentage, 2), "%")
      ) %>%
      select(Type, `Missing (%)`)
    
    datatable(na_df, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                       historical toggle for tutorial                     ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  observeEvent(input$show_more, {
    toggle("historical_section")
  })
} # End of server 