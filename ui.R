#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(tools)
library(shinyjs)
library(spnaf)
library(shinyBS)
library(paletteer)
source("data_cleaning.R") # Load in data once.

ui <- fluidPage(
  
  # Initialize shinyjs
  useShinyjs(),
  
  
  # ------------- CSS for custom widgets -------------
  
  # Define CSS for two different button states.
  tags$style("
    /* Rounded search box */
    #search_bar + .selectize-control .selectize-input {
      border-radius: 15px !important;
      background: white;
      padding-right: 12px;      
    }
      
    /* Hide the default dropdown icon */
    #search_bar + .selectize-control.single .selectize-input::after {
      content: none !important;
    }
  
    /* Defualt Facility Type action button style */
    #toggle_facility_type.btn-toggle-off {
      background-color: #f0f0f0 !important;
      color: #000 !important;
      font-weight: bold;
      font-size: 16px;
      width: 100%;
    }
    
    /* Toggled Facility Type action button style */
    #toggle_facility_type.btn-toggle-on {
      background-color: #ADD8E6 !important;  
      color: #fff !important;
      font-weight: bold;
      font-size: 16px;
      width: 100%
    }
    
    /* Default Shortage Year action button style */
    #toggle_shortage_year.btn-toggle-off {
      background-color: #f0f0f0 !important;
      color: #000 !important;
      font-weight: bold;
      font-size: 16px;
      width: 100%
    }
    
    /* Toggled Shortage Year action button style */
    #toggle_shortage_year.btn-toggle-on {
      background-color: #ADD8E6 !important;  
      color: #fff !important;
      font-weight: bold;
      font-size: 16px;
      width: 100%
    }
  "),
  
  # ------------- Navigation Bar -------------
  
  navbarPage(
    "California Water Data Consortium",
    tabPanel("Home"),
    tabPanel("Dashboard")
  ),
  
  # ------------- Main Screen -------------
  
  # This is the top row of the dashboard.
  fluidRow(
    
    # Nesting main `column()` body into parent column to add padding on all sides +15px. 
    # Also forcing children to expand to fill parents objects size.
    column(12, 
           style = "display: flex; align-items: stretch;",
           
           #### ---- Top-Left: Water Shortage Level Graph --- #### 
           column(7,
                  style = "border: 1px double black; padding-top: 15px;",
                  
                  # START First row containing dataset picker and info-button
                  fluidRow(
                    
                    # Dataset selector
                    column(4,
                           style = "display: flex;",
                           selectInput("dataset_selector", "Select Dataset", choices = NULL, width = "100%")),
                    
                    # Dynamic date picker
                    column(7,
                           uiOutput("plot_controls")
                           ),
                    
                    # Info Button
                    column(1,
                           
                           # add info-button using bsPopover
                           div(
                             style = "margin-bottom: 8px; display: flex; justify-content: flex-end;",
                             tags$span(actionButton("info_graph", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
                           ),
                           
                           # add popover content
                           bsPopover(
                             id = "info_graph",
                             title = "Information",
                             content = "To generate the graph, select a water district/Org ID and specify the desired date range.",
                             placement = "right",
                             trigger = "hover",
                             options = list(container = "body")
                           )
                    ),
                    
                    
                    # Conditional Row for Historical_production data b/c there are more inputs
                    conditionalPanel(
                      condition = "input.dataset_selector == 'historical_production'",
                      
                      column(12,
                             
                             style = "padding-top: 15px;",
                             
                             # Create another row below dataset and date inputs
                             fluidRow(
                               
                               # Delivered type taking half the space
                               column(5,
                                      
                                      # Selecting water type
                                      selectInput("delivered_type", "Select Delivered Type", c("Agriculture",
                                                                                             "Single-Family Residential",
                                                                                             "Commercial/Institutional",
                                                                                             "Industrial",
                                                                                             "Landscape Irrigation",
                                                                                             "Multi-Family Residential",
                                                                                             "Other",
                                                                                             "Other Pws"),
                                                  
                                                  multiple = TRUE, # Able to select multiple "Types"
                                                  width = "100%")),  # Selection bar covers all of the fitted area
                               
                               # Produced type taking half the space
                               column(5,
                                      
                                      # Selecting Production Water Type
                                      selectInput("produced_type", "Select Produced Type",
                                                  c("Recycled", "Surface Water", "Groundwater Wells", 
                                                    "Non-Potable (Total Excluded Recycled)",  "Purchased Or Received From Another Pws",
                                                    "Sold To Another Pws","Non-Potable Water Sold To Another Pws"
                                                  ),
                                                  multiple = TRUE,
                                                  width = "100%")),
                               
                               column(2,
                                      
                                      # Selecting Total Production and Delivery
                                      checkboxGroupInput(inputId = "include_total",
                                                         label = "Total Produced and Delivered",
                                                         choices = "Total")
                                      )
                             )
                             
                             
                      )
                    )
                    
                  ), # END First row
                  
                  
                  
                  # START second row for the date range + plot
                  fluidRow(
                    column(12,
                           # The existing plot output
                           plotOutput("plot_output", height = "550px")
                    )
                  ) # END second row
           ),
           
           #### ---- Top-Center: Interactive Tmap ---- #### 
           column(5,
                  style = "display: flex; padding-right: 0px;",
                  
                  # Columns have an default padding of 15px therefore 12/12 
                  # won't take up the full space of the parent column.
                  column(12,
                         style = "padding: 0; border: 1px double black;", # Remove padding and marging so plot is snug.
                         
                         div(
                           style = "position: relative; width: 100%; height: 100%",
                           
                           tmapOutput("shortage_map", height = "100%"), # height 100% to make our tmap fill the vertical space in the box.
                           # overlayed textbox in the top‑left corner
                           div(
                             style = "position: absolute; top: 10px; left: 55px; width: calc(100% - 100px)",
                             selectizeInput(
                               inputId = "search_bar", 
                               label = NULL,
                               choices = NULL,
                               width = "100%",
                               selected  = character(0),
                               options   = list(
                                 placeholder = "Search Districts by Org ID")
                             ),
                             tags$i(
                               class = "fa fa-search",
                               style = "position: absolute; right: 12px; top: 20%; pointer-events: none; color: #888; z-index: 2;"
                             )
                           ),
                           
                           #div(selectInput("org_id", "Select Org ID", choices = NULL)),
                           
                           # add info-button using bsPopover
                           div(
                             style = "position: absolute; top: 10px; right: 10px;",
                             tags$span(actionButton("info_map", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
                           ),
                           
                           # add popover content
                           bsPopover(
                             id = "info_map",
                             title = "Information",
                             content = "Choose a water district/Org ID by either clicking an area on the map or typing in the search bar.",
                             placement = "right",
                             trigger = "hover",
                             options = list(container = "body")
                           ),
                         ),
                  ),
           ),
           
           #### ---- Top-Right: This column function represents the left-hand side with our filter options for the tma ---- ####
           # column(2,
           #        style = "border: 1px double black;",
           #        
           #        div(style = "padding: 10px;",
           #            actionButton("toggle_shortage_year", "Shortage Year", class = "btn-toggle-off"),
           #            div(style = "padding: 15px",
           #                conditionalPanel(
           #                  condition = "input.toggle_shortage_year %% 2 == 1",
           #                  checkboxGroupInput("shortage_year", "Select Shortage Year:", 
           #                                     choices = unique(format(as.Date(water_data$actual_shortage$start_date, format = "%Y-%m-%d"), "%Y"))))
           #            )
           #        ),
           # ),
    )
  ),
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                  Bottom Section: Summary Stats & NA Values               ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  # Row for the second half of the page
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                  Bottom Section: Summary Stats & NA Values               ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fluidRow(
    column(12, 
           style = "display: flex; align-items: stretch; margin-top: 15px;",
           
           # Bottom-left: Summary Stats
           column(7,
                  style = "border: 1px double black; padding: 10px; height: 300px;",
                  uiOutput("summary_stats")
           ),
           
           # Bottom-right: Missing Info — mimics top row layout
           column(5,
                  style = "display: flex; padding-right: 0px;",
                  column(12,
                         style = "padding: 0; border: 1px double black; height: 300px;",
                         uiOutput("na_values")
                  )
           )
    )
  )
  
  
  
  
)