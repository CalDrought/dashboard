library(shiny)
library(shinydashboard)
library(tools)
library(shinyjs)
library(spnaf)
library(shinyBS)
library(paletteer)
source("data_cleaning.R") # Load in data once.

# Load the dataset names to be used for UI dropdown
# water_data <- load_water_data() 
dataset_labels <- names(water_data) |> 
  setNames(toTitleCase(gsub("_", " ", names(water_data))))

ui <- navbarPage(
  "California Water Data Consortium",
  tabPanel(
    title = "Home",
    fluidPage(
      
      # BEGIN BOX FOR ENTIRE PAGE
      box(
        width = NULL,
        
        # title
        h2(tags$strong("Welcome to the Urban Water Data Dashboard"), style = "font-size: 35px"),
        HTML('<img src="images/dwr_homepage_pic.png" width="900">')
        
  ))),
  tabPanel(
    title = "Tutorial",
    h2(tags$strong("Learn How to Use This Dashboard!"), style = "font-size: 35px"),
    fluidRow( 
      column(
        width = 12,
        includeMarkdown("text/tutorial_intro.Rmd"),
    HTML('<img src="images/dashboard_overview.png" width="900">'))),
    fluidRow(
      column(
        width = 12,
      includeMarkdown("text/tutorial_overview.Rmd")
    )),
    fluidRow(
      column(
        width = 12,
      HTML('<img src="images/select_dataset2.png" width="900">'),
      includeMarkdown("text/tutorial_dataset_select.Rmd")
    ))
  ),
  
  tabPanel(
    title = "Dashboard",
  
  useShinyjs(),
  
  tags$style("
    #search_bar + .selectize-control .selectize-input {
      border-radius: 15px !important;
      background: white;
      padding-right: 12px;      
    }
    #search_bar + .selectize-control.single .selectize-input::after {
      content: none !important;
    }
    #toggle_facility_type.btn-toggle-off,
    #toggle_shortage_year.btn-toggle-off {
      background-color: #f0f0f0 !important;
      color: #000 !important;
      font-weight: bold;
      font-size: 16px;
      width: 100%;
    }
    #toggle_facility_type.btn-toggle-on,
    #toggle_shortage_year.btn-toggle-on {
      background-color: #ADD8E6 !important;  
      color: #fff !important;
      font-weight: bold;
      font-size: 16px;
      width: 100%;
    }
  "),
  
  
  
  fluidRow(
    column(12, style = "display: flex; align-items: stretch;",
           column(7, style = "border: 1px double black; padding-top: 15px;",
                  fluidRow(
                    column(4, style = "display: flex;",
                           selectInput("dataset_selector", "Select Dataset", choices = NULL, width = "100%")
                    ),
                    column(7, uiOutput("plot_controls")),
                    column(1, uiOutput("plot_info")
                           
                    ),
                    conditionalPanel(
                      condition = "input.dataset_selector == 'historical_production'",
                      column(12, style = "padding-top: 15px;",
                             fluidRow(
                               column(5,
                                      selectInput("delivered_type", "Select Delivered Type",
                                                  c("Agriculture", "Single-Family Residential", "Commercial/Institutional",
                                                    "Industrial", "Landscape Irrigation", "Multi-Family Residential",
                                                    "Other", "Other Pws"),
                                                  multiple = TRUE, width = "100%")
                               ),
                               column(5,
                                      selectInput("produced_type", "Select Produced Type",
                                                  c("Recycled", "Surface Water", "Groundwater Wells",
                                                    "Non-Potable (Total Excluded Recycled)", "Purchased Or Received From Another Pws",
                                                    "Sold To Another Pws", "Non-Potable Water Sold To Another Pws"),
                                                  multiple = TRUE, width = "100%")
                               ),
                               column(2,
                                      checkboxGroupInput("include_total", "Total Produced and Delivered", choices = "Total")
                               )
                             )
                      )
                    )
                  ),
                  fluidRow(
                    column(12,
                           plotOutput("plot_output", height = "550px")
                    )
                  )
           ),
           
           column(5, style = "display: flex; padding-right: 0px;",
                  column(12, style = "padding: 0; border: 1px double black;",
                         div(style = "position: relative; width: 100%; height: 100%",
                             tmapOutput("tmap_by_dataset", height = "100%"),
                             div(
                               style = "position: absolute; top: 10px; left: 55px; width: calc(100% - 100px)",
                               selectizeInput("search_bar", label = NULL, choices = NULL, width = "100%",
                                              selected = character(0),
                                              options = list(placeholder = "Search Districts by Name or Org ID")),
                               tags$i(class = "fa fa-search",
                                      style = "position: absolute; right: 12px; top: 20%; pointer-events: none; color: #888; z-index: 2;")
                             ),
                             div(style = "position: absolute; top: 10px; right: 10px;",
                                 tags$span(actionButton("info_map", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
                             ),
                             bsPopover(
                               id = "info_map",
                               title = "Information",
                               content = "Choose a water district/Org ID by either clicking an area on the map or typing in the search bar.",
                               placement = "left",
                               trigger = "hover",
                               options = list(container = "body")
                             )
                         )
                  )
           )
    )
  ),
  



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
  
  
  
  
),

tabPanel(
  title = "About",
  fluidPage(
    
    # BEGIN BOX FOR ENTIRE PAGE
    box(
      width = NULL,
      
      # title
      h2(tags$strong("Learn More About the Urban Water Data Dashboard"), style = "font-size: 35px"),
      
      column(
        width = 7,
        
        # text description
        includeMarkdown("text/about_intro.Rmd")),
      
      column(
        width = 5,
        style = "border: 1px double black; background-color:#C2E0FF;",
        
        
        # text description
        includeMarkdown("text/about_box.Rmd")
        
      ),
      
      fluidRow(
        column(
          width = 12,
          
          # text description
          includeMarkdown("text/about_text.Rmd") 
          
        )),
      
      fluidRow(
        column(
          width = 6,
          style = "border: 1px double black; background-color:#C2E0FF;",
          
          # text description
          includeMarkdown("text/about_limitations.Rmd")
        ),
        column(
          width = 6,
          
          # text description
          includeMarkdown("text/about_CWDC.Rmd")
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          
          includeMarkdown("text/about_end.Rmd")
          
          
        )
      )
      
      
    )
    
    
  )
  
) # end about page

)
