#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Define UI with navbarPage                         ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- navbarPage(
  
  # START of main title/header
  "California Water Data Consortium",
  
  # START of custom CSS styling
  header = tags$head(includeCSS("www/theme.css")),
  # END of custom CSS styling
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                               Home Tab                                    ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  tabPanel(
    title = "Home",
    div(
      style = "
      height: 100vh;
      width: 100vw;
      background-image: url('images/2022_11_18_FL_0440_Folsom_Lake.jpg');
      background-size: cover;
      background-position: center;
      background-attachment: fixed;
      display: flex;
      flex-direction: column;
      align-items: center;
      padding-top: 10vh;
      text-align: center;
      color: #ffffff;
      padding-left: 20px;
      padding-right: 20px;
      position: relative;
    ",
      
      # Main transparent text box
      div(
        style = "
        background-color: rgba(0, 0, 0, 0.5);
        padding: 30px 40px;
        border-radius: 12px;
        max-width: 800px;
        box-shadow: 0px 4px 10px rgba(0,0,0,0.3);
      ",
        
        h1("Welcome to the Urban Water Dashboard", 
           style = "font-size: 3em; font-weight: bold; margin-bottom: 20px;"),
        
        p("This dashboard is designed to make it easy for anyone to interact with urban water management data without any spreadsheets or coding! Use this dashboard to view information on water shortage levels, water production and delivery, and forecasted water supply and use. ", 
          style = "font-size: 1.5em; margin: 0;")
      ),
      
      # Image credit at bottom
      div(
        "Image credit to the California Department of Water Resources",
        includeMarkdown("text/home_end.Rmd"),
        style = "
        position: absolute;
        bottom: 10px;
        font-size: 0.8em;
        color: #eeeeee;
        background-color: rgba(0, 0, 0, 0.4);
        padding: 4px 10px;
        border-radius: 6px;
      "
      )
    )
  )
  
  
    
    
  #   fluidPage(
  #     
  #     
  #     fluidRow(
  #       style = "background-color:#C2E0FF;",  # Blue banner background
  #       
  #       # Welcome text (left side)
  #       column(
  #         width = 5,
  #         h2(tags$strong("Welcome to the Urban Water Data Dashboard!"), style = "font-size: 50px"),
  #         h3("Visualize and navigate California's urban water data through graphs, maps, and summary statistics")
  #       ),
  #       
  #       # Hero image (right side)
  #       column(
  #         width = 7,
  #         HTML('<img src="images/dwr_homepage_pic.png" width="900">')
  #       )
  #     )
  #   )
  ,
  # # END of Home Tab
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                             Dashboard Tab                                 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  tabPanel(
    title = "Dashboard",
    
    useShinyjs(),  # Enable JavaScript functionality
    
    # START of custom DT table font style
    tags$style(HTML("
    .dataTables_wrapper {
      font-size: 19px !important;
    }
  ")),
    # END of custom DT table font style
    
    # START of custom UI styles
    tags$style("

      /* Rounded search bar styles */
      #search_bar + .selectize-control .selectize-input {
        border-radius: 15px !important;
        background: white;
        padding-right: 12px;      
      }

      #search_bar + .selectize-control.single .selectize-input::after {
        content: none !important;
      }

      .round-corner .selectize-input {
        border-radius: 15px !important;
      }

      .round-corner .form-control:not(.sw-air-picker) {
        border-radius: 15px !important;
      }

      .round-corner .sw-air-picker.form-control {
        border-radius: 15px 0 0 15px !important;
      }

      .round-corner .input-group .input-group-addon {
        border-radius: 0 15px 15px 0 !important;
      }

      /* Toggle button styles */
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
    # END of custom UI styles
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                      Top Section: Plot and Map                           ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    div(
      style = "background-color:#FBFEF9; width: 100%;",
      class = "round-corner",
      
      fluidRow(
        column(12, style = "display: flex; align-items: stretch;",
               
               # LEFT: Plot Panel
               column(
                 7,
                 style = "border: 1px double black; padding-top: 15px;",
                 
                 # Controls row
                 fluidRow(
                   column(4, selectInput("dataset_selector", "Select Dataset", choices = NULL, width = "100%")),
                   column(7, uiOutput("plot_controls")),
                   column(1, uiOutput("plot_info")),
                   
                   # Conditional panel for historical dataset
                   conditionalPanel(
                     condition = "input.dataset_selector == 'historical_production'",
                     column(12, style = "padding-top: 15px;",
                            fluidRow(
                              column(5, selectInput("delivered_type", "Select Delivered Type", c(
                                "Agriculture", "Single-Family Residential", "Commercial/Institutional",
                                "Industrial", "Landscape Irrigation", "Multi-Family Residential",
                                "Other", "Other Pws"
                              ), multiple = TRUE, width = "100%")),
                              column(5, selectInput("produced_type", "Select Produced Type", c(
                                "Recycled", "Surface Water", "Groundwater Wells",
                                "Non-Potable (Total Excluded Recycled)", "Purchased Or Received From Another Pws",
                                "Sold To Another Pws", "Non-Potable Water Sold To Another Pws"
                              ), multiple = TRUE, width = "100%")),
                              column(2, checkboxGroupInput("include_total", "Produced & Delivered", choices = "Total"))
                            )
                     )
                   )
                 ),
                 
                 # Main plot output
                 fluidRow(
                   column(12, plotly::plotlyOutput("plot_output", height = "550px"))
                 )
               ),
               
               # RIGHT: Map + Search Panel
               column(
                 5, style = "display: flex; padding-right: 0px;",
                 column(
                   12, style = "padding: 0; border: 1px double black;",
                   div(
                     style = "position: relative; width: 100%; height: 100%",
                     tmapOutput("tmap_by_dataset", height = "100%"),
                     
                     # Overlayed search input
                     div(
                       style = "position: absolute; top: 10px; left: 55px; width: calc(100% - 100px)",
                       selectizeInput("search_bar", label = NULL, choices = NULL, width = "100%",
                                      selected = character(0),
                                      options = list(placeholder = "Search Districts by Name or Org ID")
                       ),
                       tags$i(class = "fa fa-search",
                              style = "position: absolute; right: 12px; top: 20%; pointer-events: none; color: #888; z-index: 2;"
                       )
                     ),
                     
                     # Info icon overlay
                     div(style = "position: absolute; top: 10px; right: 10px;",
                         tags$span(actionButton("info_map", label = NULL, icon = icon("info-circle"), class = "btn btn-info btn-xs"))
                     ),
                     
                     bsPopover(
                       id = "info_map",
                       title = "Information",
                       content = "Use the search bar to find water districts by name or org ID. Plots reflect district boundaries with the respective governing org. Each one reflects summarized metrics by year showcased in the legend on the bottom right.",
                       placement = "left",
                       trigger = "hover",
                       options = list(container = "body")
                     )
                   )
                 )
               )
        )
      )
    ),
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #               Bottom Section: Summary Stats & NA Values                 ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # START of bottom panel container (holds both summary stats and NA tables)
    div(
      style = "background-color:#FBFEF9; width: 100%;",
      
      # Row layout: left = stats, right = NA values
      fluidRow(
        column(
          width = 12,
          style = "display: flex; align-items: stretch; margin-top: 15px;",
          
          # LEFT: Summary Stats UI
          column(
            width = 7,
            style = "
              border: 1px double black;
              padding: 10px;
              height: 300px;
            ",
            uiOutput("summary_stats")
          ),
          
          # RIGHT: NA Value Boxes
          column(
            width = 5,
            style = "display: flex; padding-right: 0px;",
            column(
              width = 12,
              style = "
                padding: 0;
                border: 1px double black;
                height: 300px;
              ",
              uiOutput("na_values")
            )
          )
        )
      )
    )
    # END of bottom panel container
  ),
  # END of Dashboard Tab
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                               Tutorial Tab                                ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  tabPanel(
    title = "Tutorial",
    style = "background-color:#FBFEF9;",
    
    fluidPage(
      useShinyjs(),
      h2(tags$strong("Learn How to Use This Dashboard!"), style = "font-size: 35px"),
      
      fluidRow(column(12, includeMarkdown("text/tutorial_intro.Rmd"),
                      HTML('<img src="images/dashboard_overview.png" width="900">'))),
      fluidRow(column(12, includeMarkdown("text/tutorial_overview.Rmd"))),
      fluidRow(column(12, HTML('<img src="images/select_dataset2.png" width="900">'),
                      includeMarkdown("text/tutorial_dataset_select.Rmd"))),
      fluidRow(column(12, HTML('<img src="images/select_dates2.png" width="900">'),
                      includeMarkdown("text/tutorial_date_select.Rmd"))),
      fluidRow(column(12, HTML('<img src="images/select_district.png" width="900">'),
                      includeMarkdown("text/tutorial_district_select.Rmd"))),
      fluidRow(column(12, HTML('<img src="images/select_district_map.png" width="600">'),
                      includeMarkdown("text/tutorial_district_select_map.Rmd"))),
      fluidRow(column(12, HTML('<img src="images/widget_one.png" width="900">'),
                      includeMarkdown("text/tutorial_widget_one.Rmd"))),
      
      # START of optional toggle section
      fluidRow(column(12, actionButton("show_more", "Show what additional historical production/delivery selectors mean"))),
      
      hidden(
        div(id = "historical_section",
            fluidRow(column(12, HTML('<img src="images/widget_one_historical_selector.png" width="500">'),
                            includeMarkdown("text/tutorial_widget_one_hist.Rmd"))),
            fluidRow(column(12, HTML('<img src="images/widget_one_hist_del.png" width="500">'),
                            includeMarkdown("text/tutorial_widget_one_hist_del.Rmd"))),
            fluidRow(column(12, HTML('<img src="images/widget_one_hist_prod.png" width="500">'),
                            includeMarkdown("text/tutorial_widget_one_hist_prod.Rmd"))),
            fluidRow(column(12, HTML('<img src="images/widget_one_hist_total.png" width="500">'),
                            includeMarkdown("text/tutorial_widget_one_hist_total.Rmd")))
        )
      ),
      fluidRow(column(12, HTML('<img src="images/widget_two.png" width="900">'),
                      includeMarkdown("text/tutorial_widget_two.Rmd")))
    ),
    
    fluidRow(column(12, HTML('<img src="images/widget_three.png" width="900">'),
                    includeMarkdown("text/tutorial_widget_three.Rmd"))),
    fluidRow(column(12, HTML('<img src="images/widget_four.png" width="900">'),
                    includeMarkdown("text/tutorial_widget_four.Rmd"))),
    fluidRow(column(12, includeMarkdown("text/tutorial_end.Rmd")))
  ),
  # END of Tutorial Tab
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                               About Tab                                   ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  tabPanel(
    title = "About",
    style = "background-color:#FBFEF9;",
    
    fluidPage(
      box(
        width = NULL,
        h2(tags$strong("Learn More About the Urban Water Data Dashboard"), style = "font-size: 35px"),
        
        column(7, includeMarkdown("text/about_intro.Rmd")),
        
        column(5, style = "border: 1px double black; background-color:#C2E0FF;",
               includeMarkdown("text/about_box.Rmd")),
        
        fluidRow(column(12, includeMarkdown("text/about_text.Rmd"))),
        
        fluidRow(
          column(6, style = "border: 1px double black; background-color:#C2E0FF;",
                 includeMarkdown("text/about_limitations.Rmd")),
          column(6, includeMarkdown("text/about_CWDC.Rmd"))
        ),
        
        fluidRow(column(12, includeMarkdown("text/about_end.Rmd")))
      )
    )
  )
  # END of About Tab
)
# END of UI
