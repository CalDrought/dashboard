# Interactive Dashboard for Urban Water Shortage Data
#### This repository was created for the Data for Drought Resilience capstone project 

[Link to Shiny App](https://shinyapps.bren.ucsb.edu/CalDrought/)

![](https://github.com/user-attachments/assets/36d0cc38-f028-4aca-b189-975405e8d2a3)


**Authors include:** Tom Gibbens-Matsuyama, Emma Bea Mitchell, Karol Paya, Takeen Shamloo

**Advisor:** Dr. Naomi Tague

**Client:** California Water Data Consortium

## About this repository:

This dashboard was created to to visualize urban water drought data currently located on the [CNRA website](https://data.cnra.ca.gov/dataset/urban-water-data-drought). The data is visualized in the following four ways: An interactive maps that allows users to locate water districts within California, several plotting types that allow users to see trends for each water district, summary statistics for the chosen water district, and missing information for the chosen water district. These visualizations are not meant to only view the current data, but to also give insight on what self-reported data is missing for specific districts. This is to incentivize districts to self report data on a regularly basis other than what is prompted by the state of California. This product is meant for our client, the California Water Data Consortium and other stakeholders working within the California's water domain. 

#### Content Overview

The primary files and folders are:

- `server.R`: Global environment that weaves UI and functions together, consists of many switch commands because of the interactiveness of the dashboard
- `ui.R`: Overarching UI for the dashboard display
- `functions/`: Functions for filtering, plot, summary statistics, and NA statistics outputs

The secondary folders consist are:

- `clean_names`: CSV files that incorporated water district name cleaning
- `data`: CSV and geospatial files

### Final Repository File Structure
```r
dashboard/
├── .gitignore
├── dashboard.Rproj
├── data/
│   ├── cached_data.rds
│   └── cal_drinking/
│       ├── California_Drinking_Water_System_Area_Boundaries.cpg
│       ├── California_Drinking_Water_System_Area_Boundaries.dbf
│       ├── California_Drinking_Water_System_Area_Boundaries.prj
│       ├── California_Drinking_Water_System_Area_Boundaries.shp
│       └── California_Drinking_Water_System_Area_Boundaries.shx
├── global.R
├── LICENSE
├── R/
│   ├── calculate_na_summary.R
│   ├── dashboard_functions.R
│   ├── data_cleaning.R
│   ├── name_cleaning.R
│   ├── sum_stat_functions.R
│   └── tmap_plot_functions.R
├── README.md
├── server.R
├── session_info.txt
├── text/
│   ├── about_box.Rmd
│   ├── about_CWDC.Rmd
│   ├── about_data_text.Rmd
│   ├── about_end.Rmd
│   ├── about_intro.Rmd
│   ├── about_limitations.Rmd
│   ├── home_end.Rmd
│   ├── tutorial_dataset_explanation.Rmd
│   ├── tutorial_dataset_select.Rmd
│   ├── tutorial_date_select.Rmd
│   ├── tutorial_district_select_map.Rmd
│   ├── tutorial_district_select.Rmd
│   ├── tutorial_end.Rmd
│   ├── tutorial_intro.Rmd
│   ├── tutorial_overview.Rmd
│   ├── tutorial_widget_four.Rmd
│   ├── tutorial_widget_one_hist_del.Rmd
│   ├── tutorial_widget_one_hist_prod.Rmd
│   ├── tutorial_widget_one_hist_total.Rmd
│   ├── tutorial_widget_one_hist.Rmd
│   ├── tutorial_widget_one.Rmd
│   ├── tutorial_widget_three.Rmd
│   └── tutorial_widget_two.Rmd
├── ui.R
└── www/
    ├── images/
    │   ├── 2022_11_18_FL_0440_Folsom_Lake.jpg
    │   ├── 2022_12_06_AI_0034_West_False_River.jpg
    │   ├── dashboard_overview.png
    │   ├── dwr_homepage_pic.png
    │   ├── KJ_oroville_0186_10_29_18.JPG
    │   ├── Screenshot 2025-05-31 123127.png
    │   ├── Screenshot 2025-05-31 123141.png
    │   ├── Screenshot 2025-05-31 123747.png
    │   ├── Screenshot 2025-05-31 123829.png
    │   ├── select_dataset.png
    │   ├── select_dates.png
    │   ├── select_district_map.png
    │   ├── select_district.png
    │   ├── widget_four.png
    │   ├── widget_one_hist_del.png
    │   ├── widget_one_hist_prod.png
    │   ├── widget_one_hist_total.png
    │   ├── widget_one_historical_selector.png
    │   ├── widget_one.png
    │   ├── widget_three.png
    │   └── widget_two.png
    └── theme.css
```
