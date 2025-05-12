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
├── clean_names
│   ├── supplier_names_with_org_ids.csv
│   ├── supplier_table.csv
├── data/
│   ├── cal_drinking/
│   │
│   ├── five_year_water_shortage_outlook.csv
│   ├── monthly_water_shortage_outlook.csv
│   ├── historical_production_delivery.csv
│   ├── actual_water_shortage_level.csv
│   ├── source_names.csv
│   ├── California_Drinking_Water_System_Area_Boundaries.csv
│   ├── California_Drinking_Water_System_Area_Boundaries.xml
├── functions/
    ├──dashboard_function.R
    ├──cleaning_functions.R
├── .gitignore
├── README.md
├── dashboard.Rproj
├── data_cleaning.R
├── Server.R
├── UI.R
└── README.md
```
