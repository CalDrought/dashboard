# Interactive Dashboard for Urban Water Shortage Data
#### This repository was created for the Data for Drought Resilience capstone project 

**Authors include:** Tom Gibbens-Matsuyama, Emma Bea Mitchell, Karol Paya, Takeen Shamloo

**Advisor:** Dr. Naomi Tague

**Client:** California Water Data Consortium

## Description:

This dashboard was created to to visualize urban water drought data currently located on the [CNRA website](https://data.cnra.ca.gov/dataset/urban-water-data-drought). The data is visualized in the following four ways: An interactive maps that allows users to locate water districts within California, several plotting types that allow users to see trends for each water district, summary statistics for the chosen water district, and missing information for the chosen water district. These visualizations are not meant to only view the current data, but to also give insight on what self-reported data is missing for specific districts. This is to incentivize districts to self report data on a regularly basis other than what is prompted by the state of California. This product is meant for our client, the California Water Data Consortium and other stakeholders working within the California's water domain. 

### Final Repository File Structure
```r
├── clean_names
│   ├── supplier_names_with_org_ids.csv
│   ├── supplier_table.csv
├── data/
│   ├── cal_drinking/
│   │
│   ├── five year water shortage outlook
│   ├── monthly water shortage outlook
│   ├── historical production and delivery
│   ├── actual water shortage levels
│   ├── source names
│   ├── water district geospatial
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
