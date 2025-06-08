# ---------- Helper function for downloading from CKAN-Datastore ----------
fetch_cnra_csv <- function(resource_id, show_types = FALSE) {
  url <- sprintf(
    "https://data.cnra.ca.gov/datastore/dump/%s?bom=true",
    resource_id
  )
  read_csv(url, show_col_types = show_types)
}

# ---------- Storing api keys for respective datasets ----------
RESOURCE_ID <- list(
  actual_shortage       = "c3832742-5525-442d-b762-59453d500aba",
  five_year_outlook     = "77b47bbe-7e09-4580-9eb1-092c035099e9",
  historical_production = "b73488e4-5630-4d35-a4d5-ce3b879b8546",
  monthly_water_outlook = "caf0ef52-98f9-48f1-af4c-025760fd80b2",
  source_name           = "9688aeb1-6c77-44aa-9cf7-6b1c1306f399"
)

# ---------- Loading Datasets on Call ----------
load_water_data <- function() {
  
  # Fetching actual_shortage using api key & cleaning names.
  actual_shortage <- fetch_cnra_csv(RESOURCE_ID$actual_shortage) |>
    clean_names() |>
    select(-c(id))
  
  # Fetching five_year_outlook using api key & cleaning names.
  five_year_outlook <- fetch_cnra_csv(RESOURCE_ID$five_year_outlook) |>
    clean_names() |>
    select(-c(id))
  
  # Fetching historical_production data using api key & cleaning names.
  historical_production <- fetch_cnra_csv(RESOURCE_ID$historical_production) |>
    clean_names() |>
    select(-c(id))
  
  # Fetching monthly_water_outlook data using api key & cleaning names.
  monthly_water_outlook <- fetch_cnra_csv(RESOURCE_ID$monthly_water_outlook) |>
    clean_names() |>
    select(-c(id))
  
  # Fetching source_name data using api key & cleaning names.
  source_name <- fetch_cnra_csv(RESOURCE_ID$source_name) |>
    clean_names() |>
    select(-c(id))
  
  # Cleaning the supplier names for each of our main datasets.
  historical_clean <- clean_supplier_name(historical_production, "water_system_name") |> 
    mutate(supplier_name = water_system_name)
  fiveyr_clean <- clean_supplier_name(five_year_outlook, "supplier_name")
  monthly_clean <- clean_supplier_name(monthly_water_outlook, "supplier_name")
  actual_clean <- clean_supplier_name(actual_shortage, "supplier_name")
  
  # Creating a list of the cleaned supplier names with their org_id's as one df.
  supplier_data <- bind_rows(
    historical_clean |> select(org_id, supplier_name),
    fiveyr_clean |> select(org_id, supplier_name),
    monthly_clean |> select(org_id, supplier_name),
    actual_clean |> select(org_id, supplier_name)
  ) |>
    distinct() |> 
    mutate(name_with_id = paste(supplier_name, "-", org_id))
  
  
  list(
    actual_shortage = actual_shortage,
    five_year_outlook = five_year_outlook,
    historical_production = historical_production,
    monthly_water_outlook = monthly_water_outlook,
    supplier_data = supplier_data,
    source_name = source_name
  )
}

# --------- Loading Spatial Data ----------
load_spatial_data <- function() {

  # ------- Importing & Cleaning district boundaries for our Tmap render.
  district_shape <- read_sf(here("data", "cal_drinking", "California_Drinking_Water_System_Area_Boundaries.shp")) |> 
    clean_names() |> # Clean up column names. 
    select(c("water_syst", "geometry")) |> 
    st_transform("EPSG:4269") # |> # Change CRS of district_shape data.
    # st_make_valid()
  
  # Keep ~5% of vertices but preserves boundaries.
  district_shape <- ms_simplify(district_shape, keep = 0.05, keep_shapes = TRUE)
  
  district_shape <- st_make_valid(district_shape)   # then validity check
  
  list(
    district_shape = district_shape
    # source_geo = source_geo
  )
}