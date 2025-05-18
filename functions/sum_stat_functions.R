
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------- FUNCTIONS FOR SUMMARY STATISTICS PORTION OF DASHBOARD-------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ label helper function  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# function to make the labels on the dashboard look nice (make into title case)
pretty_label <- function(raw_label, water_type_context = FALSE) {
  label <- case_when(
    raw_label == "water_use_acre_feet" ~ "Water Use",
    raw_label == "water_supplies_acre_feet" ~ "Water Supply",
    raw_label == "benefit_supply_augmentation_acre_feet" ~ "Supply Augmentation",
    raw_label == "benefit_demand_reduction_acre_feet" ~ "Demand Reduction",
    raw_label == "shortage_surplus_acre_feet" ~ "Shortage/Surplus",
    raw_label == "total_produced" ~ "Total Produced",
    raw_label == "total_delivered" ~ "Total Delivered",
    TRUE ~ str_replace_all(raw_label, "_", " ") %>% str_to_title()
  )
  if (water_type_context) {
    paste0(label, " (Water in Acre-Feet)")
  } else {
    paste0(label, " (Acre-Feet)")
  }
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ five year outlook value box calculations  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

five_values_function_sum_stat <- function(id, year_range) {
  # call five year outlook data set from water_data
  water_data$five_year_outlook %>%
    # match input id to org_ids in data set
    filter(org_id == id) %>%
    # find year of forecast_start_date and create forecast_year column
    mutate(forecast_year = lubridate::year(forecast_start_date)) %>%
    # create filter for year_range for input into function
    filter(forecast_year >= year_range[1], forecast_year <= year_range[2]) %>%
    # pivot longer to make it easier to find values
    pivot_longer(cols = starts_with(c("water", "benefit")), names_to = "use_supply_aug_red", values_to = "acre_feet") %>%
    # find total value for value box of aug, red, supply, use
    group_by(use_supply_aug_red) %>%
    summarize(total_value = sum(acre_feet, na.rm = TRUE), .groups = "drop")
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ monthly outlook value box calculations  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_values_function_sum_stat <- function(id, date) {
  # call monthly water outlook data set from water_data
  water_data$monthly_water_outlook %>%
    # match input id to org_ids in data set
    filter(org_id == id) %>%
    # create new column with year, month format for forecast start dates
    mutate(year_month = format(forecast_start_date, "%Y-%m")) %>%
    # filter data for inputted date range
    filter(year_month >= format(as.Date(date[1]), "%Y-%m"),
           year_month <= format(as.Date(date[2]), "%Y-%m")) %>%
    # pivot longer to make it easier to find values
    pivot_longer(cols = starts_with(c("shortage", "benefit")), names_to = "use_supply_aug_red", values_to = "acre_feet") %>%
    # find total value for value box of aug, red, supply, use
    group_by(use_supply_aug_red) %>%
    summarize(total_acre_feet = sum(acre_feet, na.rm = TRUE), .groups = "drop")
}

monthly_months_function <- function(id, date) {
  # call monthly water outlook data set from water_data
  water_data$monthly_water_outlook %>%
    # match input id to org_ids in data set
    filter(org_id == id) %>%
    # create new column with year, month format for forecast start dates
    mutate(year_month = format(forecast_start_date, "%Y-%m")) %>%
    # filter data for inputted date range
    filter(year_month >= format(as.Date(date[1]), "%Y-%m"),
           year_month <= format(as.Date(date[2]), "%Y-%m")) %>%
    # pivot longer to make it easier to find values
    pivot_longer(cols = starts_with(c("shortage", "benefit")), names_to = "use_supply_aug_red", values_to = "acre_feet") %>%
    # find number of months in date range of aug, red, shortage/surplus
    group_by(use_supply_aug_red) %>%
    summarize(num_months = n(), .groups = "drop")
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ actual shortage value box calculations  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

actual_filter_function_sum_stats <- function(id, date){
  date_seq <- seq(
    from = lubridate::ymd(paste0(format(as.Date(date[1]), "%Y-%m"), "-01")),
    to = lubridate::ymd(paste0(format(as.Date(date[2]), "%Y-%m"), "-01")),
    by = "1 month"
  ) %>% format("%Y-%m")
  
  water_data$actual_shortage %>%
    filter(org_id == id) %>%
    mutate(year_month = format(start_date, "%Y-%m")) %>%
    filter(year_month %in% date_seq)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ historical production value box calculations  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hist_filt_function_sum_stats <- function(id, date) {
  date_seq <- seq(
    from = lubridate::ymd(paste0(format(as.Date(date[1]), "%Y-%m"), "-01")),
    to = lubridate::ymd(paste0(format(as.Date(date[2]), "%Y-%m"), "-01")),
    by = "1 month"
  ) %>% format("%Y-%m")
  
  water_data$historical_production %>%
    filter(org_id == id) %>%
    mutate(year_month = format(start_date, "%Y-%m")) %>%
    mutate(quantity_acre_feet = as.numeric(quantity_acre_feet),
           quantity_acre_feet = replace_na(quantity_acre_feet, 0)) %>%
    filter(year_month %in% date_seq)
}

