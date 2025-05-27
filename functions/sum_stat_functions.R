##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------- FUNCTIONS FOR SUMMARY STATISTICS PORTION OF DASHBOARD -------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                     ~ Label Helper Function                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# START of pretty_label()
# Converts raw variable names into clean, human-readable labels.
# Optionally appends context string if used in a water type table.
pretty_label <- function(raw_label, water_type_context = FALSE) {
  
  # Map known labels to human-friendly titles
  label <- case_when(
    raw_label == "water_use_acre_feet"                  ~ "Water Use",
    raw_label == "water_supplies_acre_feet"             ~ "Water Supply",
    raw_label == "benefit_supply_augmentation_acre_feet"~ "Supply Augmentation",
    raw_label == "benefit_demand_reduction_acre_feet"   ~ "Demand Reduction",
    raw_label == "shortage_surplus_acre_feet"           ~ "Shortage/Surplus",
    raw_label == "total_produced"                       ~ "Total Produced",
    raw_label == "total_delivered"                      ~ "Total Delivered",
    TRUE ~ str_replace_all(raw_label, "_", " ") %>% str_to_title()
  )
  
  # Append unit context
  if (water_type_context) {
    paste0(label, " (Water in Acre-Feet)")
  } else {
    paste0(label, " (Acre-Feet)")
  }
}
# END of pretty_label()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  ~ 5-Year Outlook Summary Calculations                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# START of five_values_function_sum_stat()
# Returns total acre-feet by category (use, supply, augmentation, reduction)
# for a given organization over a selected year range.
five_values_function_sum_stat <- function(id, year_range) {
  
  water_data$five_year_outlook %>%
    
    # Filter for selected organization
    filter(org_id == id) %>%
    
    # Extract year from forecast start date
    mutate(forecast_year = lubridate::year(forecast_start_date)) %>%
    
    # Filter by selected year range
    filter(forecast_year >= year_range[1], forecast_year <= year_range[2]) %>%
    
    # Pivot to long format for easier grouping
    pivot_longer(cols = starts_with(c("water", "benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>%
    
    # Summarize total by category
    group_by(use_supply_aug_red) %>%
    summarize(total_value = sum(acre_feet, na.rm = TRUE), .groups = "drop")
}
# END of five_values_function_sum_stat()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                 ~ Monthly Outlook Summary Calculations                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# START of monthly_values_function_sum_stat()
# Returns total acre-feet by category (shortage/surplus, augmentation, reduction)
# for a selected date range and organization.
monthly_values_function_sum_stat <- function(id, date) {
  
  water_data$monthly_water_outlook %>%
    
    # Filter for organization
    filter(org_id == id) %>%
    
    # Create formatted year-month column
    mutate(year_month = format(forecast_start_date, "%Y-%m")) %>%
    
    # Filter by selected start and end dates
    filter(year_month >= format(as.Date(date[1]), "%Y-%m"),
           year_month <= format(as.Date(date[2]), "%Y-%m")) %>%
    
    # Pivot to long format for category-wise totals
    pivot_longer(cols = starts_with(c("shortage", "benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>%
    
    # Summarize total by category
    group_by(use_supply_aug_red) %>%
    summarize(total_acre_feet = sum(acre_feet, na.rm = TRUE), .groups = "drop")
}
# END of monthly_values_function_sum_stat()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  ~ Actual Shortage Summary Calculation                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# START of actual_filter_function_sum_stats()
# Returns filtered shortage level records by org ID and monthly date range.
actual_filter_function_sum_stats <- function(id, date) {
  
  # Generate list of year-month strings between the date range
  date_seq <- seq(
    from = lubridate::ymd(paste0(format(as.Date(date[1]), "%Y-%m"), "-01")),
    to   = lubridate::ymd(paste0(format(as.Date(date[2]), "%Y-%m"), "-01")),
    by   = "1 month"
  ) %>% format("%Y-%m")
  
  water_data$actual_shortage %>%
    
    # Filter for organization
    filter(org_id == id) %>%
    
    # Format date to year-month and match selected range
    mutate(year_month = format(start_date, "%Y-%m")) %>%
    filter(year_month %in% date_seq)
}
# END of actual_filter_function_sum_stats()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##             ~ Historical Production/Delivery Summary Calculation         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# START of hist_filt_function_sum_stats()
# Filters historical production and delivery data for a given organization
# and month-based date range.
hist_filt_function_sum_stats <- function(id, date) {
  
  # Generate list of year-month strings between the date range
  date_seq <- seq(
    from = lubridate::ymd(paste0(format(as.Date(date[1]), "%Y-%m"), "-01")),
    to   = lubridate::ymd(paste0(format(as.Date(date[2]), "%Y-%m"), "-01")),
    by   = "1 month"
  ) %>% format("%Y-%m")
  
  water_data$historical_production %>%
    
    # Filter for organization
    filter(org_id == id) %>%
    
    # Format and filter by date
    mutate(year_month = format(start_date, "%Y-%m")) %>%
    
    # Ensure NA and non-numeric values are handled
    mutate(quantity_acre_feet = as.numeric(quantity_acre_feet),
           quantity_acre_feet = replace_na(quantity_acre_feet, 0)) %>%
    
    # Keep only dates in specified range
    filter(year_month %in% date_seq)
}
# END of hist_filt_function_sum_stats()
