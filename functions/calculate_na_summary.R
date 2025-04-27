#' Calculate missing values within a date range
#'
#' @param df Dataframe to check
#' @param date_column Name of the date column (character)
#' @param start_date Start date (string or Date)
#' @param end_date End date (string or Date)
#' @return A tibble with variable names and number of missing values

#input$dataset_selector	Dataset name ("actual_shortage", etc.)
#input$org_id	Organization ID (numeric or character)
#input$date_picker_start	Start date (from airDatepicker)
#input$date_picker_end	End date (from airDatepicker)

monthly_na <- function(filtered_data) {
  source("functions/dashboard_functions.R") # Load dashboard plot functions.
  
  # Call filtered data 
  monthly_na_summary <- filtered_data %>% 
    
    # Pivoting columns for easier computation 
    pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>% 
    
    # Group by new pivoted column 
    group_by(use_supply_aug_red) %>% 
    
    # Summarizing total observations, total NAs, and NA percentage 
    summarize(
      total_obs = n(),
      total_na = sum(is.na(acre_feet)),
      na_percentage = (total_na / total_obs) * 100,
      .groups = "drop" # IMPORTANT: prevents warnings about ungrouped data
    )
  
  return(monthly_na_summary)
}


actual_na <- function(filtered_data) {
  # Count total rows and NAs in state_standard_shortage_level
  actual_na_summary <- filtered_data %>% 
    summarize(
      total_obs = n(),  # total rows
      total_na = sum(is.na(state_standard_shortage_level)),  # NAs only in this column
      na_percentage = (total_na / total_obs) * 100,
      .groups = "drop"
    )
  
  return(actual_na_summary)
}

# test
start_ym_1 <- '2024-06'
end_ym_2   <- '2024-10'
id <- 23

filter<-monthly_filter(id,c(start_ym_1,end_ym_2))
monthly_na(filter)

filter<-actual_filter_function(id,c(start_ym_1,end_ym_2))
actual_obs<-actual_na(filter)

monthly_values_function <- function(id, date) {
  monthly_filter<-water_data$monthly_water_outlook %>%
    filter(org_id == id) %>%
    mutate(year_month = format(forecast_start_date, "%Y-%m")) %>%
    filter(year_month >= format(as.Date(date[1]), "%Y-%m"),
           year_month <= format(as.Date(date[2]), "%Y-%m")) %>%
    pivot_longer(cols = starts_with(c("shortage", "benefit")), names_to = "use_supply_aug_red", values_to = "acre_feet") %>%
    group_by(use_supply_aug_red) %>%
    summarize(total_acre_feet = sum(acre_feet, na.rm = TRUE), .groups = "drop")
  return(monthly_filter)
}

filter2<-monthly_values_function(id,c(start_ym_1,end_ym_2))
