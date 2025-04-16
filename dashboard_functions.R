source("data_cleaning.R")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- FIVE YEAR OUTLOOK FUNCTIONS---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Five Year Filtering Function                      ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function for 5 Year Outlook Filtering
five_filter_function <- function(id, year = 2022) {
  
  # Create filtered df from original
  five_year_filter <- water_data$five_year_outlook %>% 
    
    # Filter data to specific org_id
    filter(org_id == id) %>%
    
    mutate(forecast_year = year(forecast_start_date)) %>% 
    
    filter(forecast_year %in% year) %>% 
    
    # Combining use, supply, augmentation, reduction into one columns
    pivot_longer(cols = c(starts_with("water"), starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet")
  
  # Renaming & reordering the observations for the plot & table outputs
  five_year_filter$use_supply_aug_red <- factor(five_year_filter$use_supply_aug_red,
                                                levels = c("water_supplies_acre_feet", # reordering observations
                                                           "water_use_acre_feet",
                                                           "benefit_supply_augmentation_acre_feet",
                                                           "benefit_demand_reduction_acre_feet"),
                                                labels = c("Supply", "Use",  # renaming observations
                                                           "Supply Augmentation", "Demand Reduction")) 
  
  return(five_year_filter)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Five Year Plot Function                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function for 5 Year Outlook Plot
five_year_plot <- function(id, year = 2022) {
  
  # Using five_filter_function for pre-filtered data
  filtered_data <- five_filter_function(id, year)
  
  # Generate the ggplot
  ggplot(filtered_data, aes(x = forecast_year, y = acre_feet, fill = use_supply_aug_red)) +
    
    # Geom col for a bar plot 
    geom_col(position = "dodge") + # dodge lines up bars side by side
    
    # Labels
    labs(x = "Forecast Date",
         y = "Acre-Feet",
         fill = element_blank()) + # Get's rid of the legend
    
    # Add axis breaks
    scale_x_continuous(breaks = c(2021:2025)) +
    
    # Manually choose bar colors   
    scale_fill_manual(values = c("coral", "steelblue", "forestgreen", "violetred")) + 
    theme_minimal()
  
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Five Year NA Values Function                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function for 5 Year Outlook NA values calculation
five_na_function <- function(id, year){
  
  # Use filtered data
  five_year_na <- five_filter_function(id, year)
  
  # Calculate NAs
  five_year_na %>% 
    
    # Group by the four metrics we are interested in
    group_by(use_supply_aug_red) %>% 
    
    # Calculate total rows, total NAs, percentage
    summarize(total_obs = n(),
              total_na = sum(is.na(acre_feet)),
              na_percentage = total_na / total_obs * 100)
  
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Five Year Values Function                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function for 5 Year Outlook Value calculation
five_values_function <- function(id, year){
  
  # Use filtered data
  five_values <- five_filter_function(id, year)
  
  five_values %>% 
    
    # Group metrics we are interested in 
    group_by(use_supply_aug_red) %>% 
    
    # Summarize there total acre_feet 
    summarize(total_value = sum(acre_feet))
  
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- Historical Production Functions ------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  Historical Production Filtering Function                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


hist_filt_function <- function(id, date){
  
  # If date arguemnt has 2 values, assume it's a start/end range and expand it
  if (length(date) == 2) {
    date <- seq(
      from = lubridate::ymd(paste0(date[1], "-01")), # converts into a Date type
      
      to   = lubridate::ymd(paste0(date[2], "-01")), # converts into a Date type
      
      by   = "1 month" # sequences by month
      
    ) %>% format("%Y-%m") # formats back into Year and month, same as input values
  }
  
  # Start of historical production filtering
  hist_filter <- historical_production %>% 
    
    # filter to org_id
    filter(org_id == id) %>% 
    
    # Create new forecast year column
    mutate(year_month = format(start_date, "%Y-%m"))  
  
  hist_total <- hist_filter %>% 
    # Group by date 
    group_by(start_date) %>% 
    
    # Append a new row for each start_date & produced_delivered calculating the "total" 
    bind_rows(
      hist_filter %>% 
        group_by(start_date, water_produced_or_delivered) %>% 
        
        # Summarize is producing a new total row by the group by above 
        summarize(water_type = "total",
                  quantity_acre_feet = sum(quantity_acre_feet, na.rm = TRUE),
                  
                  # Using unique returns the "original" observation of these columns,
                  # These are not being changed 
                  start_date = unique(start_date),
                  end_date = unique(end_date),
                  pwsid = unique(pwsid),
                  water_system_name = unique(water_system_name),
                  org_id = unique(org_id),
                  year_month = unique(year_month),
                  .groups = "drop_last")
    ) %>% 
    ungroup() %>% 
    # Filter for a given year range
    filter(year_month %in% date)
} 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    Historical Production Plot Function                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hist_plot_function <- function(id, date, type){
  
  # Use filtered historical production data
  hist_plot <- hist_filt_function(id, date ) %>% 
    
    # filter for water type
    filter(water_type %in% type) %>% 
    
    # Plot filtered data 
    ggplot(aes(x = start_date, y = quantity_acre_feet, color = water_type, linetype = water_produced_or_delivered)) +
    geom_line() +
    labs(x = "Date",
         y = "Quantity (Acre-Feet)") +
    theme_minimal()
  
  return(hist_plot)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            Historical Production NA for Total Delivered/Produced         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


hist_na_total_function <- function(id, date){
  
  # Calculating NAs 
  hist_na <- hist_filt_function(id, date) %>% 
    
    # Filter out total b/c we created this, not a part of the original data
    # Don't want to include it in our calculation 
    filter(water_type != "total") %>% 
    group_by(water_produced_or_delivered) %>% 
    
    # Calculate total observations, total NAs, and NA percentage 
    summarize(total_obs = n(),
              total_na = sum(is.na(quantity_acre_feet)),
              na_percentage = total_na / total_obs * 100)
  
  return(hist_na)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Historical Production NA calculation for Specific water types     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hist_na_specific_function <- function(id, date, type){
  
  hist_na_specific <- hist_filt_function(id, date) %>% 
    
    # Filter out total b/c we created this, not a part of the original data
    # Don't want to include it in our calculation 
    filter(water_type != "total") %>% 
    group_by(water_produced_or_delivered, water_type) %>% 
    
    # Calculate total observations, total NAs, and NA percentage 
    summarize(total_obs = n(),
              total_na = sum(is.na(quantity_acre_feet)),
              na_percentage = total_na / total_obs * 100) %>% 
    
    # Filter to only the variables we want 
    # In this case these are randomly selected
    filter(water_type %in% type)
  
  return(hist_na_specific)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Historical Production Total Delivered/Produced Calculation Function    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


hist_values_function <- function(id, date){
  # Calculating total values for "total produced/delivered"
  hist_values <- hist_filt_function(id, date) %>% 
    
    # Filter out "total" as its not a part of the original data
    filter(water_type != "total") %>% 
    group_by(water_produced_or_delivered) %>% 
    
    # Summing quantity acre-feet for total 
    summarize(total_value = sum(quantity_acre_feet, na.rm = TRUE))
  
  return(hist_values)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Historical Production Specific Water Type Value Calculation       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hist_values_specific <- function(id, date, type){
  
  # Calculating values for specific water types chosen above 
  hist_values_specific <- hist_filt_function(id, date) %>% 
    
    # Filter out "total" as it's not a part of the original data
    filter(water_type != "total") %>% 
    group_by(water_produced_or_delivered, water_type) %>% 
    
    # Calculating totals and then filtered to chosen water types 
    summarize(total_value = sum(quantity_acre_feet, na.rm = TRUE)) %>% 
    filter(water_type %in% type)
  
  return(hist_values_specific)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------- MONTHLY WATER OUTLOOK FUNCTIONS-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Monthly Water Filtering Function                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_filter <- function(id, date){
  
  # If date arguemnt has 2 values, assume it's a start/end range and expand it
  if (length(date) == 2) {
    date <- seq(
      from = lubridate::ymd(paste0(date[1], "-01")), # Converts into Date type
      
      to   = lubridate::ymd(paste0(date[2], "-01")), # Converts into Date type
      
      by   = "1 month" # sequences by month 
    ) %>% format("%Y-%m") # Reformats back into year and month, same as input values
  }
  
  # Filter to org_id
  monthly_filter <- monthly_water_outlook %>% 
    filter(org_id == id) %>% 
    
    # Create new forecast year column
    mutate(year_month = format(forecast_start_date, "%Y-%m")) %>% 
    
    filter(year_month %in% date)
  
  return(monthly_filter)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Monthly Water Plot Function                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_plot_function <- function(id, date){
  
  monthly_plot <- monthly_filter(id, date) %>% 
    
    # Filter out annual reports, interested in monthly metrics
    filter(is_annual == "FALSE") %>%
    
    # Pivot longer shortage surplus, benefit demand reduction and benefit supply aug columns
    # Want information in one column for easier access when plotting
    pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>% 
    
    # Plot data 
    ggplot(aes(x = forecast_start_date, y = acre_feet, fill = use_supply_aug_red)) + 
    geom_col(position = "dodge") +
    labs(x = "Date",
         y = "Acre-Feet",
         fill = element_blank()) +
    
    # Changing labels for graph representation 
    scale_fill_discrete(labels = c("Demand Reduction", "Supply Augmentation", "Shortage/Surplus")) +
    theme_minimal()
  
  return(monthly_plot)
  
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    Monthly Water NA Values Calculation                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_na_function <- function(id, date){
  
  # Calculate alameda NAs for dashboard 
  monthly_na <- monthly_filter(id, date) %>% 
    
    # Pivoting columns for easier computation 
    pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>% 
    
    # Group by new pivoted column 
    group_by(use_supply_aug_red) %>% 
    
    # Summarizing total observations, total NAs, and NA percentage 
    summarize(total_obs = n(),
              total_na = sum(is.na(acre_feet)),
              na_percentage = total_na / total_obs * 100)
  
  return(monthly_na)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Monthly Water Value Calculation                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_values_function <- function(id, date){
  
  # Calculate total values 
  monthly_values <- monthly_filter(id, date) %>% 
    
    # Pivot columns for easier computation 
    pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>% 
    
    # Group by new column and summarize total acre-feet
    group_by(use_supply_aug_red) %>% 
    summarize(total = sum(acre_feet, na.rm = TRUE))
  
  return(monthly_values)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                 Monthly Water Number of Months Calculation               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_months_function <- function(id, date){
  
  monthly_months <- monthly_filter(id, date) %>% 
    
    # Pivot columns for easier computation
    pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>% 
    
    # Remove total annual observations
    filter(is_annual == FALSE,
           
           # Filter where observations not = 0 
           acre_feet != 0) %>% 
    
    # Group by new column and summarize total acre-feet
    group_by(use_supply_aug_red) %>% 
    summarize(num_months = n())
  
  return(monthly_months)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------- ACTUAL SHORTAGE FUNCTIONS---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                     Actual Shortage Filtering Function                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

actual_filter_function <- function(id, date){
  
  # If date arguemnt has 2 values, assume it's a start/end range and expand it
  if (length(date) == 2) {
    date <- seq(
      from = lubridate::ymd(paste0(date[1], "-01")), # Convert to Date type
      
      to   = lubridate::ymd(paste0(date[2], "-01")), # Convert to Date type
      
      by   = "1 month" # Sequence by month
      
    ) %>% format("%Y-%m") # Format back to Year and month, same as original input
  }
  
  
  # Filter water shortage to Goleta 
  actual_filter <- actual_shortage %>% 
    filter(org_id == id) %>% 
    
    mutate(year_month = format(start_date, "%Y-%m")) %>% 
    
    filter(year_month %in% date)
  
  return(actual_filter)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Actual Water Plot Function                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    Actual Water NA Values Calculation                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

actual_values_function <- function(id, date){
  
  # Calculating total values
  actual_values <- actual_filter_function(id, date) %>% 
    summarize(average = mean(state_standard_shortage_level, na.rm = TRUE))
  
  return(actual_values)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Actual Water Values Calculation                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

actual_values_function <- function(id, date){
  
  # Calculating total values
  actual_values <- actual_filter_function(id, date) %>% 
    summarize(average = mean(state_standard_shortage_level, na.rm = TRUE))
  
  return(actual_values)
}
