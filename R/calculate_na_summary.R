#' Calculate missing values within a date range
#'
#' @param df Dataframe to check
#' @param date_column Name of the date column (character)
#' @param start_date Start date (string or Date)
#' @param end_date End date (string or Date)
#' @return A tibble with variable names and number of missing values


# -----------------------------------------
# --- Filtering ---
# -----------------------------------------

actual_filter <- function(id, date) {
  start_date <- as.Date(paste0(date[1], "-01"))
  end_date   <- as.Date(paste0(date[2], "-01"))
  
  # Check that start_date and end_date are valid
  if (is.na(start_date) || is.na(end_date)) {
    stop("Start date or end date is not valid.")
  }
  
  # Generate a month sequence
  date_seq <- seq(
    from = lubridate::floor_date(start_date, "month"),
    to = lubridate::floor_date(end_date, "month"),
    by = "1 month"
  ) %>% format("%Y-%m")
  
  water_data$actual_shortage %>%
    filter(org_id == id) %>%
    mutate(year_month = format(start_date, "%Y-%m")) %>%
    filter(year_month %in% date_seq)
  
}

monthlywater_filter <- function(id, date) {
  
  # date[1] and date[2] are already Dates! No need to paste "-01"
  start_date <- as.Date(date[1])
  end_date   <- as.Date(date[2])
  
  # Safety check
  if (is.na(start_date) | is.na(end_date)) {
    stop("One or both of the dates are not in a valid format.")
  }
  
  # Filter the dataset for the given org_id and date range
  monthly_filter <- water_data$monthly_water_outlook %>%
    filter(org_id == id) %>%
    mutate(year_month = format(forecast_start_date, "%Y-%m")) %>%
    filter(
      forecast_start_date >= start_date,
      forecast_start_date <= end_date
    )
  
  # Check if no data was returned
  if (nrow(monthly_filter) == 0) {
    return(NULL)  # Or return a message like "No data found"
  }
  
  return(monthly_filter)
}

# 5 yr filter function
fiveyr_filter<- function(id, year_range) {
  # Assume year_range is a vector of two full dates like "2021-01-01"
  
  start_year <- lubridate::year(as.Date(year_range[1]))
  end_year   <- lubridate::year(as.Date(year_range[2]))
  
  if (is.na(start_year) || is.na(end_year)) {
    stop("Start year or end year is invalid.")
  }
  
  # Filter dataset by org_id and year
  water_data$five_year_outlook %>%
    filter(org_id == id) %>%
    mutate(forecast_year = lubridate::year(forecast_start_date)) %>%
    filter(forecast_year >= start_year, forecast_year <= end_year)
}

#Historical

historical_filtering <- function(id, date, type = NULL) {
  if (length(date) == 2) {
    start_date <- lubridate::ymd(date[1])  
    end_date <- lubridate::ymd(date[2])    
  } else {
    start_date <- lubridate::ymd(date)
    end_date <- start_date
  }
  
  date_seq <- seq(
    from = lubridate::floor_date(start_date, "month"),
    to = lubridate::floor_date(end_date, "month"),
    by = "1 month"
  ) %>% format("%Y-%m")
  
  hist_filter <- water_data$historical_production %>%
    filter(org_id == id) %>%
    mutate(year_month = format(start_date, "%Y-%m")) %>%
    filter(year_month %in% date_seq | is.na(year_month))
  
  if (!is.null(type)) {
    hist_filter <- hist_filter %>%
      mutate(water_type = tolower(water_type)) %>%      # lowercase your data
      filter(water_type %in% tolower(type))              # lowercase your input selection
  }
  
  return(hist_filter)
}


# -----------------------------------------
# --- Missing Information Calculation ---
# -----------------------------------------

monthly_na <- function(filtered_data) {
  
  # Assuming filtered_data contains the required columns
  monthly_na_summary <- filtered_data %>% 
    summarize(
      shortage_na = sum(is.na(shortage_surplus_acre_feet))/ n() * 100,
      augmentation_na = sum(is.na(benefit_supply_augmentation_acre_feet))/ n() * 100,
      demand_red_na = sum(is.na(benefit_demand_reduction_acre_feet))/ n() * 100
    )
  
  # Return the summary
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

# 5 year NAN function
fiveyr_na <- function(filtered_data) {
  if (nrow(filtered_data) == 0) {
    return(tibble(
      water_use_na = NA_real_,
      water_supplies_na = NA_real_,
      benefit_supply_na = NA_real_,
      benefit_demand_na = NA_real_
    ))
  }
  
  na_summary <- filtered_data %>%
    summarize(
      water_use_na = sum(is.na(water_use_acre_feet)) / n() * 100,
      water_supplies_na = sum(is.na(water_supplies_acre_feet)) / n() * 100,
      benefit_supply_na = sum(is.na(benefit_supply_augmentation_acre_feet)) / n() * 100,
      benefit_demand_na = sum(is.na(benefit_demand_reduction_acre_feet)) / n() * 100
    )
  return(na_summary)
}


hist_na <- function(historical_filtering) {
  historical_filtering %>%
    filter(water_type != "total") %>%
    group_by(water_produced_or_delivered, water_type) %>%
    summarize(na_percentage = sum(is.na(quantity_acre_feet)) / n() * 100, .groups = "drop")
}


# testing functions - DELETE WHEN DONE
# start_ym_1 <- '2013-01-01'
# end_ym_2   <- '2024-01-01'
# id <- 23
# type <-c('recycled','industrial')

# combined_water_types <- reactive({
#   delivered <- input$delivered_type
#   produced  <- input$produced_type
# 
# #actual
# filter<-actual_filter_function(id,c(start_ym_1,end_ym_2))
# actual_obs<-actual_na(filter)
# 
# #monthly
# filter<-monthly_filter(id,c(start_ym_1,end_ym_2))
# monthly<-monthly_na(filter)
# 
# filter3<-five_filter_function(440,c(start_ym_1,end_ym_2))
# monthly3<-fiveyr_na(filter3)
# 
# filter2<-monthly_values_filter(id,c(start_ym_1,end_ym_2))
# monthly2<-monthly_na(filter2)


# filter4<-historical_filtering(id,c(start_ym_1,end_ym_2),type)
# hist<-hist_na(filter4)
