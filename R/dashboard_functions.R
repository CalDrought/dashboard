##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- FIVE YEAR OUTLOOK FUNCTIONS---------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Five Year Filtering Function                      ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function for 5 Year Outlook Filtering
five_filter_function <- function(id, year) {
  
  # If year has two values, assume it's a range
  if (length(year) == 2) {
    year <- seq(from = as.numeric(year[1]), to = as.numeric(year[2]), by = 1)
  }
  
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
  five_year_filter$use_supply_aug_red <- factor(five_year_filter$use_supply_aug_red, # Make a character for plotly syntax, plotly doesn't like factors
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
five_year_plot <- function(id, year) {
  
  # Using five_filter_function for pre-filtered data
  filtered_data <- five_filter_function(id, year)
  
  # Generate the ggplot
  ggplot(filtered_data, aes(x = forecast_year, y = acre_feet, 
                            # Group to force order in plotly plot: i.e. Supply, Use, Supply Aug, Demand Red
                            fill = use_supply_aug_red, group = use_supply_aug_red)) + 
    
    # Geom col for a bar plot 
    geom_col(position = "dodge",
             aes(text = paste0("Year: ", forecast_year, 
                               "<br>Value: ", acre_feet,
                               "<br>Type: ", use_supply_aug_red))) + # dodge lines up bars side by side
    
    # Labels
    labs(x = "Forecast Year",
         y = "Quantity (Acre-Feet)",
         fill = "") + # Gets rid of plotly legend title
    
    # Add axis breaks
    scale_x_continuous(breaks = c(2021:2025)) +
    
    scale_fill_manual(values = c(
      "Supply" = "#E69F00",
      "Use" = "#56B4E9",
      "Supply Augmentation" = "#009E73",
      "Demand Reduction" = "#D55E00"
    )) +
    scale_y_continuous(labels = scales::comma_format()) +
    # Manually choose bar colors   
    # scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00" )) + 
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = rel(1.2), color = "black"),
      axis.text.y = element_text(size = rel(1.5), color = "black"),
      axis.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1))
    )
  
  
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
  hist_filter <- water_data$historical_production %>% 
    
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
    filter(year_month %in% date) %>% 
    
    # Capitalizing Observations in the following columns for a cleaner plot output
    mutate(water_produced_or_delivered = fct_recode(water_produced_or_delivered,
                                                    "Water Delivered" = "water delivered",
                                                    "Water Produced" = "water produced"),
           water_type = fct_relabel(water_type, ~ str_replace_all(., "_", " ") %>% 
                                      str_to_title()))
} 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    Historical Production Plot Function                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hist_plot_function <- function(id, date, water_types){
  
  pal <- c(
    "Total" = "#000000",
    "SFR" = "#004949",
    "Comm/Inst" = "#009292",
    "Ind" = "#ff6db6",
    "Landscape" = "#ffb6db",
    "MFR" = "#490092",
    "Other" = "#006ddb",
    "Other PWS" = "#b66dff",
    "NP Sold" = "#6db6ff",
    "Ag" = "#b6dbff",
    "Recycled" = "#920000",
    "Surface" = "#924900",
    "Ground" = "#db6d00",
    "NP" = "#24ff24",
    "Purchased" = "#ec5300",
    "Sold" = "#c10020"
  )
  
  hist_data <- hist_filt_function(id, date) %>%
    filter(water_type %in% water_types) %>%
    mutate(
      short_label = dplyr::recode(water_type,
                                  "Agriculture" = "Ag",
                                  "Single-Family Residential" = "SFR",
                                  "Commercial/Institutional" = "Comm/Inst",
                                  "Industrial" = "Ind",
                                  "Landscape Irrigation" = "Landscape",
                                  "Multi-Family Residential" = "MFR",
                                  "Other" = "Other",
                                  "Other Pws" = "Other PWS",
                                  "Non-Potable Water Sold To Another Pws" = "NP Sold",
                                  "Recycled" = "Recycled",
                                  "Surface Water" = "Surface",
                                  "Groundwater Wells" = "Ground",
                                  "Non-Potable (Total Excluded Recycled)" = "NP",
                                  "Purchased Or Received From Another Pws" = "Purchased",
                                  "Sold To Another Pws" = "Sold",
                                  "Total" = "Total"
      ),
      prod_deliv_simple = dplyr::recode(water_produced_or_delivered,
                                        "Water Produced" = "Produced",
                                        "Water Delivered" = "Delivered")
    )
  
  hist_plot <- ggplot(hist_data, aes(x = start_date, y = quantity_acre_feet)) +
    geom_line(aes(
      color = short_label,
      linetype = prod_deliv_simple,
      group = interaction(short_label, prod_deliv_simple),
      text = paste0(
        "Date: ", format(start_date, "%b %Y"), "<br>",
        "Water Type: ", water_type, "<br>",
        "Source: ", prod_deliv_simple, "<br>",
        "Quantity: ", scales::comma(round(quantity_acre_feet, 1)), " AF"
      )
    ), linewidth = 0.8) +
    
    scale_linetype_manual(values = c("solid", "dashed"), name = "") +
    scale_color_manual(values = pal, name = "") +
    scale_y_continuous(labels = scales::comma_format()) +
    
    labs(x = "Date", y = "Quantity (Acre-Feet)", fill = "Type, Produced/Delivered") +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = rel(1.2), color = "black"),
      axis.text.y = element_text(size = rel(1.2), color = "black"),
      axis.title = element_text(size = rel(1.4)),
      legend.title = element_text(size = rel(1.0)),
      legend.text = element_text(size = rel(0.8))
    )
  
  return(hist_plot)
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
  monthly_filter <- water_data$monthly_water_outlook %>% 
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
    
    filter(is_annual == "FALSE") %>%
    
    pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>%
    
    # ✅ Convert to factor with custom labels
    mutate(use_supply_aug_red = factor(
      use_supply_aug_red,
      levels = c("benefit_demand_reduction_acre_feet", 
                 "benefit_supply_augmentation_acre_feet", 
                 "shortage_surplus_acre_feet"),
      labels = c("Demand Reduction", "Supply Augmentation", "Shortage/Surplus")
    )) %>%
    
    ggplot(aes(x = forecast_start_date, y = acre_feet, fill = use_supply_aug_red)) + 
    geom_col(position = "stack",
             aes(text = paste0("Date: ", forecast_start_date,
                               "<br>Value: ", acre_feet,
                               "<br>Type: ", use_supply_aug_red))) +
    
    labs(x = "Date",
         y = "Acre-Feet",
         fill = "") +
    
    scale_fill_manual(values = c(
      "Demand Reduction" = "#D55E00",
      "Supply Augmentation" = "#009E73",
      "Shortage/Surplus" = "#56B4E9"
    )) +
    scale_y_continuous(labels = scales::comma_format()) +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = rel(1.2), color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = rel(1.5), color = "black"),
      axis.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1))
    )
  
  return(monthly_plot)
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
  actual_filter <- water_data$actual_shortage %>% 
    filter(org_id == id) %>% 
    
    mutate(year_month = format(start_date, "%Y-%m")) %>% 
    
    filter(year_month %in% date)
  
  return(actual_filter)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Actual Water Plot Function                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

actual_plot_function <- function(id, date){
  
  # Plot Water shortage levels for Goleta 
  ggplot(actual_filter_function(id, date), aes(x = start_date, y = state_standard_shortage_level)) +
    
    geom_col(
      fill = "orange3",
      width = 20,
      color = "orange3",
      aes(text = paste0("Month: ", format(start_date, "%b %Y"),
                        "<br>Shortage Level: ", state_standard_shortage_level))
    ) +
    
    # Format the x-axis to show month & year (e.g., "Mar 2022")
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "2 months",
      expand = c(0.01, 0.01)) +
    
    scale_y_continuous(
      breaks = c(1,2,3,4,5,6),
      limits = c(0,6)
    ) +
    
    scale_y_continuous(labels = scales::comma_format()) +
    
    labs(x = "Month",
         y = "Shortage Level") +
    
    theme_minimal(base_size = 13) +
    
    theme(
      axis.text.x = element_text(size = rel(1.2), color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = rel(1.5), color = "black"),
      axis.title = element_text(size = rel(1.5)),
      legend.text = element_text(size = rel(1))
    )
}

