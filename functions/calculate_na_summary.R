#' Calculate NA Summary for selected dataset
#' @param df The dataset selected
#' @param date_col The column containing dates
#' @param start_date Start date for filtering
#' @param end_date End date for filtering
#' @return A tibble with variable names and their number of NAs

library(readr)
df <- read_csv('/data/actual_water_shortage_level.csv')

calculate_na_summary <- function(df, date_col, start_date, end_date) {
  df_filtered <- df %>%
    filter(
      !!sym(date_col) >= as.Date(start_date),
      !!sym(date_col) <= as.Date(end_date)
    )
  
  na_counts <- df_filtered %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Number_of_NAs") %>%
    arrange(desc(Number_of_NAs))
  
  return(na_counts)
}
