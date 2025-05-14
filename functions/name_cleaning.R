
library(janitor)
library(tidyverse)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ clean names function  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_supplier_name <- function(data, col_name, new_col_name = "supplier_name") {
  col_sym <- rlang::sym(col_name)
  new_col_sym <- rlang::sym(new_col_name)
  
  data %>%
    clean_names() |> 
    mutate(
      !!new_col_sym := !!col_sym,
      !!new_col_sym := str_squish(!!new_col_sym),
      !!new_col_sym := str_replace_all(!!new_col_sym, "(?i)\\s+", " "),
      !!new_col_sym := str_trim(!!new_col_sym),
      
      # Add space after hyphen or slash
      !!new_col_sym := str_replace_all(!!new_col_sym, "([a-zA-Z])[-/]([a-zA-Z])", "\\1 - \\2"),
      
      
      # Remove entire (ventura)
      !!new_col_sym := str_replace_all(!!new_col_sym, regex("\\(ventura\\)", ignore_case = TRUE), ""),
      
      # Remove parentheses but keep the content
      !!new_col_sym := str_replace_all(!!new_col_sym, "[()]", ""),
      
      !!new_col_sym := str_replace_all(!!new_col_sym, "ter Agencyer", ""),
      
      # Normalize utility + region
      !!new_col_sym := str_replace(!!new_col_sym, "(?i)^(cal am water company|golden state water company)\\s+(\\S.*)$", "\\1 - \\2"),
      
      # Fix "mutual water co."
      !!new_col_sym := str_replace_all(!!new_col_sym, "(?i)mutual water co\\.?$", "mutual water company"),
      
      # Normalize all "City of" and "Town of" suffixes to be prefixes
      !!new_col_sym := str_replace_all(
        !!new_col_sym,
        regex("^(.+?)[,\\s-]*city of$", ignore_case = TRUE),
        "City Of \\1"
      ),
      !!new_col_sym := str_replace_all(
        !!new_col_sym,
        regex("^(.+?)[,\\s-]*town of$", ignore_case = TRUE),
        "Town Of \\1"
      ),
      !!new_col_sym := str_replace_all(
        !!new_col_sym,
        regex("^(.+?)[,\\s-]*city of[,\\s-]*(.*)$", ignore_case = TRUE),
        "City Of \\1 \\2"
      ),
      !!new_col_sym := str_replace_all(
        !!new_col_sym,
        regex("^(.+?)[,\\s-]*town of[,\\s-]*(.*)$", ignore_case = TRUE),
        "Town Of \\1 \\2"
      ),
      !!new_col_sym := str_replace_all(
        !!new_col_sym,
        regex("(.*)[,\\s-]+(city|town) of[,\\s-]*(.*)", ignore_case = TRUE),
        "\\2 Of \\1 \\3"
      ),
      
      # Common department renamings
      !!new_col_sym := str_replace(!!new_col_sym, "(\\b[a-z]+) city water dept\\.?$", "city of \\1 water department"),
      !!new_col_sym := str_replace(!!new_col_sym, "(\\b[a-z]+) water dept\\.?$", "city of \\1 water department"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "dept\\.?\\b", "department"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "pw", "public works"),
      
      # Suffix standardizations
      !!new_col_sym := str_replace_all(!!new_col_sym, "pud", "public utility district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "slvwd", "San Lorenzo Valley Water District"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "mwc", "mutual water company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "scwa", "sacramento county water agency"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bwc\\b", "water company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bwd\\b|\\bw\\.d\\.\\b", "water district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bi\\.d\\.?\\b", "irrigation district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bu\\.d\\b", "utility district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bc\\.s\\.d\\b", "community services district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bagencyd\\b", "agency"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bcorp\\.?\\b", "corporation"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\btud\\b", "tuolumne utilities district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bcsd\\b", "community services district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bsd\\b", "services district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bdist\\b", "district"),
      
      # Company spacing
      !!new_col_sym := str_replace_all(!!new_col_sym, "(?i)(company)\\.", "\\1"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "(?i)(company)(?=\\w)", "\\1 "),
      
      # Common abbreviations
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bCws\\b", "california water service"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bGolden State Wc\\b|\\bGswc\\b|\\bGolden State Water Co\\.?\\b", "golden state water company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bCal[- ]?Am\\b|\\bCal[- ]?American Water.*", "cal am water company"),
      
      # Fix Thousand Oaks variants
      !!new_col_sym := if_else(
        str_to_lower(!!new_col_sym) %in% c(
          "city of thousand oaks", "thousand oaks water dept",
          "thousand oaks city of", "	thousand oaks  city of",
          "thousand city of oaks water department",
          "thousand oaks city of"
        ),
        "city of thousand oaks water department",
        !!new_col_sym
      ),
      
      # Fix Port Hueneme variants
      !!new_col_sym := if_else(
        str_to_lower(!!new_col_sym) %in% c(
          "port hueneme city water dept", "port city of hueneme water department",
          "port city of hueneme water department", "city of port hueneme water department"
        ),
        "city of port hueneme water department",
        !!new_col_sym
      ),
      
      # Fix Chowchilla variants
      !!new_col_sym := if_else(
        str_to_lower(!!new_col_sym) %in% c(
          "city of chowchilla", "chowcilla, city of", "chowchilla, city of water department"
        ),
        "city of chowchilla water department",
        !!new_col_sym
      ),
      
      # Handle special cases
      !!new_col_sym := if_else(
        str_to_lower(!!new_col_sym) %in% c(
          "great oaks water companyin", "great oaks wc inc"
        ),
        "great oaks water company inc",
        !!new_col_sym
      ),
      
      # Other one-off fixes
      !!new_col_sym := str_replace_all(!!new_col_sym, "folsom, city of", "city of folsom"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "-\\s*$|[,\\.-]+\\s*$", ""),
      !!new_col_sym := str_replace_all(!!new_col_sym, "--", "-"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "- -", "-"),
      
      # Special misspellings
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bWirrigation Districtood\\b", "wildwood"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bWirrigation Districtor\\b", "windsor"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "maintence", "maintenance"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "San Dima", "- San Dimas"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "Bell, Bell Gardens|Bell-Bell Gardens", "bell/bell gardens"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "coachella vwater|coachella vwd", "Coachella Valley Water"),
      
      # Normalize hyphen spacing
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\s*-\\s*", " - "),
      
      # Clean up repeat words
      !!new_col_sym := str_replace_all(!!new_col_sym, "water company\\s+water company", "water company"),
      
      # Preserve KOA acronym
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bKoa\\b", "KOA"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "cal - am", "cal am"),     
      
      # Final cleanup
      !!new_col_sym := str_squish(!!new_col_sym),
      !!new_col_sym := str_to_title(!!new_col_sym)
    )
}
