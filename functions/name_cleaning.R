##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Clean Supplier Name Function                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# START of clean_supplier_name()
# This function takes a dataset and column name, applies a series of string cleaning
# and standardization operations, and returns a new cleaned supplier name column.
# It ensures naming consistency for matching across datasets or visualizing in dashboards.

clean_supplier_name <- function(data, col_name, new_col_name = "supplier_name") {
  
  # Capture column names as symbols for tidy evaluation
  col_sym <- rlang::sym(col_name)
  new_col_sym <- rlang::sym(new_col_name)
  
  data %>%
    clean_names() %>%  # Start by cleaning all column names (snake_case)
    
    mutate(
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                     Step 1: Extract + normalize base string
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      !!new_col_sym := !!col_sym,
      !!new_col_sym := str_squish(!!new_col_sym),  # Remove excess spaces
      !!new_col_sym := str_replace_all(!!new_col_sym, "(?i)\\s+", " "),
      !!new_col_sym := str_trim(!!new_col_sym),    # Trim outer whitespace
      
      # Remove " (Ventura)" completely
      !!new_col_sym := str_replace_all(!!new_col_sym, regex("\\(ventura\\)", ignore_case = TRUE), ""),
      
      # Strip remaining parentheses but keep their contents
      !!new_col_sym := str_replace_all(!!new_col_sym, "[()]", ""),
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##             Step 2: Structural normalization (hyphens, slashes, etc.)
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Add space around hyphens/slashes (e.g., City/County → City / County)
      !!new_col_sym := str_replace_all(!!new_col_sym, "([a-zA-Z])[-/]([a-zA-Z])", "\\1 - \\2"),
      
      # Fix awkward joins from prior transformations
      !!new_col_sym := str_replace_all(!!new_col_sym, "(-\\s*$|[,\\.-]+\\s*$)", ""),  # trailing junk
      !!new_col_sym := str_replace_all(!!new_col_sym, "--", "-"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "- -", "-"),
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##            Step 3: Normalize known city/town patterns (suffix → prefix)
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Convert suffixes like "Folsom, City Of" → "City Of Folsom"
      !!new_col_sym := str_replace_all(!!new_col_sym, regex("^(.+?)[,\\s-]*city of$", ignore_case = TRUE), "City Of \\1"),
      !!new_col_sym := str_replace_all(!!new_col_sym, regex("^(.+?)[,\\s-]*town of$", ignore_case = TRUE), "Town Of \\1"),
      !!new_col_sym := str_replace_all(!!new_col_sym, regex("^(.+?)[,\\s-]*city of[,\\s-]*(.*)$", ignore_case = TRUE), "City Of \\1 \\2"),
      !!new_col_sym := str_replace_all(!!new_col_sym, regex("^(.+?)[,\\s-]*town of[,\\s-]*(.*)$", ignore_case = TRUE), "Town Of \\1 \\2"),
      !!new_col_sym := str_replace_all(!!new_col_sym, regex("(.*)[,\\s-]+(city|town) of[,\\s-]*(.*)", ignore_case = TRUE), "\\2 Of \\1 \\3"),
      !!new_col_sym := str_replace_all(!!new_col_sym, regex("^(.+?)[\\s,-]+city\\b", ignore_case = TRUE), "City Of \\1"),
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##               Step 4: Normalize known departments & utilities
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      !!new_col_sym := str_replace(!!new_col_sym, "(\\b[a-z]+) city water dept\\.?$", "city of \\1 water department"),
      !!new_col_sym := str_replace(!!new_col_sym, "(\\b[a-z]+) water dept\\.?$", "city of \\1 water department"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "dept\\.?\\b", "department"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bpw\\b", "public works"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bawa\\b", "amador water district"),
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##               Step 5: Suffix and acronym standardization
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bmutual water co\\.?$", "mutual water company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bpud\\b", "public utility district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bslvwd\\b", "San Lorenzo Valley Water District"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bmwc\\b", "mutual water company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bscwa\\b", "sacramento county water agency"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bwc\\b", "water company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bwd\\b|\\bw\\.d\\.\\b", "water district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\b(i\\.d\\.?|id)\\b", "irrigation district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bu\\.d\\b", "utility district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bc\\.s\\.d\\b", "community services district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bagencyd\\b", "agency"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bcorp\\.?\\b", "corporation"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\btud\\b", "tuolumne utilities district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bcsd\\b", "community services district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bcwd\\b", "community water district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bsd\\b", "services district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bdist\\b", "district"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bcwwd\\b", "county water works district"),
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##            Step 6: Company renaming and known abbreviation cleanup
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bCal[- ]?Am\\b|\\bCal[- ]?American Water.*", "cal am water company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bGolden State Wc\\b|\\bGswc\\b|\\bGolden State Water Co\\.?\\b|gswc", "golden state water company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bcws\\b", "california water service"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bdiv\\b", "division"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bco\\.?\\b", "company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bmwd\\.?\\b", "municipal water district"),
      
      # Ensure proper spacing around company
      !!new_col_sym := str_replace_all(!!new_col_sym, "(?i)(company)(?=\\w)", "\\1 "),
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                Step 7: Known edge case corrections
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Thousand Oaks
      !!new_col_sym := if_else(
        str_to_lower(!!new_col_sym) %in% c(
          "city of thousand oaks", "thousand oaks water dept", "thousand oaks city of", "thousand oaks  city of",
          "thousand city of oaks water department", "thousand oaks city of"
        ),
        "city of thousand oaks water department", !!new_col_sym
      ),
      
      # Port Hueneme
      !!new_col_sym := if_else(
        str_to_lower(!!new_col_sym) %in% c(
          "port hueneme city water dept", "port city of hueneme water department", "city of port hueneme water department"
        ),
        "city of port hueneme water department", !!new_col_sym
      ),
      
      # Chowchilla
      !!new_col_sym := if_else(
        str_to_lower(!!new_col_sym) %in% c(
          "city of chowchilla", "chowcilla, city of", "chowchilla, city of water department"
        ),
        "city of chowchilla water department", !!new_col_sym
      ),
      
      # Other one-off fixes
      !!new_col_sym := str_replace_all(!!new_col_sym, "folsom, city of", "city of folsom"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "great oaks water companyin|great oaks wc inc", "great oaks water company inc"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "bell, bell gardens|bell-bell gardens", "bell/bell gardens"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "coachella vwater|coachella vwd", "coachella valley water"),
      
      # Misspellings and cleanup
      !!new_col_sym := str_replace_all(!!new_col_sym, "maintence", "maintenance"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "san dima", "san dimas"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "companypany", "company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "water company\\s+water company", "water company"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bWirrigation Districtood\\b", "wildwood"),
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bWirrigation Districtor\\b", "windsor"),
      
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                          Step 8: Final cleanup
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\s*-\\s*", " - "),       # normalize hyphen spacing
      !!new_col_sym := str_replace_all(!!new_col_sym, "\\bkoa\\b", "KOA"),       # preserve acronym
      !!new_col_sym := str_replace_all(!!new_col_sym, "cal - am", "cal am"),     # spacing fix
      !!new_col_sym := str_squish(!!new_col_sym),                                # final whitespace cleanup
      !!new_col_sym := str_to_title(!!new_col_sym)                               # title case for readability
    )
}
# END of clean_supplier_name()
