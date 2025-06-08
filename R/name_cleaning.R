clean_supplier_name <- function(data, col_name) {
  col_sym <- rlang::sym(col_name)
  
  data %>%
    # Step 1: Copy original column to 'supplier_name'
    mutate(supplier_name = !!col_sym) %>%
    
    # Step 2: Standard whitespace and punctuation cleanup
    mutate(
      supplier_name = str_squish(supplier_name),
      supplier_name = str_replace_all(supplier_name, "(?i)\\s+", " "),
      supplier_name = str_trim(supplier_name),
      supplier_name = str_replace_all(supplier_name, "([a-zA-Z])[-/]([a-zA-Z])", "\\1 - \\2"),
      supplier_name = str_replace_all(supplier_name, regex("\\(ventura\\)", ignore_case = TRUE), ""),
      supplier_name = str_replace_all(supplier_name, "[()]", ""),
      supplier_name = str_replace_all(supplier_name, "ter Agencyer", "")
    ) %>%
    
    # Step 3: Normalize corporate formats
    mutate(
      supplier_name = str_replace(supplier_name,
                                  "(?i)^(cal am water company|golden state water company)\\s+(\\S.*)$",
                                  "\\1 - \\2"),
      supplier_name = str_replace_all(supplier_name, "(?i)mutual water co\\.?$", "mutual water company")
    ) %>%
    
    # Step 4: Normalize suffixes like 'City of' and 'Town of' to prefixes
    mutate(
      supplier_name = str_replace_all(supplier_name, regex("^(.+?)[,\\s-]*city of$", ignore_case = TRUE), "City Of \\1"),
      supplier_name = str_replace_all(supplier_name, regex("^(.+?)[,\\s-]*town of$", ignore_case = TRUE), "Town Of \\1"),
      supplier_name = str_replace_all(supplier_name, regex("^(.+?)[,\\s-]*city of[,\\s-]*(.*)$", ignore_case = TRUE), "City Of \\1 \\2"),
      supplier_name = str_replace_all(supplier_name, regex("^(.+?)[,\\s-]*town of[,\\s-]*(.*)$", ignore_case = TRUE), "Town Of \\1 \\2"),
      supplier_name = str_replace_all(supplier_name, regex("(.*)[,\\s-]+(city|town) of[,\\s-]*(.*)", ignore_case = TRUE), "\\2 Of \\1 \\3"),
      supplier_name = str_replace_all(supplier_name, regex("^(.+?)[\\s,-]+city\\b", ignore_case = TRUE), "City Of \\1")
    ) %>%
    
    # Step 5: Department and agency cleanup
    mutate(
      supplier_name = str_replace_all(supplier_name, "(\\b[a-z]+) city water dept\\.?$", "city of \\1 water department"),
      supplier_name = str_replace_all(supplier_name, "(\\b[a-z]+) water dept\\.?$", "city of \\1 water department"),
      supplier_name = str_replace_all(supplier_name, "dept\\.?\\b", "department")
    ) %>%
    
    # Step 6: Abbreviation expansions
    mutate(
      supplier_name = str_replace_all(supplier_name, "pw", "public works"),
      supplier_name = str_replace_all(supplier_name, "\\bpud\\b", "public utility district"),
      supplier_name = str_replace_all(supplier_name, "\\bslvwd\\b", "san lorenzo valley water district"),
      supplier_name = str_replace_all(supplier_name, "\\bmwc\\b", "mutual water company"),
      supplier_name = str_replace_all(supplier_name, "\\bscwa\\b", "sacramento county water agency"),
      supplier_name = str_replace_all(supplier_name, "\\bwc\\b", "water company"),
      supplier_name = str_replace_all(supplier_name, regex("\\b(i\\.d\\.?|id)\\b", ignore_case = TRUE), "irrigation district"),
      supplier_name = str_replace_all(supplier_name, regex("\\bw[\\.-]?d\\b", ignore_case = TRUE), "water district"),
      supplier_name = str_replace_all(supplier_name, regex("\\bw[\\.-]?a\\b", ignore_case = TRUE), "water agency"),
      supplier_name = str_replace_all(supplier_name, "\\bu\\.d\\b", "utility district"),
      supplier_name = str_replace_all(supplier_name, "\\bc\\.s\\.d\\b", "community services district"),
      supplier_name = str_replace_all(supplier_name, "\\btud\\b", "tuolumne utilities district"),
      supplier_name = str_replace_all(supplier_name, "\\bcsd\\b", "community services district"),
      supplier_name = str_replace_all(supplier_name, "\\bcwd\\b", "community water district"),
      supplier_name = str_replace_all(supplier_name, "\\bsd\\b", "services district"),
      supplier_name = str_replace_all(supplier_name, "\\bdist\\b", "district"),
      supplier_name = str_replace_all(supplier_name, "\\bcws\\b", "california water service"),
      supplier_name = str_replace_all(supplier_name, "\\bgolden state (wc|water co\\.?|water company)\\b", "golden state water company"),
      supplier_name = str_replace_all(supplier_name, "\\bcal[- ]?am(\\b|erican water.*)", "cal am water company")
    ) %>%
    
    # Step 6.5: Collapse repeated prefixes
    mutate(
      supplier_name = str_replace_all(supplier_name, 
                                      regex("\\b(City Of|Town Of)\\b(?:\\s+\\1\\b)+", ignore_case = TRUE), 
                                      "\\1")
    ) %>%
    
    # Step 7: Final formatting
    mutate(
      supplier_name = str_replace_all(supplier_name, "\\s*-\\s*", " - "),
      supplier_name = str_squish(supplier_name),
      supplier_name = str_to_title(supplier_name),
      supplier_name = str_replace_all(supplier_name, "\\bKoa\\b|koa", "KOA")
    ) |> 
  
    # Step 8: Optionally overwrite original column with cleaned names
    mutate(!!col_sym := supplier_name)
}
