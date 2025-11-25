
# This file prepares edits and modification made by TAG on October 12, these were changes not made in the master file 


indicators_update <- read_csv("data/LENS_indicators_100925_TAG.csv") %>% 
  filter(is.na(DROP)) %>% select(-DROP) %>% 
  mutate(indicator_id = as.character(indicator_id))

# Checking for duplicated indicators
indicators_update %>% get_dupes(indicator_id)
                     
#Fixing main_mandate_umbrella column
indicators_update %>% 
  mutate(
    main_mandate_umbrella = ifelse(main_mandate_umbrella == "Financial Inclusion", "Financial inclusion", main_mandate_umbrella)
  ) -> indicators_update

#Fixing main sector column
indicators_update %>% 
  mutate(
    main_sector = ifelse(main_sector == "Several/Other", "Several/other", main_sector)
  ) -> indicators_update

# Fixing secondary sector columns 
indicators_update %>% 
  mutate(
    secondary_mandates = str_replace(secondary_mandates, "Financial Inclusion", "Financial inclusion")
  ) -> indicators_update

# Fixing secondary objectives columns 
indicators_update %>% 
  mutate(
    secondary_mandates = str_replace(secondary_mandates, "Financial Inclusion", "Financial inclusion"), 
    secondary_mandates = str_replace(secondary_mandates, "Consumer Protection", "Consumer protection")
  ) -> indicators_update


# Creating 

remove_trailing_comma <- function(x) {
  sub(",\\s*$", "", x)
}

indicators_update %>% 
  mutate(secondary_mandates_umbrella = map_chr(secondary_mandates, reclassify_mandates), 
         secondary_mandates_umbrella = ifelse(secondary_mandates_umbrella == "NA", NA, secondary_mandates_umbrella), 
         secondary_mandates_umbrella = remove_trailing_comma(secondary_mandates_umbrella), 
         secondary_mandates_umbrella = ifelse(secondary_mandates_umbrella == "All", "Financial inclusion, Consumer protection, Prudential supervision, Market development", secondary_mandates_umbrella)) -> indicators_update

# Check the unique values after cleaning
unique(indicators_update$secondary_mandates_umbrella)

# Stitching together mandates and objectives: 

indicators_update %>% 
  mutate(
    main_mandate_objective = map2_chr(main_mandate_umbrella, main_objectives, combine_cols),
    secondary_mandate_objective = map2_chr(secondary_mandates_umbrella, secondary_objectives, combine_cols), 
    secondary_mandate_objective = ifelse(secondary_mandate_objective == "NA (NA)", NA, secondary_mandate_objective), 
    secondary_mandate_objective = ifelse(secondary_mandate_objective == "Financial inclusion (NA)", NA, secondary_mandate_objective)
  ) -> indicators_update



# Harmonizing disaggregation variables 

disagg_recode_map = c(
"FSP type of entity" = "Financial service provider (FSP) type", 
"Type of customer" = "Customer type", 
"Age of customer" = "Age of customer", 
"type of insurance" = "Insurance type",
"Customer income" ="Customer income category",
"Type of credit" = "Credit type",
"Type of access points" = "Access point type",
"Type of pension" = "Pension type",
"Type of loan" = "Loan type",
"Type of product" = "Product type", 
"interest rate" = "Interest rate category",
"Interest rate" = "Interest rate category",
"Type of deposit account" = "Deposit account type",
"collateral type" = "Collateral type", 
"FSP main activity" = "Financial service provider (FSP) main activity", 
"Average size category" = "Loan size",                              
"Average term category" = "Loan term"   
)

# Function to reclassify one cell
reclassify_disagg <- function(x) {
  x %>%
    str_split(";\\s*") %>%           # split on commas
    unlist() %>%
    str_trim() %>%
    map_chr(~ ifelse(.x %in% names(disagg_recode_map),
                     disagg_recode_map[[.x]],  # replace if in map
                     .x)) %>%
    unique() %>%
    paste(collapse = "; ")
}

indicators_update %>%
  mutate(essential_disagg = map_chr(essential_disagg, reclassify_disagg)) -> indicators_update

# Checking the disaggrgation variables

unique_disagg_vars <- indicators_update %>%
  pull(essential_disagg) %>%
  str_split(";\\s*") %>%     # split on ';' followed by optional space
  unlist() %>%
  str_trim() %>%
  unique() %>%
  sort()


indicators_update %>% 
  select(-secondary_mandates) %>% 
  rename(main_mandate = main_mandate_umbrella, secondary_mandates = secondary_mandates_umbrella, disaggregation_vars = essential_disagg) %>% 
  select(indicator_id, indicator_order, main_mandate, main_objectives, main_mandate_objective, secondary_mandates, secondary_objectives, secondary_mandate_objective, main_sector,
         indicator_name, indicator_description, indicator_long_description, gender_questions, unit_of_analysis, disaggregation_vars, preset_foundation, preset_digital) -> indicators_update


# Selecting columns from older version to merge in

tomerge <- indicators_base %>% select(indicator_id, secondary_sectors)

indicators_update %>% left_join(tomerge, by = "indicator_id") %>% left_join(sources, by = "indicator_id") -> indicators

#Fixing main sector column
indicators %>% 
  mutate(
    # Replacing "Main sector" if "Several/Other" with all applicable sectors 
    main_sector = ifelse(main_sector == "Several/other", secondary_sectors, main_sector), 
    main_sector = ifelse(indicator_name %in% c("Financial transactions by channel (volume)", "Financial transactions by channel (value)"), "Payments", main_sector), 
    main_sector = str_replace(main_sector, ", Other", "")
  ) %>% 
  select(-secondary_sectors) -> indicators

# Ordering the main_mandates column: 

indicators <- indicators %>% 
  mutate(
    main_mandate_order = case_when(
      main_mandate == "Financial inclusion" ~ 1, 
      main_mandate == "Consumer protection" ~ 2, 
      main_mandate == "Market development" ~ 3, 
      main_mandate == "Sustainability" ~ 4, 
      main_mandate == "Prudential supervision" ~ 5, 
      main_mandate == "Other" ~ 6
    ), 
    main_mandate = fct_reorder(main_mandate, main_mandate_order)
) 

# Saving file for use in the web application

save(indicators, file = "data/indicators.RData")

# Saving indicators for use for public downloads: 

write_xlsx(
  indicators %>% arrange(main_mandate, indicator_order) %>% 
    select(main_mandate, main_objectives, main_sector, indicator_id, indicator_name, indicator_description, indicator_long_description, gender_questions, unit_of_analysis, secondary_mandates, secondary_objectives, disaggregation_vars), 
    "www/LENS_INDICATORS_DB_v11062025.xlsx"
  )


