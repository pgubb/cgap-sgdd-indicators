library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(kableExtra)
library(janitor)

# Helpers 

fixNA <- function(x) { 
  ifelse(x == "-", NA, x)
}

isnotNA <- function(x) { 
  ifelse(is.na(x), 0, 1)
}

noStar <- function(x) { 
  ifelse(str_detect(x, "\\*"), 0, 1)
}

notNA_noStar <- function(x) { 
  ifelse(isnotNA(x) & noStar(x), 1, 0)
  }

add_dash <- function(vec) {
  # vector of “trigger” phrases
  targets <- c(
    "Definitions and concepts:",
    "Data requirements:",
    "Limitations and considerations:",
    "Derivable indicators:"
  )
  
  # build a single regex that matches any target NOT already preceded by “- ”
  pattern <- paste0("(?<!-)\\b(", paste(targets, collapse = "|"), ")")
  
  # insert “- ” in front of each match
  stringr::str_replace_all(vec, pattern, "!- \\1")
}

  
# Globals
MND_OBJ <- 
  list(
    "Central banking" = c("Currency management & cash handling"), 
    "Statistics & research" = c("Statistics & research"), 
    "Macroprudential supervision" = c("Stability"), 
    "Microprudential supervision" = c("Credit risk", "Market risk", "Operational risk", "Liquidity risk", "Solvency", "Soundness", "Reputational and legal risk", "AML/CFT"), 
    "Competition" = c("Competition"), 
    "Capital markets development" = c("Capital markets development"), 
    "Sustainability" = c("Climate and environmental objectives", "Gender equality"), 
    "Consumer protection" = c("Data privacy and protection", "Fair treatment", "Complaints handling", "Safety and security", "Suitability", "Impact", "Transparency"), 
    "Financial inclusion" = c("Access", "Uptake (account ownership)", "Usage", "Quality", "Outcomes"), 
    "Financial safety net" = c("Depositor protection")
  )


COST_CODES <- 
  c(
    "L" = "I can have it with low effort/cost", 
    "M" = " I can collect the data but entails a relevant effort (time, budget, coordination)", 
    "H" = "I could collect the data but entails a high effort/cost (budget, regulatory, capacity, internal support, coordination, time, etc)", 
    "NC" = "I already have it", 
    "NF" = "I cannot collect this data and at this stage"
  )

# Data pipeline 

#filename <- "data/RGDD indicators_051225.xlsx"

filename <- "data/RGDD indicators_051225.csv"

# STEP 1: Cleaning and columns

#data <- read_excel(filename, sheet = "4. Indicators full list") %>% 

data <- read_csv(filename) %>% 
  clean_names() %>% 
  rename(indicator_name = indicator, 
         indicator_description = description, 
         indicator_long_description = long_description, 
         gender_questions = exploratory_questions_for_gender_analysis, 
         mandate_objective = mandates_objectives_indicators_main_mandate
         ) %>% 
  mutate(
    indicator_id = as.character(indicator_id), 
    unit_of_analysis = str_to_title(unit_of_analysis), 
    unit_of_analysis = ifelse(unit_of_analysis == "Msme", "MSME", unit_of_analysis),
    unit_of_analysis = ifelse(unit_of_analysis == "Individual And Msme", "Individual & MSME", unit_of_analysis),
    unit_of_analysis = ifelse(unit_of_analysis == "N/A", "Not applicable", unit_of_analysis),
    measurement_type = str_to_title(measurement_type), 
    measurement_type = ifelse(measurement_type == "Volume And Value", "Volume & Value", measurement_type), 
    high_priority = ifelse(high_priority == "yes", "High priority", "Other"), 
    high_priority = ifelse(is.na(high_priority), "Other", high_priority), 
    indicator_long_description = add_dash(indicator_long_description)
  )

# Checking for duplicated indicators
data %>% get_dupes(indicator_id)

# Core columns ------
core_columns <- data %>% 
  select(indicator_id, indicator_name, indicator_description, indicator_long_description, unit_of_analysis, measurement_type, gender_questions, high_priority, formula1_volume, formula2_value, formula_3_other) %>% 
  mutate(
    formula1_volume = ifelse(formula1_volume %in% c("No formula", "No formular", "NA") | is.na(formula1_volume), "Not applicable", formula1_volume), 
    formula2_value = ifelse(formula2_value %in% c("No formula", "No formular", "NA") | is.na(formula2_value), "Not applicable", formula2_value), 
  )

# Cleaning mandates-objectives -------

mandates <- data %>% 
  select(indicator_id, mandate_objective, starts_with(c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10"))) %>% 
  separate_wider_delim(mandate_objective, delim = "\r\n", names_sep = "_", too_few = "align_start") %>% 
  mutate_if(is.character, fixNA) %>% 
  mutate(
    main_mandate = case_when(
      str_detect(m1_financial_inclusion, "\\*") == TRUE ~ names(MND_OBJ[9]), 
      str_detect(m2_statistics_reasearch, "\\*") == TRUE ~ names(MND_OBJ[2]), 
      str_detect(m3_financial_consumer_protection, "\\*") == TRUE ~ names(MND_OBJ[8]), 
      str_detect(m4_microprudential_supervision, "\\*") == TRUE ~ names(MND_OBJ[4]), 
      str_detect(m5_financial_safety_net, "\\*") == TRUE ~ names(MND_OBJ[10]), 
      str_detect(m6_central_banking, "\\*") == TRUE ~ names(MND_OBJ[1]), 
      str_detect(m7_macroprudential_supervision, "\\*") == TRUE ~ names(MND_OBJ[3]), 
      str_detect(m8_sustainability, "\\*") == TRUE ~ names(MND_OBJ[7]), 
      str_detect(m9_capital_markets_development, "\\*") == TRUE ~ names(MND_OBJ[6]), 
      str_detect(m10_competition, "\\*") == TRUE ~ names(MND_OBJ[5])
    ), 
    main_objectives = case_when(
      str_detect(m1_financial_inclusion, "\\*") == TRUE ~ m1_financial_inclusion, 
      str_detect(m2_statistics_reasearch, "\\*") == TRUE ~ m2_statistics_reasearch, 
      str_detect(m3_financial_consumer_protection, "\\*") == TRUE ~ m3_financial_consumer_protection, 
      str_detect(m4_microprudential_supervision, "\\*") == TRUE ~ m4_microprudential_supervision, 
      str_detect(m5_financial_safety_net, "\\*") == TRUE ~ m5_financial_safety_net, 
      str_detect(m6_central_banking, "\\*") == TRUE ~ m6_central_banking, 
      str_detect(m7_macroprudential_supervision, "\\*") == TRUE ~ m7_macroprudential_supervision, 
      str_detect(m8_sustainability, "\\*") == TRUE ~ m8_sustainability, 
      str_detect(m9_capital_markets_development, "\\*") == TRUE ~ m9_capital_markets_development, 
      str_detect(m10_competition, "\\*") == TRUE ~ m10_competition
    ), 
    main_objectives = ifelse(main_objectives == "Access Usage", "Access, usage", main_objectives),
    across(starts_with(c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10")), notNA_noStar, .names = "sm_{.col}"), 
  ) %>% 
  mutate(
    sm_m1_financial_inclusion = ifelse(sm_m1_financial_inclusion == 1,names(MND_OBJ[9]), NA), 
    sm_m2_statistics_reasearch = ifelse(sm_m2_statistics_reasearch == 1, names(MND_OBJ[2]), NA), 
    sm_m3_financial_consumer_protection = ifelse(sm_m3_financial_consumer_protection == 1, names(MND_OBJ[8]), NA), 
    sm_m4_microprudential_supervision = ifelse(sm_m4_microprudential_supervision == 1, names(MND_OBJ[4]), NA),
    sm_m5_financial_safety_net = ifelse(sm_m5_financial_safety_net == 1, names(MND_OBJ[10]), NA), 
    sm_m6_central_banking = ifelse(sm_m6_central_banking == 1, names(MND_OBJ[1]), NA),  
    sm_m7_macroprudential_supervision = ifelse(sm_m7_macroprudential_supervision ==1, names(MND_OBJ[3]), NA), 
    sm_m8_sustainability = ifelse(sm_m8_sustainability == 1, names(MND_OBJ[7]), NA), 
    sm_m9_capital_markets_development = ifelse(sm_m9_capital_markets_development == 1, names(MND_OBJ[6]), NA), 
    sm_m10_competition = ifelse(sm_m10_competition == 1, names(MND_OBJ[5]), NA)
  ) %>% 
  unite("secondary_mandates", starts_with("sm_"), na.rm = TRUE, sep = ", ") %>% 
  separate_wider_delim(main_objectives, delim = ",", names_sep = "_", too_few = "align_start") %>% 
  mutate(
    main_mandate = ifelse(is.na(main_mandate), "Not assigned", main_mandate), 
    main_objectives_1 = str_trim(str_replace(main_objectives_1, "\\*", "")), 
    main_objectives_2 = str_trim(str_to_title(main_objectives_2)), 
    main_objectives_3 = str_trim(str_to_title(main_objectives_3))
  ) %>% 
  unite("main_objectives", starts_with("main_objectives"), na.rm = TRUE, sep = ", ") %>% 
  mutate(main_objectives = ifelse(main_objectives == "Currency management cash handling", "Currency management & cash handling", main_objectives)) %>% 
  select(indicator_id, main_mandate, secondary_mandates, main_objectives)

# Cleaning sectors ------

sectors <- data %>% 
  select(indicator_id, sectors) %>% 
  separate_wider_delim(sectors, delim = " ", names_sep = "_", too_few = "align_start") %>% 
  mutate(across(starts_with(c("sectors_")), isnotNA, .names = "bin_{.col}")) %>% 
  mutate(N = bin_sectors_1 + bin_sectors_2 + bin_sectors_3 + bin_sectors_4 + bin_sectors_5 + bin_sectors_6 + bin_sectors_7) %>% 
  mutate(main_sector = ifelse(N == 1, sectors_1, "Several/Others")) %>% 
  unite("all_sectors", starts_with("sectors_"), na.rm = TRUE, sep = ", ") %>% 
  mutate(
    secondary_sectors = str_replace(all_sectors, main_sector, "")
  ) %>% 
  select(indicator_id, main_sector, secondary_sectors)

# Cleaning use cases 

use_cases <- data %>% select(indicator_id, use_cases) %>% 
  separate_wider_delim(use_cases, delim = "\n", names_sep = "_", too_few = "align_start") %>% 
  unite("use_cases", starts_with("use_cases_"), na.rm = TRUE, sep = ", ") 

# Gender prioritization and costs 
# 
# gender_priority <- read_excel(filename, sheet = "0. Indicators Master Data", skip = 2) %>% 
#   select(`Indicator ID`, `Indicator's prioritization by gender relevance`, `Indicator's RegSup expected costs`, `Indicator's prioritization adjusted by collection feasibility`) %>% 
#   clean_names() %>% 
#   rename(
#     gender_priority = indicators_prioritization_by_gender_relevance, 
#     costs_regsup = indicators_reg_sup_expected_costs, 
#     gender_priority_adjusted = indicators_prioritization_adjusted_by_collection_feasibility
#   ) %>% 
#   mutate(
#     indicator_id = as.character(indicator_id),
#   gender_priority = case_when(
#     gender_priority %in% c("1-Must have", "1-must have") ~ "Must have", 
#     gender_priority %in% c("2-Good to have", "2-good to have") ~ "Good to have", 
#     gender_priority %in% c("3-aspirational") ~ "Aspirational"
#   ), 
#   gender_priority = ifelse(is.na(gender_priority), "(Not assigned)", gender_priority), 
#   gender_priority_top = ifelse(gender_priority == "Must have", "High priority", "Other"), 
#   gender_priority_adjusted = case_when(
#     gender_priority_adjusted %in% c("1-must have") ~ "Must have", 
#     gender_priority_adjusted %in% c("2-good to have") ~ "Good to have", 
#     gender_priority_adjusted %in% c("3-aspirational") ~ "Aspirational", 
#     gender_priority_adjusted %in% c("4-not a priority") ~ "Not a priority", 
#     gender_priority_adjusted %in% c("6-not Feasible", "6-not feasible" ) ~ "Not feasible"
#   ), 
#   gender_priority_adjusted = ifelse(is.na(gender_priority_adjusted), "(Not assigned)", gender_priority_adjusted),   
#   costs_regsup = ifelse(is.na(costs_regsup), "(Not assigned)", costs_regsup),
#  )

# External sources

references <- data %>% 
  rename(
    in_imf = imf_fas_core, 
    in_gpfi = gpfi, 
    in_afi = afi,
    in_wef = we_f
  ) %>% 
  mutate(
    indicator_id = as.character(indicator_id),
    in_imf = ifelse(is.na(in_imf), 0, in_imf), 
    in_gpfi = ifelse(is.na(in_gpfi), 0, in_gpfi), 
    in_afi = ifelse(is.na(in_afi), 0, in_afi), 
    in_wef = ifelse(is.na(in_wef), 0, in_wef), 
    references = ifelse(references == " ", NA, references)
  ) %>% 
  filter(!is.na(indicator_id)) %>% 
  select(indicator_id, in_imf, in_gpfi, in_afi, in_wef, references)

# Breakdowns

breakdowns <- data %>% 
  select(indicator_id, starts_with("d")) 

# 1.  map column names → human-readable labels ------------
segment_map <- c(
  d1_customer_gender              = "Customer gender",
  d2_provider_gender              = "Provider gender",
  d3_customer_type                = "Type of customer",
  d4_customer_age                 = "Age of customer",
  d5_fsp_type                     = "Financial service provider type",
  d6_product_type                 = "Product type",
  d7_channel_type                 = "Channel type",
  d8_transaction_type             = "Transaction type",
  d9_customer_location            = "Customer location",
  d10_average_size_bands          = "Average size category",
  d11_average_term_bands          = "Average term category",
  d12_average_interest_rate_bands = "Average interest rate category",
  d13_customer_income             = "Customer income category"
)

# 2.  reshape + collapse  ---------------------------------
breakdowns <- breakdowns %>%                                   # your data
  pivot_longer(starts_with("d"),                   # all “d#_” columns
               names_to  = "segment",
               values_to = "flag") %>%             # 0 / 1 / 2
  filter(flag %in% 1:2) %>%                        # keep 1 = essential, 2 = non-essential
  mutate(segment = segment_map[segment],           # replace with label
         class   = if_else(flag == 1,
                           "essential_disagg",
                           "nonessential_disagg")) %>% 
  select(indicator_id, class, segment) %>%
  group_by(indicator_id, class) %>%                # collect per indicator & class
  summarise(segment = paste(segment, collapse = "; "),
            .groups = "drop") %>%                 
  pivot_wider(names_from  = class,                 # → wide format
              values_from = segment,
              values_fill = "")                    # empty string if none



# Combining all components together
indicators <- core_columns %>% 
                left_join(mandates, by = "indicator_id") %>% 
                left_join(sectors, by = "indicator_id") %>% 
                left_join(use_cases, by = "indicator_id") %>% 
                #left_join(gender_priority, by = "indicator_id") %>% 
                left_join(references, by = "indicator_id") %>% 
                left_join(breakdowns, by = "indicator_id") 
  
# Saving file 

filename <- "data/RGDD indicators_051225.xlsx"

usecases <-  read_excel(filename, sheet = "Use cases", skip = 1) %>% 
  clean_names() %>% 
  fill(overarching_vision_for_the_fs, gender_related_challenge_opportunity, how_sgdd_can_help_market_actors_tackle_challenge_opportunity)

names(usecases) <- c("vision", "opportunity", "rgdd_role", "use_case", "indicators_explanation", "type_of_analysis", "indicator_mandates")

# Saving file 

save(indicators, usecases, file = "data/indicators.RData")


  
