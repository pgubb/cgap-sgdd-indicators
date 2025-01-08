
cost_codes <- 
  c(
    "L" = "I can have it with low effort/cost", 
    "M" = " I can collect the data but entails a relevant effort (time, budget, coordination)", 
    "H" = "I could collect the data but entails a high effort/cost (budget, regulatory, capacity, internal support, coordination, time, etc)", 
    "NC" = "I already have it", 
    "NF" = "I cannot collect this data and at this stage"
  )

indicators <- read_excel("data/INDICATORS_DB.xlsx", sheet = "5. Indicators") %>% 
  filter(!is.na(indicator_measurement)) %>% # Removing two indicators that are suggestions 
  mutate(unit_of_analysis = str_to_sentence(unit_of_analysis), 
         measurement_type = str_to_sentence(measurement_type),
         mandate_main = str_to_sentence(mandate_main), 
         objective_main = str_to_sentence(objective_main), 
         priority_gender = str_to_title(priority_gender),
         priority_feasibility = str_to_title(priority_feasibility),
         costs_regsup = cost_codes[costs_regsup], 
         costs_fsp = cost_codes[costs_fsp], 
         breakdowns = str_replace_all(breakdowns, "\n", ", "),
         breakdowns = str_replace_all(breakdowns, "\r", ""),
         breakdowns = str_to_title(breakdowns), 
         breakdowns = str_replace(breakdowns, "Fsp", "FSP"), 
         uc_sgdd_indicators_mandate_obj = paste(mandate_main, objective_main, sep = ": ")) %>% 
  separate_wider_delim(uc_sgdd_indicators_mandate_obj, ": ", names_sep = "", too_few = "align_start") %>% 
  mutate(uc_sgdd_indicators_mandate_obj = paste(uc_sgdd_indicators_mandate_obj1, uc_sgdd_indicators_mandate_obj2, sep = ": ")) %>% 
  select(-uc_sgdd_indicators_mandate_obj1, -uc_sgdd_indicators_mandate_obj2, -uc_sgdd_indicators_mandate_obj3) %>% 
  mutate(sectors = str_to_title(sectors))

length(unique(indicators$indicator_name))
# There are 212 unique indicators in the databse

usecases <-  read_excel("data/INDICATORS_DB.xlsx", sheet = "6. Use cases") %>% 
  separate_wider_delim(uc_sgdd_indicators_mandate_obj, "; ", names_sep = "", too_few = "align_start", cols_remove = FALSE) %>% 
  rename(uc_sgdd_indicators_mandate_obj = uc_sgdd_indicators_mandate_objuc_sgdd_indicators_mandate_obj)

challenges_codebook <-read_excel("data/INDICATORS_DB.xlsx", sheet = "challenges_codebook")

coldescs <- read_excel("data/INDICATORS_DB.xlsx", sheet = "col_codebook")
col_codebook <- coldescs[["coldescription"]]
names(col_codebook) <- coldescs[["colname"]]
