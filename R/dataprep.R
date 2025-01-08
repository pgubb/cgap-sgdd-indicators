
indicators <- read_excel("data/INDICATORS_DB.xlsx", sheet = "5. Indicators") %>% 
  filter(!is.na(indicator_measurement)) %>% # Removing two indicators that are suggestions 
  mutate(unit_of_analysis = str_to_sentence(unit_of_analysis), 
         measurement_type = str_to_sentence(measurement_type),
         mandate_main = str_to_sentence(mandate_main), 
         objective_main = str_to_sentence(objective_main), 
         priority_gender = str_to_title(priority_gender),
         priority_feasibility = str_to_title(priority_feasibility),
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