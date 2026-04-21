# ============================================================================
# LENS Catalog — Data Preparation Script
# ============================================================================
# Run this script locally to pull the latest indicators from Google Sheets
# and save as indicators.RData for the catalog app.
#
# Usage:
#   source("data_prep.R")
#
# Prerequisites:
#   - .Renviron file in this directory with:
#       LENS_SHEET_ID=your-google-sheet-id
#       LENS_SHEET_NAME=indicators
#       GOOGLE_SERVICE_ACCOUNT_KEY=/path/to/service-account.json
#   - googlesheets4 and googledrive packages installed
#
# After running:
#   - data/indicators.RData is updated
#   - Commit and push to Github
#   - App will use the fresh data on next deployment
# ============================================================================
library(readxl)
library(writexl)
library(googlesheets4)
library(googledrive)


cat("=== LENS Data Prep ===\n\n")

# Load the connector
source("R/data_connector.R")

# Pull fresh data from Google Sheets
indicators <- load_indicators(force_refresh = TRUE) %>% 
  mutate(main_mandate = factor(main_mandate, levels = c("Financial inclusion", "Consumer protection", "Stability, safety and soundness", "Sustainability", "Market development"), ordered = TRUE)) %>% 
  # Dropping select indicators from catalog
  filter(indicator_name %not_in% c("Payroll loans", "New licenses granted to diverse FSPs"))

# Quick summary
cat("\n--- Summary ---\n")
cat("  Rows:    ", nrow(indicators), "\n")
cat("  Columns: ", ncol(indicators), "\n")
cat("  Mandates:", paste(unique(indicators$main_mandate), collapse = ", "), "\n")
cat("  Sectors: ", paste(unique(indicators$main_sector), collapse = ", "), "\n")

# Save
save(indicators, file = "data/indicators.RData")
cat("\n  Saved to data/indicators.RData\n")
cat("  Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("\n=== Done. Commit and push to deploy. ===\n")

# Save public version
write_xlsx(
  list(
    "Indicators" = indicators %>% arrange(main_mandate, indicator_order) %>% 
      select(main_mandate, main_objectives, main_sector, indicator_id, indicator_name, 
             indicator_description, indicator_long_description, gender_questions, 
             unit_of_analysis, secondary_mandates, secondary_objectives, disaggregation_vars)
  ),
  "www/LENS_INDICATORS_DB.xlsx"
)

