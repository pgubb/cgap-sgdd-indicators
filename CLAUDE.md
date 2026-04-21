# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

CGAP LENS is an R Shiny application that serves as an interactive catalog of ~220 regulatory indicators for financial sector authorities. It helps users explore, filter, and build custom sets of indicators relevant to sociodemographic (especially gender) disaggregated analysis in financial regulation. Part of CGAP's Supply-side Gender Disaggregated Data (S-GDD) project.

**Live app:** `datanalytics.worldbank.org/cgap-lens/` (Beta, final release targeted June 2026)

LENS is one component of a broader deliverable package that includes a Technical Guide for FSAs ("Using Disaggregated Regulatory Data to Improve Policy, Regulation and Supervision") and country case studies.

## Running the App

```r
# Restore dependencies (first time or after pulling)
renv::restore()

# Run the Shiny app
shiny::runApp("app.R")
```

The app uses `renv` for dependency management. The lockfile is `renv.lock`.

## Data Pipeline

The app loads indicator data from `data/indicators.RData`. To refresh this data from the upstream Google Sheet:

```r
source("data_prep.R")
```

This requires environment variables in `.Renviron`:
- `LENS_SHEET_ID` — Google Sheet ID
- `LENS_SHEET_NAME` — tab name (default: "indicators")
- `GOOGLE_SERVICE_ACCOUNT_KEY` — path to service account JSON file

After running `data_prep.R`, commit the updated `data/indicators.RData` and `www/LENS_INDICATORS_DB.xlsx` to deploy fresh data.

`R/data_connector.R` provides an alternative runtime connector with in-memory caching (5-min TTL), but the app currently uses the static `.RData` file.

## Architecture

**Single-file Shiny app** (`app.R`, ~350 lines) with manual module sourcing. Shiny autoload is disabled via `R/_disable_autoload.R` and `options(shiny.autoload.r = FALSE)`.

Source order in `app.R`:
1. `R/globals.R` — constants: `SECTOR_COLORS`, `MND_OBJ_2` (mandate-objective hierarchy), `COLUMN_DESCRIPTIONS`, `BREAKDOWNS`
2. `R/utils.R` — helper functions: UI builders (`enhanced_navigation_helper`, `enhanced_mandate_header`, `indicator_key`, `about_modal_content`), table renderers, text processing, PDF report generation
3. `R/modules/indicatorCard.R` — renders individual indicator cards + all client-side JavaScript (selection, expand/collapse, scroll navigation)
4. `R/modules/filterPanel.R` — sidebar filter controls with pre-computed lookup tables for performance
5. `R/modules/setManager.R` — CRUD for multiple named indicator sets, with bulk add/remove operations
6. `R/modules/selectedIndicatorsMulti.R` — the "Your Indicator Sets" tab, uses setManager internally

### Key Data Flow

Filters (filterPanel) → debounced reactive (200ms) → dynamic UI insertion into `#indicator_container` grouped by mandate → user selects indicators via JS events (`indicator_selected`) → `set_manager` tracks selections across named sets → export via CSV or HTML report.

### Performance Notes

- Filter lookups (mandates, objectives, sectors, search text) are pre-computed at module init, not on every reactive tick
- `add_many_to_active()` / `remove_many_from_active()` perform bulk set operations in a single reactive write
- Search uses SnowballC stemming with dual-match (original word + stemmed)

### UI Framework

`bslib` (`page_navbar`, `sidebar`, `accordion`). Custom CSS in `www/custom.css`. JavaScript embedded in `indicatorCardJS()`.

### Design System

- **Font:** Figtree (Google Fonts) — do not change
- **Brand colors:** Purple gradient `#6F5B9D` → `#402C60` (Browse header), Brand blue `#1A5A80` (navbar, Indicator Sets header, accents) — do not change
- **Sector colors:** Defined in `SECTOR_COLORS` in `globals.R` — each financial service has a distinct color
- **Card design:** Border-driven depth (no box-shadows), flat buttons, 8px border-radius on cards, 4px on badges
- **Hover states:** Border-color and background-color transitions only — no translateY lifts or growing shadows
- **Page background:** `#fafaf8` (subtle warm tint) so white cards separate naturally

## Deployment

Deployed to shinyapps.io (account: `pgubbins`). Deployment config in `rsconnect/`. The `NOT_PUBLIC/` folder contains internal documents and is excluded from deployment via `.gitignore`.

## Key Domain Concepts

- **Mandates**: Top-level regulatory categories — Financial inclusion, Consumer protection, Stability/safety/soundness, Sustainability, Market development (5 mandates, ordered)
- **Objectives**: Sub-categories within mandates, defined in `MND_OBJ_2` in `globals.R` (e.g., Access, Usage, Fair treatment, Credit risk)
- **Sectors/Services**: Financial service types — Payments, Credit, Insurance, Investments, Pensions, Savings, Several/other — each with a distinct color in `SECTOR_COLORS`
- **Breakdowns**: Suggested dimensions for disaggregated analysis (gender, age, location, FSP type, product type, etc.) — defined in `BREAKDOWNS` in `globals.R` with ~20 categories
- **Indicator sets**: Users can create multiple named collections of indicators, add notes, and export as CSV or HTML report
- **Presets**: The "Digital finance ecosystem" preset filters to indicators tagged `preset_digital == 1`. A "Foundational" preset exists in the data (`preset_foundation` column) but is currently disabled in the UI.
- **Long descriptions**: Use `!-` as a delimiter for sections (Definitions, Data requirements, Limitations, Derivable indicators)

## File Structure

```
app.R                           # Main app (UI + server, ~350 lines)
R/globals.R                     # Constants (colors, mandates, breakdowns)
R/utils.R                       # Helpers, UI builders, report generation
R/modules/indicatorCard.R       # Card rendering + all JS
R/modules/filterPanel.R         # Filter controls + pre-computed lookups
R/modules/setManager.R          # Named set management (CRUD + bulk ops)
R/modules/selectedIndicatorsMulti.R  # "Your Indicator Sets" tab
R/data_connector.R              # Google Sheets runtime connector (alternative)
R/_disable_autoload.R           # Prevents Shiny autoload
data_prep.R                     # Data refresh script
data/indicators.RData           # Pre-built indicator dataset
www/custom.css                  # All custom styling
www/CGAP Lens User Guide.pdf    # Downloadable user guide
www/LENS_INDICATORS_DB.xlsx     # Full catalog download
```
