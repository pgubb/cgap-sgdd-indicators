# CGAP LENS

CGAP LENS is an interactive catalog of regulatory indicators designed to help financial sector authorities explore, filter, and build custom sets of indicators relevant to gender-disaggregated and sociodemographic analysis in financial regulation. It is part of CGAP's Supply-side Gender Disaggregated Data (S-GDD) project.

**Live app:** [datanalytics.worldbank.org/cgap-lens](https://datanalytics.worldbank.org/cgap-lens/)

## Getting Started

```r
# Restore dependencies
renv::restore()

# Run the app
shiny::runApp("app.R")
```

## Data Refresh

To update the indicator dataset from the upstream Google Sheet:

```r
source("data_prep.R")
```

This requires environment variables in `.Renviron` (see `CLAUDE.md` for details). After running, commit the updated `data/indicators.RData`.

## Architecture

Single-file R Shiny app (`app.R`) with modular components in `R/modules/`. Uses `bslib` for UI, `renv` for dependency management, and deploys to shinyapps.io. See `CLAUDE.md` for full architectural details.
