# ============================================================================
# LENS Catalog — Google Sheets Data Connector
# ============================================================================
# Loads the indicator catalog at runtime from the Google Sheet maintained by
# the CGAP LENS admin app, falling back to the committed snapshot
# (data/indicators.RData) when the sheet cannot be reached.
#
# Usage in app.R (after R/globals.R and R/utils.R are sourced):
#   source("R/data_connector.R")
#   indicators <- lens_get_indicators()   # never errors; sheet or snapshot
#
# Environment variables:
#   LENS_SHEET_ID                - Google Sheet ID (required for live data)
#   LENS_SHEET_NAME              - Tab name (default: "indicators")
#   GOOGLE_SERVICE_ACCOUNT_KEY   - Path to JSON key file, or raw JSON string
#   LENS_DATA_SOURCE             - "sheet" (default) or "snapshot" to force
#                                  the committed snapshot and skip the API
#   LENS_CACHE_TTL_SECONDS       - In-memory cache lifetime (default: 300)
#
# Caching:
#   Data is cached in-memory with a configurable TTL (default: 5 min).
#   All sessions within the same R process share the cache. Once the TTL
#   expires, the next new session triggers a refresh from the sheet; if that
#   refresh fails, the stale cache is served rather than the snapshot.
#   Call load_indicators(force_refresh = TRUE) to bypass the cache.
# ============================================================================

library(googlesheets4)
library(googledrive)
library(dplyr)

# --- Configuration ---
.lens_config <- list(
  sheet_id    = Sys.getenv("LENS_SHEET_ID", ""),
  sheet_name  = Sys.getenv("LENS_SHEET_NAME", "indicators"),
  cache_ttl   = as.numeric(Sys.getenv("LENS_CACHE_TTL_SECONDS", "300")),  # 5 min
  data_source = tolower(Sys.getenv("LENS_DATA_SOURCE", "sheet")),          # or "snapshot"
  snapshot    = "data/indicators.RData"
)

# Indicators removed from the public catalog regardless of data source.
LENS_DROPPED_INDICATORS <- c("Payroll loans", "New licenses granted to diverse FSPs")

# --- In-memory cache ---
.lens_cache <- new.env(parent = emptyenv())
.lens_cache$data       <- NULL   # finalized tibble from the sheet
.lens_cache$timestamp  <- NULL   # when it was last read from the sheet
.lens_cache$snapshot   <- NULL   # finalized tibble from data/indicators.RData
.lens_cache$source     <- NULL   # "sheet" or "snapshot": what was last served

# --- Authentication ---
#' Authenticate with Google via service account.
#' Called once at startup; token is reused automatically.
.lens_authenticate <- function() {
  key <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY", "")
  
  if (!nzchar(key)) {
    if (!interactive()) {
      stop("[LENS] GOOGLE_SERVICE_ACCOUNT_KEY is not set.", call. = FALSE)
    }
    message("[LENS] No GOOGLE_SERVICE_ACCOUNT_KEY found. ",
            "Falling back to interactive auth.")
    googlesheets4::gs4_auth()
    return(invisible())
  }
  
  # File path or raw JSON?
  if (file.exists(key)) {
    path <- key
  } else {
    path <- tempfile(fileext = ".json")
    writeLines(key, path)
    on.exit(unlink(path), add = TRUE)
  }
  
  googledrive::drive_auth(path = path)
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  message("[LENS] Authenticated with service account.")
}

# --- Helpers ---

# Reverse lookup: objective name → mandate name (built once from MND_OBJ_2)
# Uses tolower keys for case-insensitive matching.
.obj_to_mandate <- {
  lkp <- character(0)
  for (mnd in names(MND_OBJ_2)) {
    for (obj in MND_OBJ_2[[mnd]]) {
      lkp[[tolower(obj)]] <- mnd
    }
  }
  lkp
}

#' Build "Mandate1 (Obj1, Obj2), Mandate2 (Obj3)" from delimited strings.
#' Uses the MND_OBJ_2 lookup to resolve which objectives belong to which
#' mandate — no assumptions about objective delimiters between groups.
#' Mandates are ";"-separated; objectives are always ","-separated.
.build_secondary_label <- function(mandates_str, objectives_str) {
  if (is.na(mandates_str) || !nzchar(mandates_str)) return(NA_character_)
  
  mandates <- trimws(strsplit(mandates_str, ";")[[1]])
  mandates_lc <- tolower(mandates)
  
  # Split ALL objectives by comma (flat list)
  objectives <- if (!is.na(objectives_str) && nzchar(objectives_str)) {
    trimws(strsplit(objectives_str, ",")[[1]])
  } else {
    character(0)
  }
  
  # Bucket each objective under its matching secondary mandate via lookup
  obj_by_mandate <- setNames(
    vector("list", length(mandates)),
    mandates_lc
  )
  
  for (obj in objectives) {
    parent <- .obj_to_mandate[tolower(obj)]
    if (!is.na(parent) && tolower(parent) %in% mandates_lc) {
      key <- tolower(parent)
      obj_by_mandate[[key]] <- c(obj_by_mandate[[key]], obj)
    }
  }
  
  # Assemble labels preserving original mandate casing and objective order
  labels <- vapply(seq_along(mandates), function(i) {
    objs <- obj_by_mandate[[mandates_lc[i]]]
    if (length(objs) > 0) {
      paste0(mandates[i], " (", paste(objs, collapse = ", "), ")")
    } else {
      mandates[i]
    }
  }, character(1))
  
  paste(labels, collapse = ", ")
}

# --- Core reader ---
#' Read raw data from Google Sheets and produce the indicators tibble.
#' Handles list columns, type coercion, empty strings, and computed columns.
.lens_read_sheet <- function() {
  if (!nzchar(.lens_config$sheet_id)) {
    stop("[LENS] LENS_SHEET_ID environment variable is not set.",
         call. = FALSE)
  }
  
  message("[LENS] Reading indicators from Google Sheets...")
  t0 <- Sys.time()
  
  raw <- googlesheets4::read_sheet(
    .lens_config$sheet_id,
    sheet = .lens_config$sheet_name
  )
  
  # 1. Flatten list columns (mixed types from Sheets API)
  raw <- raw %>%
    mutate(across(everything(), ~ {
      if (is.list(.)) {
        sapply(., function(x) {
          if (is.null(x) || length(x) == 0) NA_character_
          else as.character(x[[1]])
        })
      } else .
    }))
  
  # 1b. Normalize column names (Google Sheet may use different casing)
  if ("preset_MSME" %in% names(raw)) {
    raw <- raw %>% rename(preset_msme = preset_MSME)
  }

  # 2. Empty strings → NA
  raw <- raw %>%
    mutate(across(where(is.character), ~ ifelse(. == "", NA_character_, .)))
  
  # 3. Coerce numeric columns (safe: character → integer)
  int_cols <- c("indicator_id", "indicator_order", "main_mandate_order",
                "preset_digital", "preset_msme", "preset_finhealth", "preset_di", "preset_fraud",
                "sources_any", "FEMAMETER")
  for (col in intersect(int_cols, names(raw))) {
    raw[[col]] <- suppressWarnings(as.integer(as.character(raw[[col]])))
  }
  
  # 3b. Rename objective: "Gender equality" -> "Diversity and inclusion"
  #     Applied to the raw objective columns before derived labels are built,
  #     so it propagates to main_mandate_objective / secondary_mandate_objective
  #     and matches the MND_OBJ_2 hierarchy in globals.R.
  for (col in intersect(c("main_objectives", "secondary_objectives"), names(raw))) {
    raw[[col]] <- gsub("Gender equality", "Diversity and inclusion",
                       raw[[col]], fixed = TRUE)
  }

  # 4. Recompute derived columns (always fresh from source data)
  raw <- raw %>%
    mutate(
      # Main mandate + objective label
      main_mandate_objective = ifelse(
        !is.na(main_mandate) & !is.na(main_objectives),
        paste0(main_mandate, " (", main_objectives, ")"),
        NA_character_
      ),
      # Secondary mandate + objective label (positional pairing)
      secondary_mandate_objective = mapply(
        .build_secondary_label,
        secondary_mandates, secondary_objectives,
        USE.NAMES = FALSE
      ),
      # Has any international initiative source?
      sources_any = {
        has_source <- (!is.na(GPFI) | !is.na(IMF) | !is.na(AFI) | !is.na(WEF))
        # Include FEMAMETER if column exists
        if ("FEMAMETER" %in% names(raw)) {
          has_source <- has_source | !is.na(FEMAMETER)
        }
        ifelse(has_source, 1L, NA_integer_)
      }
    )
  
  elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
  message(sprintf("[LENS] Loaded %d indicators in %s seconds.",
                  nrow(raw), elapsed))
  
  raw
}

# --- Finalization (shared by the runtime loader and data_prep.R) ---

#' Apply the catalog-level transformations that turn the raw sheet tibble into
#' what the app expects: the "Sustainability (ESG)" mandate label, rebuilt
#' mandate-objective labels, an ordered mandate factor, and the drop list.
#' Idempotent, so it is safe to apply to data that was already finalized.
finalize_indicators <- function(raw) {
  raw %>%
    mutate(
      main_mandate       = sub("^Sustainability$", "Sustainability (ESG)", main_mandate),
      secondary_mandates = gsub("Sustainability(?! \\(ESG\\))", "Sustainability (ESG)",
                                secondary_mandates, perl = TRUE),
      main_mandate_objective = ifelse(
        !is.na(main_mandate) & !is.na(main_objectives),
        paste0(main_mandate, " (", main_objectives, ")"), NA_character_),
      secondary_mandate_objective = mapply(
        .build_secondary_label, secondary_mandates, secondary_objectives,
        USE.NAMES = FALSE),
      main_mandate = factor(
        main_mandate,
        levels = c("Financial inclusion", "Consumer protection",
                   "Stability, safety and soundness", "Sustainability (ESG)",
                   "Market development"),
        ordered = TRUE)
    ) %>%
    filter(!indicator_name %in% LENS_DROPPED_INDICATORS)
}

# --- Public API ---

#' Load indicators, using cache when available.
#'
#' @param force_refresh  Logical. If TRUE, bypass the cache.
#' @return A tibble matching the structure of the old indicators.RData.
load_indicators <- function(force_refresh = FALSE) {
  now <- Sys.time()
  
  # Return cache if still valid
  if (!force_refresh &&
      !is.null(.lens_cache$data) &&
      !is.null(.lens_cache$timestamp) &&
      difftime(now, .lens_cache$timestamp, units = "secs") < .lens_config$cache_ttl) {
    message("[LENS] Serving cached data (",
            round(difftime(now, .lens_cache$timestamp, units = "secs")),
            "s old).")
    return(.lens_cache$data)
  }
  
  # Authenticate if needed (only once per process)
  if (!googlesheets4::gs4_has_token()) {
    .lens_authenticate()
  }
  
  # Read fresh data
  indicators <- tryCatch(
    finalize_indicators(.lens_read_sheet()),
    error = function(e) {
      # If we have stale cache, use it rather than crashing
      if (!is.null(.lens_cache$data)) {
        warning("[LENS] Failed to refresh data: ", e$message,
                "\n  Serving stale cache from ",
                format(.lens_cache$timestamp, "%H:%M:%S"), ".",
                call. = FALSE)
        return(.lens_cache$data)
      }
      stop("[LENS] Failed to load data and no cache available: ",
           e$message, call. = FALSE)
    }
  )
  
  # Update cache
  .lens_cache$data      <- indicators
  .lens_cache$timestamp <- now
  
  indicators
}

#' Force-refresh the cache. Useful for a "Refresh" button in the UI.
refresh_indicators <- function() {
  load_indicators(force_refresh = TRUE)
}

#' Load the committed snapshot (data/indicators.RData), finalized and cached.
load_snapshot_indicators <- function() {
  if (is.null(.lens_cache$snapshot)) {
    env <- new.env(parent = emptyenv())
    load(.lens_config$snapshot, envir = env)
    snap <- env$indicators
    if ("preset_MSME" %in% names(snap)) {
      snap <- snap %>% rename(preset_msme = preset_MSME)
    }
    .lens_cache$snapshot <- finalize_indicators(snap)
  }
  .lens_cache$snapshot
}

#' Get the indicator catalog for the app. Never errors.
#'
#' Tries the Google Sheet (via the in-memory cache) and falls back to the
#' committed snapshot when the sheet is not configured, LENS_DATA_SOURCE is
#' "snapshot", or the API call fails with nothing cached. Records what was
#' served in .lens_cache$source so the UI can display it (see lens_data_status).
lens_get_indicators <- function() {
  use_sheet <- .lens_config$data_source != "snapshot" &&
    nzchar(.lens_config$sheet_id)

  if (use_sheet) {
    result <- tryCatch(
      withCallingHandlers(
        load_indicators(),
        warning = function(w) {
          message(conditionMessage(w))          # stale-cache notice, keep going
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        message("[LENS] Live data unavailable (", conditionMessage(e),
                "). Falling back to snapshot.")
        NULL
      }
    )
    if (!is.null(result)) {
      .lens_cache$source <- "sheet"
      return(result)
    }
  } else if (.lens_config$data_source == "snapshot") {
    message("[LENS] LENS_DATA_SOURCE=snapshot; using committed snapshot.")
  } else {
    message("[LENS] LENS_SHEET_ID not set; using committed snapshot.")
  }

  .lens_cache$source <- "snapshot"
  load_snapshot_indicators()
}

#' Describe what lens_get_indicators() last served: source and, for the sheet,
#' when it was last read. Used for a small status line in the UI.
lens_data_status <- function() {
  list(
    source    = if (is.null(.lens_cache$source)) "snapshot" else .lens_cache$source,
    timestamp = if (identical(.lens_cache$source, "sheet")) .lens_cache$timestamp else NULL
  )
}
